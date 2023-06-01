/**
 * @file
 *
 * @brief Source for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./quickdump.h"

#include <elektra/core/errors.h>
#include <elektra/type/types.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <internal/utility/endian.h>
#include <stdio.h>

#define MAGIC_NUMBER_BASE (0x454b444200000000UL) // EKDB (in ASCII) + Version placeholder

#define MAGIC_NUMBER_V1 ((kdb_unsigned_long_long_t) (MAGIC_NUMBER_BASE + 1))
#define MAGIC_NUMBER_V2 ((kdb_unsigned_long_long_t) (MAGIC_NUMBER_BASE + 2))
#define MAGIC_NUMBER_V3 ((kdb_unsigned_long_long_t) (MAGIC_NUMBER_BASE + 3))

struct metaLink
{
	const void * meta;
	size_t keyNameSize;
	const char * keyName;
};

struct list
{
	size_t alloc;
	size_t size;
	struct metaLink ** array;
};

struct stringbuffer
{
	size_t alloc;
	size_t offset;
	char * string;
};

static ssize_t findMetaLink (struct list * list, const Key * meta);
static void insertMetaLink (struct list * list, size_t index, const Key * meta, Key * key, size_t parentOffset);

static void setupBuffer (struct stringbuffer * buffer, size_t initialAlloc);
static void ensureBufferSize (struct stringbuffer * buffer, size_t minSize);

// keep #ifdef in sync with kdb export
#ifdef _WIN32
#define STDOUT_FILENAME ("CON")
#else
#define STDOUT_FILENAME ("/dev/stdout")
#endif

#include "./varint.c"

static inline void closeFile (FILE * file)
{
	if (file != stdout)
	{
		fclose (file);
	}
}

static inline bool writeData (FILE * file, const char * data, kdb_unsigned_long_long_t size, Key * errorKey)
{
	if (!varintWrite (file, size))
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, feof (file) ? "Premature end of file" : "Unknown error");
		return false;
	}

	if (size > 0)
	{
		if (fwrite (data, sizeof (char), size, file) < size)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, feof (file) ? "Premature end of file" : "Unknown error");
			return false;
		}
	}
	return true;
}

static inline bool readStringIntoBuffer (FILE * file, struct stringbuffer * buffer, Key * errorKey)
{
	kdb_unsigned_long_long_t size = 0;
	if (!varintRead (file, &size))
	{
		ELEKTRA_SET_RESOURCE_ERROR (errorKey, feof (file) ? "Premature end of file" : "Unknown error");
		return false;
	}

	size_t newSize = buffer->offset + size + 1;
	ensureBufferSize (buffer, newSize);

	if (fread (&buffer->string[buffer->offset], sizeof (char), size, file) < size)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, feof (file) ? "Premature end of file" : "Unknown error");
		return false;
	}
	buffer->string[newSize - 1] = '\0';
	return true;
}

int elektraQuickdumpGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/quickdump"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/quickdump", KEY_VALUE, "quickdump plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/quickdump/exports", KEY_END),
			keyNew ("system:/elektra/modules/quickdump/exports/get", KEY_FUNC, elektraQuickdumpGet, KEY_END),
			keyNew ("system:/elektra/modules/quickdump/exports/set", KEY_FUNC, elektraQuickdumpSet, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/quickdump/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	FILE * file = fopen (keyString (parentKey), "rb");

	if (file == NULL)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	kdb_unsigned_long_long_t magic;
	if (fread (&magic, sizeof (kdb_unsigned_long_long_t), 1, file) < 1)
	{
		if (feof (file) && ftell (file) == 0)
		{
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_SUCCESS;
		}
		else
		{
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	magic = be64toh (magic); // magic number is written big endian so EKDB magic string is readable

	switch (magic)
	{
	case MAGIC_NUMBER_V1:
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, "Quickdump v1 no longer supported");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	case MAGIC_NUMBER_V2:
		ELEKTRA_SET_RESOURCE_ERROR (parentKey, "Quickdump v2 no longer supported");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	case MAGIC_NUMBER_V3:
		// break, current version implemented below
		break;
	default:
		fclose (file);
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unknown magic number " ELEKTRA_UNSIGNED_LONG_LONG_F, magic);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// setup buffers
	struct stringbuffer valueBuffer;
	setupBuffer (&valueBuffer, 4);

	struct stringbuffer metaNameBuffer;
	setupBuffer (&metaNameBuffer, 4);

	// setup name buffer with parent key
	struct stringbuffer nameBuffer;

	size_t parentSize = keyGetNameSize (parentKey); // includes null terminator
	setupBuffer (&nameBuffer, parentSize + 4);

	keyGetName (parentKey, nameBuffer.string, parentSize);
	nameBuffer.string[parentSize - 1] = '/'; // replaces null terminator
	nameBuffer.string[parentSize] = '\0';	 // set new null terminator
	nameBuffer.offset = parentSize;		 // set offset to null terminator

	int fc;
	while ((fc = fgetc (file)) != EOF)
	{
		char c = fc;
		ungetc (c, file);

		if (!readStringIntoBuffer (file, &nameBuffer, parentKey))
		{
			elektraFree (nameBuffer.string);
			elektraFree (metaNameBuffer.string);
			elektraFree (valueBuffer.string);
			fclose (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		int ftype = fgetc (file);
		if (ftype == EOF)
		{
			elektraFree (nameBuffer.string);
			elektraFree (metaNameBuffer.string);
			elektraFree (valueBuffer.string);
			fclose (file);
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (parentKey, "Missing key type");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		char type = ftype;
		Key * k;

		switch (type)
		{
		case 'b': {
			// binary key value
			kdb_unsigned_long_long_t valueSize = 0;
			if (!varintRead (file, &valueSize))
			{
				ELEKTRA_SET_RESOURCE_ERROR (parentKey, feof (file) ? "Premature end of file" : "Unknown error");
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			if (valueSize == 0)
			{
				k = keyNew (nameBuffer.string, KEY_BINARY, KEY_SIZE, valueSize, KEY_END);
			}
			else
			{
				void * value = elektraMalloc (valueSize);
				if (fread (value, sizeof (char), valueSize, file) < valueSize)
				{
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					fclose (file);
					ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, "Error while reading file");
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				k = keyNew (nameBuffer.string, KEY_BINARY, KEY_SIZE, (size_t) valueSize, KEY_VALUE, value, KEY_END);
				elektraFree (value);
			}
			break;
		}
		case 's': {
			// string key value
			if (!readStringIntoBuffer (file, &valueBuffer, parentKey))
			{
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			k = keyNew (nameBuffer.string, KEY_VALUE, valueBuffer.string, KEY_END);
			break;
		}
		default:
			elektraFree (nameBuffer.string);
			elektraFree (metaNameBuffer.string);
			elektraFree (valueBuffer.string);
			fclose (file);
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Unknown key type %c", type);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		while ((fc = fgetc (file)) != 0)
		{
			if (fc == EOF)
			{
				keyDel (k);
				fclose (file);
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, "Missing key end");
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			c = fc;

			switch (c)
			{
			case 'm': {
				// meta key
				if (!readStringIntoBuffer (file, &metaNameBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!readStringIntoBuffer (file, &valueBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				const char * metaValue = valueBuffer.string;

				keySetMeta (k, metaNameBuffer.string, metaValue);
				break;
			}
			case 'c': {
				// copy meta
				if (!readStringIntoBuffer (file, &nameBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!readStringIntoBuffer (file, &metaNameBuffer, parentKey))
				{
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				const Key * sourceKey = ksLookupByName (returned, nameBuffer.string, 0);
				if (sourceKey == NULL)
				{
					ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not copy meta data from key '%s': Key not found",
								     nameBuffer.string);
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (keyCopyMeta (k, sourceKey, metaNameBuffer.string) != 1)
				{
					ELEKTRA_SET_INTERNAL_ERRORF (parentKey, "Could not copy meta data from key '%s': Error during copy",
								     &nameBuffer.string[nameBuffer.offset]);
					keyDel (k);
					elektraFree (nameBuffer.string);
					elektraFree (metaNameBuffer.string);
					elektraFree (valueBuffer.string);
					fclose (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
				break;
			}
			default:
				keyDel (k);
				elektraFree (nameBuffer.string);
				elektraFree (metaNameBuffer.string);
				elektraFree (valueBuffer.string);
				fclose (file);
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Unknown meta type %c", type);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		ksAppendKey (returned, k);
	}

	elektraFree (nameBuffer.string);
	elektraFree (metaNameBuffer.string);
	elektraFree (valueBuffer.string);

	fclose (file);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraQuickdumpSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	FILE * file;

	// cannot open stdout for writing, because its already open
	if (elektraStrCmp (keyString (parentKey), STDOUT_FILENAME) == 0)
	{
		file = stdout;
	}
	else
	{
		file = fopen (keyString (parentKey), "wb");
	}

	if (file == NULL)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	// magic number is written big endian so EKDB magic string is readable
	kdb_unsigned_long_long_t magic = htobe64 (MAGIC_NUMBER_V3);
	if (fwrite (&magic, sizeof (kdb_unsigned_long_long_t), 1, file) < 1)
	{
		closeFile (file);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	struct list metaKeys;
	metaKeys.alloc = 16;
	metaKeys.size = 0;
	metaKeys.array = elektraMalloc (metaKeys.alloc * sizeof (struct metaLink *));

	// we assume all keys in returned are below parentKey
	size_t parentOffset = keyGetNameSize (parentKey);

	// ... unless /noparent is in config, then we just take the full
	// (cascading) keynames as relative to the parentKey
	KeySet * config = elektraPluginGetConfig (handle);
	if (ksLookupByName (config, "/noparent", 0) != NULL)
	{
		parentOffset = 1;
	}

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		size_t fullNameSize = keyGetNameSize (cur);
		if (fullNameSize < parentOffset)
		{
			closeFile (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		kdb_unsigned_long_long_t nameSize = fullNameSize == parentOffset ? 0 : fullNameSize - 1 - parentOffset;
		if (!writeData (file, keyName (cur) + parentOffset, nameSize, parentKey))
		{
			closeFile (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (keyIsBinary (cur))
		{
			if (fputc ('b', file) == EOF)
			{
				closeFile (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			kdb_unsigned_long_long_t valueSize = keyGetValueSize (cur);

			char * value = NULL;
			if (valueSize > 0)
			{
				value = elektraMalloc (valueSize);
				if (keyGetBinary (cur, value, valueSize) == -1)
				{
					closeFile (file);
					elektraFree (value);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
			}

			if (!writeData (file, value, valueSize, parentKey))
			{
				closeFile (file);
				elektraFree (value);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
			elektraFree (value);
		}
		else
		{
			if (fputc ('s', file) == EOF)
			{
				closeFile (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}

			kdb_unsigned_long_long_t valueSize = keyGetValueSize (cur) - 1;
			if (!writeData (file, keyString (cur), valueSize, parentKey))
			{
				closeFile (file);
				return ELEKTRA_PLUGIN_STATUS_ERROR;
			}
		}

		KeySet * metaKS = keyMeta (cur);

		for (elektraCursor itMeta = 0; itMeta < ksGetSize (metaKS); ++itMeta)
		{
			const Key * meta = ksAtCursor (metaKS, itMeta);
			ssize_t result = findMetaLink (&metaKeys, meta);
			if (result < 0)
			{
				if (fputc ('m', file) == EOF)
				{
					closeFile (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				// ignore meta namespace when writing to file
				kdb_unsigned_long_long_t metaNameSize = keyGetNameSize (meta) - 1 - (sizeof ("meta:/") - 1);
				if (!writeData (file, keyName (meta) + sizeof ("meta:/") - 1, metaNameSize, parentKey))
				{
					closeFile (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				kdb_unsigned_long_long_t metaValueSize = keyGetValueSize (meta) - 1;
				if (!writeData (file, keyString (meta), metaValueSize, parentKey))
				{
					closeFile (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				insertMetaLink (&metaKeys, -result - 1, meta, cur, parentOffset);
			}
			else
			{
				if (fputc ('c', file) == EOF)
				{
					closeFile (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				kdb_unsigned_long_long_t keyNameSize = metaKeys.array[result]->keyNameSize;
				if (!writeData (file, metaKeys.array[result]->keyName, keyNameSize, parentKey))
				{
					closeFile (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				// ignore meta namespace when writing to file
				kdb_unsigned_long_long_t metaNameSize = keyGetNameSize (meta) - 1 - (sizeof ("meta:/") - 1);
				if (!writeData (file, keyName (meta) + sizeof ("meta:/") - 1, metaNameSize, parentKey))
				{
					closeFile (file);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}
			}
		}

		if (fputc (0, file) == EOF)
		{
			closeFile (file);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	for (size_t i = 0; i < metaKeys.size; ++i)
	{
		elektraFree (metaKeys.array[i]);
	}
	elektraFree (metaKeys.array);

	closeFile (file);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

ssize_t findMetaLink (struct list * list, const Key * meta)
{
	const void * search = meta;

	if (list->size == 0)
	{
		return -1;
	}

	if (search > list->array[list->size - 1]->meta)
	{
		return -(ssize_t) list->size - 1;
	}

	ssize_t left = 0;
	ssize_t right = list->size;
	--right;

	ssize_t insertpos = 0;

	while (left <= right)
	{
		size_t middle = left + ((right - left) / 2);

		if (list->array[middle]->meta < search)
		{
			insertpos = left = middle + 1;
		}
		else if (list->array[middle]->meta == search)
		{
			return middle;
		}
		else
		{
			insertpos = middle;
			right = middle - 1;
		}
	}

	return -insertpos - 1;
}

void insertMetaLink (struct list * list, size_t index, const Key * meta, Key * key, size_t parentOffset)
{
	if (list->size + 1 >= list->alloc)
	{
		list->alloc *= 2;
		elektraRealloc ((void **) &list->array, sizeof (struct metaLink *) * list->alloc);
	}

	struct metaLink * link = elektraMalloc (sizeof (struct metaLink));
	link->meta = meta;
	size_t fullNameSize = keyGetNameSize (key);
	link->keyNameSize = fullNameSize <= parentOffset ? 0 : fullNameSize - 1 - parentOffset;
	link->keyName = keyName (key) + parentOffset;

	if (index < list->size)
	{
		memmove (&list->array[index + 1], &list->array[index], (list->size - index) * sizeof (struct metaLink *));
	}

	list->array[index] = link;
	++list->size;
}

void setupBuffer (struct stringbuffer * buffer, size_t initialAlloc)
{
	buffer->offset = 0;
	buffer->alloc = initialAlloc;
	buffer->string = elektraMalloc (initialAlloc * sizeof (char));
}

void ensureBufferSize (struct stringbuffer * buffer, size_t minSize)
{
	size_t alloc = buffer->alloc;
	while (alloc < minSize)
	{
		alloc *= 2;
	}

	if (alloc != buffer->alloc)
	{
		elektraRealloc ((void **) &buffer->string, alloc);
		buffer->alloc = alloc;
	}
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("quickdump",
				    ELEKTRA_PLUGIN_GET,	&elektraQuickdumpGet,
				    ELEKTRA_PLUGIN_SET,	&elektraQuickdumpSet,
				    ELEKTRA_PLUGIN_END);
	// clang-format on
}
