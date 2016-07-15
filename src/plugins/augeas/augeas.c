/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

/* used for asprintf */
#define _GNU_SOURCE

#include "aug.h"

#define ELEKTRA_SET_GENERAL_ERROR(id, parentKey, message)                                                                                  \
	do                                                                                                                                 \
	{                                                                                                                                  \
		ELEKTRA_SET_ERROR (id, parentKey, message);                                                                                \
		errno = errnosave;                                                                                                         \
		return -1;                                                                                                                 \
	} while (0)

#define ELEKTRA_SET_ERRNO_ERROR(id, parentKey) ELEKTRA_SET_GENERAL_ERROR (id, parentKey, strerror (errno))

#define ELEKTRA_SET_AUGEAS_ERROR(handle, parrentKey) ELEKTRA_SET_GENERAL_ERROR (85, parentKey, getAugeasError (augeasHandle))

struct KeyConversion
{
	KeySet * ks;
	Key * parentKey;
	int currentOrder;
};

struct OrphanSearch
{
	KeySet * ks;
	Key * parentKey;
};

typedef int (*ForeachAugNodeClb) (augeas *, const char *, void *);

int keySetOrderMeta (Key * key, int order)
{
	char * buffer;
	int result;
	result = asprintf (&buffer, "%d", order);

	if (result < 0) return result;

	result = keySetMeta (key, "order", buffer);
	elektraFree (buffer);
	return result;
}

static int keyCmpOrderWrapper (const void * a, const void * b)
{
	return elektraKeyCmpOrder (*((const Key **)a), *((const Key **)b));
}

static const char * getLensPath (Plugin * handle)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * lensPathKey = ksLookupByName (config, "/lens", 0);
	return keyString (lensPathKey);
}

static const char * getAugeasError (augeas * augeasHandle)
{
	const char * reason = 0;
	if (aug_error (augeasHandle) != 0)
	{
		reason = aug_error_message (augeasHandle);
	}
	else
	{
		const char * augeasError;
		aug_get (augeasHandle, "/augeas/text" AUGEAS_TREE_ROOT "/error", &augeasError);

		if (augeasError)
		{
			const char * lens;
			const char * line;
			const char * character;
			const char * message;

			aug_get (augeasHandle, "/augeas/text" AUGEAS_TREE_ROOT "/error/lens", &lens);
			aug_get (augeasHandle, "/augeas/text" AUGEAS_TREE_ROOT "/error/line", &line);
			aug_get (augeasHandle, "/augeas/text" AUGEAS_TREE_ROOT "/error/char", &character);
			aug_get (augeasHandle, "/augeas/text" AUGEAS_TREE_ROOT "/error/message", &message);

			const char * format = "%s\n\tposition: %s:%s\n\tmessage: %s\n\tlens: %s";
			size_t messageSize = strlen (lens) + strlen (line) + strlen (character) + strlen (message) + strlen (format);
			char * buffer = elektraMalloc (messageSize);
			sprintf (buffer, format, augeasError, line, character, message);
			reason = buffer;
		}
		else
		{
			reason = "No specific reason was reported";
		}
	}

	/* should not happen, but avoid 0 return */
	if (!reason)
	{
		reason = "";
	}
	return reason;
}

static Key * createKeyFromPath (Key * parentKey, const char * treePath)
{
	Key * key = keyDup (parentKey);
	const char * baseName = (treePath + strlen (AUGEAS_TREE_ROOT) + 1);

	size_t baseSize = keyGetNameSize (key);
	size_t keyNameSize = strlen (baseName) + baseSize + 1;
	char * newName = elektraMalloc (keyNameSize);

	if (!newName) return 0;

	strcpy (newName, keyName (key));
	newName[baseSize - 1] = KDB_PATH_SEPARATOR;
	newName[baseSize] = 0;
	strcat (newName, baseName);

	keySetName (key, newName);
	elektraFree (newName);

	return key;
}

/**
 * Creates a new Elektra key from the specified Augeas key.
 * If any step during key conversion fails, an error is returned
 * in order to prevent inconsistent keys.
 */
static int convertToKey (augeas * handle, const char * treePath, void * data)
{
	struct KeyConversion * conversionData = (struct KeyConversion *)data;
	int result = 0;
	const char * value = 0;
	result = aug_get (handle, treePath, &value);

	/* we were unable to retrieve the augeas value */
	if (result < 0) return result;

	Key * key = createKeyFromPath (conversionData->parentKey, treePath);

	/* fill key values */
	keySetString (key, value);
	conversionData->currentOrder++;
	result = keySetOrderMeta (key, conversionData->currentOrder);

	/* setting the correct key order failed */
	if (result < 0) return result;

	result = ksAppendKey (conversionData->ks, key);

	return result;
}

/**
 * Checks whether a given path must be pruned from the Augeas tree by comparing
 * it with the supplied keyset. If any operation during pruning fails, an error
 * is returned in order to prevent invalid keys.
 */
static int removeOrphan (augeas * handle, const char * treePath, void * data)
{
	int result;
	struct OrphanSearch * orphanData = (struct OrphanSearch *)data;

	Key * key = createKeyFromPath (orphanData->parentKey, treePath);

	if (!ksLookup (orphanData->ks, key, KDB_O_NONE))
	{
		char * nodeMatch;
		char ** matches;
		result = asprintf (&nodeMatch, "%s/*", treePath);

		if (result < 0) return -1;

		int numChildNodes = aug_match (handle, nodeMatch, &matches);
		elektraFree (nodeMatch);

		/* if the node is a leaf node we can safely delete it */
		if (numChildNodes == 0)
		{
			aug_rm (handle, treePath);
		}
		else
		{
			short pruneTree = 1;
			for (int i = 0; i < numChildNodes; i++)
			{
				Key * childKey = createKeyFromPath (orphanData->parentKey, matches[i]);
				if (ksLookup (orphanData->ks, childKey, KDB_O_NONE))
				{
					pruneTree = 0;
				}
				keyDel (childKey);
				elektraFree (matches[i]);
			}
			elektraFree (matches);

			if (pruneTree)
			{
				aug_rm (handle, treePath);
			}
		}
	}

	keyDel (key);

	return 0;
}

static int foreachAugeasNode (augeas * handle, const char * treePath, ForeachAugNodeClb callback, void * callbackData)
{
	char * matchPath;
	int result = 0;
	result = asprintf (&matchPath, "%s//*", treePath);

	if (result < 0) return -1;

	/* must be non NULL for aug_match to return matches */
	char ** matches = (char **)1;
	int numMatches = aug_match (handle, matchPath, &matches);
	elektraFree (matchPath);

	if (numMatches < 0) return numMatches;

	int i;
	for (i = 0; i < numMatches; i++)
	{
		/* retrieve the value from augeas */
		char * curPath = matches[i];

		result = (*callback) (handle, curPath, callbackData);

		/* if handling the key failed, abort with an error as
		 * the failed key could be a crucial one
		 */
		if (result < 0) break;

		elektraFree (curPath);
	}

	for (; i < numMatches; i++)
	{
		elektraFree (matches[i]);
	}

	elektraFree (matches);

	return result;
}

static char * loadFile (FILE * fh)
{
	/* open the file */
	char * content = 0;

	if (fseek (fh, 0, SEEK_END) != 0) return 0;

	long fileSize = ftell (fh);
	rewind (fh);

	if (fileSize > 0)
	{
		content = elektraMalloc (fileSize * sizeof (char) + 1);
		if (content == 0) return 0;
		int readBytes = fread (content, sizeof (char), fileSize, fh);

		if (feof (fh) || ferror (fh) || readBytes != fileSize) return 0;

		/* null terminate the string, as fread doesn't do it */
		content[fileSize] = 0;
	}
	else if (fileSize == 0)
	{
		content = elektraMalloc (1);
		if (content == 0) return 0;
		*content = (char)0;
	}

	return content;
}

static int loadTree (augeas * augeasHandle, const char * lensPath, char * content)
{
	aug_set (augeasHandle, AUGEAS_CONTENT_ROOT, content);

	return aug_text_store (augeasHandle, lensPath, AUGEAS_CONTENT_ROOT, AUGEAS_TREE_ROOT);
}

static int saveFile (augeas * augeasHandle, FILE * fh)
{
	/* retrieve the file content */
	int ret = 0;
	const char * value = 0;
	aug_get (augeasHandle, AUGEAS_OUTPUT_ROOT, &value);

	/* write the file */
	if (value)
	{
		ret = fwrite (value, sizeof (char), strlen (value), fh);

		if (feof (fh) || ferror (fh)) return -1;
	}

	return ret;
}

static int saveTree (augeas * augeasHandle, KeySet * ks, const char * lensPath, Key * parentKey)
{
	int ret = 0;

	size_t prefixSize = keyGetNameSize (parentKey) - 1;
	size_t arraySize = ksGetSize (ks);
	Key ** keyArray = calloc (ksGetSize (ks), sizeof (Key *));
	ret = elektraKsToMemArray (ks, keyArray);

	if (ret < 0) goto memoryerror;

	qsort (keyArray, arraySize, sizeof (Key *), keyCmpOrderWrapper);

	/* convert the Elektra KeySet to an Augeas tree */
	for (size_t i = 0; i < arraySize; i++)
	{
		Key * key = keyArray[i];
		char * nodeName;
		ret = asprintf (&nodeName, AUGEAS_TREE_ROOT "%s", (keyName (key) + prefixSize));

		if (ret < 0) goto memoryerror;

		aug_set (augeasHandle, nodeName, keyString (key));
		elektraFree (nodeName);
	}

	elektraFree (keyArray);

	/* remove keys not present in the KeySet */
	struct OrphanSearch * data = elektraMalloc (sizeof (struct OrphanSearch));

	if (!data) return -1;

	data->ks = ks;
	data->parentKey = parentKey;

	ret = foreachAugeasNode (augeasHandle, AUGEAS_TREE_ROOT, &removeOrphan, data);

	elektraFree (data);

	/* build the tree */
	ret = aug_text_retrieve (augeasHandle, lensPath, AUGEAS_CONTENT_ROOT, AUGEAS_TREE_ROOT, AUGEAS_OUTPUT_ROOT);

	if (ret < 0)
	{
		/* report the augeas specific error */
		ELEKTRA_SET_ERROR (85, parentKey, getAugeasError (augeasHandle));
	}

	return ret;

memoryerror:
	elektraFree (keyArray);
	ELEKTRA_SET_ERROR (87, parentKey, "Unable to allocate memory while saving the augeas tree");
	return -1;
}

int elektraAugeasOpen (Plugin * handle, Key * parentKey)
{
	augeas * augeasHandle;
	augeasHandle = aug_init (NULL, NULL, AUG_NO_MODL_AUTOLOAD | AUG_NO_ERR_CLOSE);
	int ret;

	if (aug_error (augeasHandle) != AUG_NOERROR)
	{
		char * errormessage;
		ret = asprintf (&errormessage, "Unable to initialize augeas: %s", aug_error_message (augeasHandle));

		if (ret >= 0)
		{
			ELEKTRA_SET_ERROR (87, parentKey, "Unable to allocate memory for a detailed augeas error message");
			return -1;
		}

		ELEKTRA_SET_ERROR (85, parentKey, errormessage);
		elektraFree (errormessage);
		return -1;
	}

	elektraPluginSetData (handle, augeasHandle);
	return 0;
}

int elektraAugeasClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	augeas * augeasHandle = elektraPluginGetData (handle);

	if (augeasHandle)
	{
		aug_close (augeasHandle);
		elektraPluginSetData (handle, 0);
	}

	return 0;
}

int elektraAugeasGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	int errnosave = errno;
	int ret = 0;

	if (!strcmp (keyName (parentKey), "system/elektra/modules/augeas"))
	{
		KeySet * info =
#include "contract.h"

			ksAppend (returned, info);
		ksDel (info);
		return 1;
	}

	augeas * augeasHandle = elektraPluginGetData (handle);

	/* retrieve the lens to use */
	const char * lensPath = getLensPath (handle);
	if (!lensPath) ELEKTRA_SET_GENERAL_ERROR (86, parentKey, keyName (parentKey));

	FILE * fh = fopen (keyString (parentKey), "r");

	if (fh == 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	/* load its contents into a string */
	char * content = loadFile (fh);

	if (content == 0)
	{
		fclose (fh);
		ELEKTRA_SET_ERRNO_ERROR (76, parentKey);
	}

	/* convert the string into an augeas tree */
	ret = loadTree (augeasHandle, lensPath, content);
	elektraFree (content);

	if (ret < 0)
	{
		fclose (fh);
		ELEKTRA_SET_AUGEAS_ERROR (augeasHandle, parentKey);
	}

	/* convert the augeas tree to an Elektra KeySet */
	ksClear (returned);
	KeySet * append = ksNew (ksGetSize (returned) * 2, KS_END);

	Key * key = keyDup (parentKey);
	ksAppendKey (append, key);

	struct KeyConversion * conversionData = elektraMalloc (sizeof (struct KeyConversion));

	if (!conversionData)
	{
		fclose (fh);
		ELEKTRA_SET_GENERAL_ERROR (87, parentKey, strerror (errno));
	}

	conversionData->currentOrder = 0;
	conversionData->parentKey = key;
	conversionData->ks = append;

	ret = foreachAugeasNode (augeasHandle, AUGEAS_TREE_ROOT, &convertToKey, conversionData);

	elektraFree (conversionData);

	if (ret < 0)
	{
		fclose (fh);
		ksDel (append);
		ELEKTRA_SET_AUGEAS_ERROR (augeasHandle, parentKey);
	}

	fclose (fh);

	ksAppend (returned, append);
	ksDel (append);
	errno = errnosave;
	return 1;
}

int elektraAugeasSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	int errnosave = errno;
	augeas * augeasHandle = elektraPluginGetData (handle);

	const char * lensPath = getLensPath (handle);

	if (!lensPath)
	{
		ELEKTRA_SET_GENERAL_ERROR (86, parentKey, keyName (parentKey));
	}

	FILE * fh = fopen (keyValue (parentKey), "w+");

	if (fh == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	int ret = 0;

	if (aug_match (augeasHandle, AUGEAS_TREE_ROOT, NULL) == 0)
	{
		/* load a fresh copy of the file into the tree */
		char * content = loadFile (fh);

		if (content == 0)
		{
			fclose (fh);
			ELEKTRA_SET_ERRNO_ERROR (76, parentKey);
		}

		/* convert the string into an augeas tree */
		ret = loadTree (augeasHandle, lensPath, content);
		elektraFree (content);

		if (ret < 0)
		{
			fclose (fh);
			ELEKTRA_SET_AUGEAS_ERROR (augeasHandle, parentKey);
		}
	}

	ret = saveTree (augeasHandle, returned, lensPath, parentKey);

	if (ret < 0)
	{
		fclose (fh);
		errno = errnosave;
		return -1;
	}

	/* write the Augeas tree to the file */
	ret = saveFile (augeasHandle, fh);
	fclose (fh);

	if (ret < 0) ELEKTRA_SET_ERRNO_ERROR (75, parentKey);

	errno = errnosave;
	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (augeas)
{
	// clang-format off
	return elektraPluginExport ("augeas",
			ELEKTRA_PLUGIN_GET, &elektraAugeasGet,
			ELEKTRA_PLUGIN_SET, &elektraAugeasSet,
			ELEKTRA_PLUGIN_OPEN, &elektraAugeasOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraAugeasClose,
			ELEKTRA_PLUGIN_END);
}

