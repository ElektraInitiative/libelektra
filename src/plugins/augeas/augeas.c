/**
 * @file
 *
 * @brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

/* used for asprintf */
#define _GNU_SOURCE
#include <stdio.h>

#include <ctype.h>
#include <elektra/core/errors.h>
#include <elektra/ease/utils.h>
#include <glob.h>
#include <internal/config.h>
#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/alloc.h>
#include <internal/utility/format.h>
#include <libgen.h>

#include "./aug.h"

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

	result = (int) keySetMeta (key, "order", buffer);
	elektraFree (buffer);
	return result;
}

static int keyCmpOrderWrapper (const void * a, const void * b)
{
	return elektraKeyCmpOrder (*((const Key **) a), *((const Key **) b));
}

static const char * getLensPath (Plugin * handle)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * lensPathKey = ksLookupByName (config, "/lens", 0);
	return keyString (lensPathKey);
}

int elektraAugeasGenConf (KeySet * ks, Key * errorKey ELEKTRA_UNUSED)
{
	glob_t pglob;
	int retval = 1;
	const char * f = LIBAUGEAS_PREFIX "/share/augeas/lenses/dist/*.aug";
	if (glob (f, GLOB_NOSORT, NULL, &pglob) == 0)
	{
		ELEKTRA_LOG ("has glob %zd", pglob.gl_pathc);
		for (size_t i = 0; i < pglob.gl_pathc; ++i)
		{
			char * p = elektraStrDup (basename (pglob.gl_pathv[i]));
			size_t l = strlen (p);
			if (l > 4)
			{
				p[l - 4] = '\0';
				Key * k = keyNew ("system:/", KEY_END);
				keyAddBaseName (k, p);
				ksAppendKey (ks, keyDup (k, KEY_CP_ALL));

				Key * b = keyDup (k, KEY_CP_ALL);
				keyAddBaseName (b, "infos");
				ksAppendKey (ks, keyDup (b, KEY_CP_ALL));
				keyAddBaseName (b, "provides");
				char * s = elektraFormat ("storage/%s", p);
				keySetString (b, s);
				elektraFree (s);
				ksAppendKey (ks, b);

				keyAddBaseName (k, "config");
				ksAppendKey (ks, keyDup (k, KEY_CP_ALL));
				keyAddBaseName (k, "lens");
				p[0] = (char) toupper (p[0]);
				p[l - 1] = 's';
				p[l - 2] = 'n';
				p[l - 3] = 'l';
				p[l - 4] = '.';
				keySetString (k, p);
				ksAppendKey (ks, k);
			}
			elektraFree (p);
		}
		globfree (&pglob);
	}
	else
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Could not glob %s because of augeas", f);
		retval = -1;
	}
	return retval;
}

static const char * getAugeasError (augeas * augeasHandle, const char * lensPath)
{
	const char * reason = 0;
	if (aug_error (augeasHandle) != 0)
	{
		const char * format = "%s\n\tlensPath: %s";
		reason = aug_error_message (augeasHandle);
		size_t messageSize = strlen (reason) + strlen (lensPath) + strlen (format);
		char * buffer = elektraMalloc (messageSize);
		snprintf (buffer, messageSize, format, reason, lensPath);
		reason = buffer;
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
			size_t messageSize = (augeasError ? strlen (augeasError) : 0) + (line ? strlen (line) : 0) +
					     (character ? strlen (character) : 0) + (message ? strlen (message) : 0) +
					     (lens ? strlen (lens) : 0) + strlen (format);
			char * buffer = elektraMalloc (messageSize);
			snprintf (buffer, messageSize, format, augeasError ? augeasError : "", line ? line : "", character ? character : "",
				  message ? message : "", lens ? lens : "");
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
	Key * key = keyDup (parentKey, KEY_CP_ALL);
	char * baseName = elektraStrDup (treePath + strlen (AUGEAS_TREE_ROOT) + 1);
	char * lastSlash = strrchr (baseName, '/');
	const char * lastPart = lastSlash != NULL ? lastSlash + 1 : baseName;

	if (strcmp (lastPart, "#comment") == 0)
	{
		if (lastSlash != NULL)
		{
			*lastSlash = '\0';
			if (keyAddName (key, baseName) < 0)
			{
				keyDel (key);
				elektraFree (baseName);
				return NULL;
			}
		}

		if (keyAddBaseName (key, lastPart) < 0)
		{
			keyDel (key);
			elektraFree (baseName);
			return NULL;
		}
	}
	else
	{
		if (keyAddName (key, baseName) < 0)
		{
			keyDel (key);
			elektraFree (baseName);
			return NULL;
		}
	}
	elektraFree (baseName);
	return key;
}

/**
 * Creates a new Elektra key from the specified Augeas key.
 * If any step during key conversion fails, an error is returned
 * in order to prevent inconsistent keys.
 */
static int convertToKey (augeas * handle, const char * treePath, void * data)
{
	struct KeyConversion * conversionData = (struct KeyConversion *) data;
	int result = 0;
	const char * value = 0;
	result = aug_get (handle, treePath, &value);

	/* we were unable to retrieve the augeas value */
	if (result < 0) return result;

	Key * key = createKeyFromPath (conversionData->parentKey, treePath);

	if (key == NULL) return -1;

	/* fill key values */
	keySetString (key, value);
	conversionData->currentOrder++;
	result = keySetOrderMeta (key, conversionData->currentOrder);

	/* setting the correct key order failed */
	if (result < 0) return result;

	result = (int) ksAppendKey (conversionData->ks, key);

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
	struct OrphanSearch * orphanData = (struct OrphanSearch *) data;

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
	char ** matches = (char **) 1;
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
		content = elektraMalloc ((size_t) fileSize * sizeof (char) + 1);
		if (content == 0) return 0;
		size_t readBytes = fread (content, sizeof (char), (size_t) fileSize, fh);

		if (feof (fh) || ferror (fh) || readBytes != (size_t) fileSize) return 0;

		/* null terminate the string, as fread doesn't do it */
		content[fileSize] = 0;
	}
	else if (fileSize == 0)
	{
		content = elektraMalloc (1);
		if (content == 0) return 0;
		*content = (char) 0;
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
		ret = (int) fwrite (value, sizeof (char), strlen (value), fh);

		if (feof (fh) || ferror (fh)) return -1;
	}

	return ret;
}

static int saveTree (augeas * augeasHandle, KeySet * ks, const char * lensPath, Key * parentKey)
{
	int ret = 0;

	size_t prefixSize = (size_t) (keyGetNameSize (parentKey) - 1);
	size_t arraySize = (size_t) ksGetSize (ks);
	Key ** keyArray = calloc ((size_t) ksGetSize (ks), sizeof (Key *));
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

		if (strcmp (keyBaseName (key), "#comment") == 0)
		{
			size_t offset = strlen (nodeName) - sizeof ("#comment") + 1;
			strcpy (nodeName + offset, "#comment");
		}

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
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (parentKey, getAugeasError (augeasHandle, lensPath));
	}

	return ret;

memoryerror:
	elektraFree (keyArray);
	ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
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
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			return -1;
		}

		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, errormessage);
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

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/augeas"))
	{
		KeySet * info =
#include "./contract.h"

			ksAppend (returned, info);
		ksDel (info);
		return 1;
	}

	augeas * augeasHandle = elektraPluginGetData (handle);

	/* retrieve the lens to use */
	const char * lensPath = getLensPath (handle);
	if (!lensPath)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "No Augeas lens was configured: %s", keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	FILE * fh = fopen (keyString (parentKey), "r");

	if (fh == 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	/* load its contents into a string */
	char * content = loadFile (fh);

	if (content == 0)
	{
		fclose (fh);
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Error while reading file. Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	/* convert the string into an augeas tree */
	ret = loadTree (augeasHandle, lensPath, content);
	elektraFree (content);

	if (ret < 0)
	{
		fclose (fh);
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, getAugeasError (augeasHandle, lensPath));
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	/* convert the augeas tree to an Elektra KeySet */
	ksClear (returned);
	KeySet * append = ksNew ((size_t) ksGetSize (returned) * 2, KS_END);

	Key * key = keyDup (parentKey, KEY_CP_ALL);
	ksAppendKey (append, key);

	struct KeyConversion * conversionData = elektraMalloc (sizeof (struct KeyConversion));

	if (!conversionData)
	{
		fclose (fh);
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	conversionData->currentOrder = 0;
	conversionData->parentKey = keyDup (key, KEY_CP_ALL);
	conversionData->ks = append;

	ret = foreachAugeasNode (augeasHandle, AUGEAS_TREE_ROOT, &convertToKey, conversionData);

	keyDel (conversionData->parentKey);
	elektraFree (conversionData);

	if (ret < 0)
	{
		fclose (fh);
		ksDel (append);
		ELEKTRA_SET_INSTALLATION_ERROR (parentKey, getAugeasError (augeasHandle, lensPath));
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
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
		ELEKTRA_SET_INSTALLATION_ERRORF (parentKey, "No Augeas lens was configured: %s", keyName (parentKey));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	FILE * fh = fopen (keyValue (parentKey), "w+");

	if (fh == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	int ret = 0;

	if (aug_match (augeasHandle, AUGEAS_TREE_ROOT, NULL) == 0)
	{
		/* load a fresh copy of the file into the tree */
		char * content = loadFile (fh);

		if (content == 0)
		{
			fclose (fh);
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Error while reading file. Reason: %s", strerror (errno));
		}

		/* convert the string into an augeas tree */
		ret = loadTree (augeasHandle, lensPath, content);
		elektraFree (content);

		if (ret < 0)
		{
			fclose (fh);
			ELEKTRA_SET_INSTALLATION_ERROR (parentKey, getAugeasError (augeasHandle, lensPath));
			errno = errnosave;
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ret = saveTree (augeasHandle, returned, lensPath, parentKey);

	if (ret < 0)
	{
		fclose (fh);
		errno = errnosave;
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	/* write the Augeas tree to the file */
	ret = saveFile (augeasHandle, fh);
	fclose (fh);

	if (ret < 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open file for writing. Reason: %s", strerror (errno));
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	errno = errnosave;
	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("augeas",
			ELEKTRA_PLUGIN_GET, &elektraAugeasGet,
			ELEKTRA_PLUGIN_SET, &elektraAugeasSet,
			ELEKTRA_PLUGIN_OPEN, &elektraAugeasOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraAugeasClose,
			ELEKTRA_PLUGIN_END);
}

