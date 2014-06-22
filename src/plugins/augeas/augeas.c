/**
 * \file
 *
 * \brief A plugin that makes use of libaugeas to read and write configuration files
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

/* used for asprintf */
#define _GNU_SOURCE

#include "augeas.h"

#define ELEKTRA_SET_GENERAL_ERROR(id, parentKey, message) \
	do { \
		ELEKTRA_SET_ERROR(id, parentKey, message); \
		errno = errnosave; \
		return -1; \
	} while (0)

#define ELEKTRA_SET_ERRNO_ERROR(id, parentKey) \
	ELEKTRA_SET_GENERAL_ERROR(id, parentKey, strerror(errno))

#define ELEKTRA_SET_AUGEAS_ERROR(handle, parrentKey) \
	ELEKTRA_SET_GENERAL_ERROR(85, parentKey, getAugeasError(augeasHandle))

struct KeyConversion
{
	KeySet *ks;
	Key *parentKey;
	int currentOrder;
};

struct OrphanSearch
{
	KeySet *ks;
	Key *parentKey;
};

typedef int (*ForeachCallback)(augeas *, const char *, void *);

void keySetOrderMeta(Key *key, int order)
{
	char *buffer;
	asprintf (&buffer, "%d", order);
	keySetMeta (key, "order", buffer);
	free (buffer);
}

int keyCmpOrder(const void *a, const void *b)
{
	const Key **ka = (const Key **) a;
	const Key **kb = (const Key **) b;

	int aorder = 0;
	int border = 0;

	const Key *kam = keyGetMeta (*ka, "order");
	const Key *kbm = keyGetMeta (*kb, "order");

	if (kam) aorder = atoi (keyString (kam));
	if (kbm) border = atoi (keyString (kbm));

	return aorder - border;
}

static const char *getLensPath(Plugin *handle)
{
	KeySet *config = elektraPluginGetConfig (handle);
	Key* lensPathKey = ksLookupByName (config, "/lens", 0);
	return keyString (lensPathKey);
}

static const char *getAugeasError(augeas* augeasHandle)
{
	const char* message = 0;
	if (aug_error (augeasHandle) != 0)
	{
		message = aug_error_message (augeasHandle);
	}
	else
	{
		aug_get (augeasHandle, "/augeas/text"AUGEAS_TREE_ROOT"/error/message",
				&message);
		if (!message) message = "No specific reason was reported";
	}

	/* should not happen, but avoid 0 return */
	if (!message) message = "";

	return message;
}

static Key *createKeyFromPath(Key *parentKey, const char *treePath)
{
	Key *key = keyDup (parentKey);
	const char *baseName = (treePath + strlen (AUGEAS_TREE_ROOT) + 1);
	keyAddBaseName (key, baseName);
	return key;
}

static int convertToKey(augeas *handle, const char *treePath, void *data)
{
	struct KeyConversion *conversionData = (struct KeyConversion *) data;
	int result = 0;
	const char *value = 0;
	result = aug_get (handle, treePath, &value);

	if (result < 0) return result;

	Key *key = createKeyFromPath (conversionData->parentKey, treePath);

	/* fill key values */
	keySetString (key, value);
	conversionData->currentOrder++;
	keySetOrderMeta (key, conversionData->currentOrder);
	result = ksAppendKey (conversionData->ks, key);

	return result;
}

static int removeOrphan(augeas *handle, const char *treePath, void *data)
{
	struct OrphanSearch *orphanData = (struct OrphanSearch *) data;

	Key *key = createKeyFromPath (orphanData->parentKey, treePath);

	if (!ksLookup (orphanData->ks, key, KDB_O_NONE))
	{
		char *nodeMatch;
		char **matches;
		asprintf (&nodeMatch, "%s/*", treePath);
		int numChildNodes = aug_match (handle, nodeMatch, &matches);
		free (nodeMatch);

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
				Key *childKey = createKeyFromPath (orphanData->parentKey,
						matches[i]);
				if (ksLookup (orphanData->ks, childKey, KDB_O_NONE))
				{
					pruneTree = 0;
				}
				keyDel (childKey);
				free (matches[i]);
			}
			free (matches);

			if (pruneTree)
			{
				aug_rm (handle, treePath);
			}
		}
	}

	keyDel (key);

	return 0;
}

static int foreachAugeasNode(augeas *handle, const char *treePath,
		ForeachCallback callback, void *callbackData)
{
	char *matchPath;
	asprintf (&matchPath, "%s/*", treePath);

	/* must be non NULL for aug_match to return matches */
	char **matches = (char **) 1;
	int numMatches = aug_match (handle, matchPath, &matches);
	free (matchPath);

	if (numMatches < 0) return numMatches;

	int i;
	int result = 0;
	for (i = 0; i < numMatches; i++)
	{
		/* retrieve the value from augeas */
		char *curPath = matches[i];

		result = (*callback) (handle, curPath, callbackData);

		/* handle the subtree */
		result = foreachAugeasNode (handle, curPath, callback, callbackData);

		if (result < 0) break;

		free (curPath);
	}

	for (; i < numMatches; i++)
	{
		free (matches[i]);
	}

	free (matches);

	return result;
}

static Key **convertToArray(KeySet *ks)
{
	/* build an array of keys ordered by the order MetaKey */
	Key **result;
	size_t arraySize = ksGetSize (ks);
	result = calloc (arraySize, sizeof(Key *));

	if (result == 0) return 0;

	ksRewind (ks);
	size_t index = 0;

	Key *key;
	while ((key = ksNext (ks)) != 0)
	{
		result[index] = key;
		++index;
	}

	qsort (result, arraySize, sizeof(Key *), keyCmpOrder);

	return result;
}

static char *loadFile(FILE *fh)
{
	/* open the file */
	char* content = 0;

	if (fseek (fh, 0, SEEK_END) != 0) return 0;

	long fileSize = ftell (fh);
	rewind (fh);

	if (fileSize > 0)
	{
		content = malloc (fileSize * sizeof(char) + 1);
		if (content == 0) return 0;
		int readBytes = fread (content, sizeof(char), fileSize, fh);

		if (feof (fh) || ferror (fh) || readBytes != fileSize) return 0;

		/* null terminate the string, as fread doesn't do it */
		(content)[fileSize] = 0;
	}
	else if (fileSize == 0)
	{
		content = malloc (1);
		if (content == 0) return 0;
		*content = (char) 0;
	}

	return content;
}

static int loadTree(augeas* augeasHandle, const char *lensPath, char *content)
{
	aug_set (augeasHandle, AUGEAS_CONTENT_ROOT, content);

	return aug_text_store (augeasHandle, lensPath, AUGEAS_CONTENT_ROOT,
	AUGEAS_TREE_ROOT);
}

static int saveFile(augeas* augeasHandle, FILE* fh)
{
	/* retrieve the file content */
	int ret = 0;
	const char* value = 0;
	aug_get (augeasHandle, AUGEAS_OUTPUT_ROOT, &value);

	/* write the file */
	if (value)
	{
		ret = fwrite (value, sizeof(char), strlen (value), fh);

		if (feof (fh) || ferror (fh)) return -1;
	}

	return ret;
}

static int saveTree(augeas* augeasHandle, KeySet* ks, const char* lensPath,
		Key *parentKey)
{
	int ret = 0;

	size_t prefixSize = keyGetNameSize (parentKey) - 1;
	size_t arraySize = ksGetSize (ks);
	Key **keyArray = convertToArray (ks);

	if (keyArray == 0) return -1;

	/* convert the Elektra KeySet to an Augeas tree */
	for (size_t i = 0; i < arraySize; i++)
	{
		Key *key = keyArray[i];
		char *nodeName;
		asprintf (&nodeName, AUGEAS_TREE_ROOT "%s",
				(keyName (key) + prefixSize));
		aug_set (augeasHandle, nodeName, keyString (key));
		free (nodeName);
	}

	/* remove keys not present in the KeySet */
	struct OrphanSearch *data = malloc (sizeof(struct OrphanSearch));

	if (!data) return -1;

	data->ks = ks;
	data->parentKey = parentKey;

	foreachAugeasNode (augeasHandle, AUGEAS_TREE_ROOT, &removeOrphan, data);

	/* build the tree */
	ret = aug_text_retrieve (augeasHandle, lensPath, AUGEAS_CONTENT_ROOT,
	AUGEAS_TREE_ROOT, AUGEAS_OUTPUT_ROOT);

	return ret;
}

int elektraAugeasOpen(Plugin *handle, Key *parentKey)
{
	augeas *augeasHandle;
	augeasHandle = aug_init (NULL, NULL,
			AUG_NO_MODL_AUTOLOAD | AUG_NO_ERR_CLOSE);

	if (aug_error (augeasHandle) != AUG_NOERROR)
	{
		char *errormessage;
		asprintf (&errormessage, "Unable to initialize augeas: %s",
				aug_error_message (augeasHandle));
		ELEKTRA_SET_ERROR(85, parentKey, errormessage);
		free (errormessage);
		return -1;
	}

	elektraPluginSetData (handle, augeasHandle);
	return 0;
}

int elektraAugeasClose(Plugin *handle, Key *parentKey ELEKTRA_UNUSED)
{
	augeas *augeasHandle = elektraPluginGetData (handle);

	if (augeasHandle) aug_close (augeasHandle);

	return 0;
}

int elektraAugeasGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	int ret = 0;

	if (!strcmp (keyName (parentKey), "system/elektra/modules/augeas"))
	{
		KeySet *info =
#include "contract.h"

				ksAppend (returned, info);
		ksDel (info);
		return 1;
	}

	augeas *augeasHandle = elektraPluginGetData (handle);

	/* retrieve the lens to use */
	const char* lensPath = getLensPath (handle);
	if (!lensPath)
	ELEKTRA_SET_GENERAL_ERROR(86, parentKey, keyName (parentKey));

	FILE *fh = fopen (keyString (parentKey), "r");

	if (fh == 0) ELEKTRA_SET_ERRNO_ERROR(9, parentKey);

	/* load its contents into a string */
	char *content = loadFile (fh);

	if (content == 0) ELEKTRA_SET_ERRNO_ERROR(76, parentKey);

	/* convert the string into an augeas tree */
	ret = loadTree (augeasHandle, lensPath, content);
	free (content);

	if (ret < 0) ELEKTRA_SET_AUGEAS_ERROR(augeasHandle, parentKey);

	/* convert the augeas tree to an Elektra KeySet */
	ksClear (returned);
	KeySet *append = ksNew (ksGetSize (returned) * 2, KS_END);

	Key *key = keyDup (parentKey);
	ksAppendKey (append, key);

	struct KeyConversion *conversionData = malloc (
			sizeof(struct KeyConversion));

	if (!conversionData)
	ELEKTRA_SET_GENERAL_ERROR(87, parentKey, strerror (errno));

	conversionData->currentOrder = 0;
	conversionData->parentKey = key;
	conversionData->ks = append;

	ret = foreachAugeasNode (augeasHandle, AUGEAS_TREE_ROOT, &convertToKey,
			conversionData);

	free (conversionData);

	if (ret < 0)
	{
		ksDel (append);
		ELEKTRA_SET_AUGEAS_ERROR(augeasHandle, parentKey);
	}

	ksAppend (returned, append);
	ksDel (append);
	errno = errnosave;
	return 1;
}

int elektraAugeasSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	int errnosave = errno;
	augeas *augeasHandle = elektraPluginGetData (handle);

	const char *lensPath = getLensPath (handle);

	if (!lensPath)
	ELEKTRA_SET_GENERAL_ERROR(86, parentKey, keyName (parentKey));

	FILE *fh = fopen (keyValue (parentKey), "w+");

	if (fh == 0) ELEKTRA_SET_ERRNO_ERROR(9, parentKey);

	int ret = 0;

	if (aug_match (augeasHandle, AUGEAS_TREE_ROOT, NULL) == 0)
	{
		/* load a fresh copy of the file into the tree */
		char *content = loadFile (fh);

		if (content == 0) ELEKTRA_SET_ERRNO_ERROR(76, parentKey);

		/* convert the string into an augeas tree */
		ret = loadTree (augeasHandle, lensPath, content);
		free (content);

		if (ret < 0) ELEKTRA_SET_AUGEAS_ERROR(augeasHandle, parentKey);
	}

	ret = saveTree (augeasHandle, returned, lensPath, parentKey);

	if (ret < 0)
	{
		fclose (fh);
		/* TODO: this is not always an Augeas error (could be an malloc error) */
		ELEKTRA_SET_AUGEAS_ERROR(augeasHandle, parentKey);
	}

	/* write the Augeas tree to the file */
	ret = saveFile (augeasHandle, fh);
	fclose (fh);

	if (ret < 0) ELEKTRA_SET_ERRNO_ERROR(75, parentKey);

	errno = errnosave;
	return 1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(augeas){
return elektraPluginExport ("augeas",
		ELEKTRA_PLUGIN_GET, &elektraAugeasGet,
		ELEKTRA_PLUGIN_SET, &elektraAugeasSet,
		ELEKTRA_PLUGIN_OPEN, &elektraAugeasOpen,
		ELEKTRA_PLUGIN_CLOSE, &elektraAugeasClose,
		ELEKTRA_PLUGIN_END);
}

