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

#define AUGEAS_OUTPUT_ROOT "/raw/output"
#define AUGEAS_CONTENT_ROOT "/raw/content"
#define AUGEAS_TREE_ROOT "/raw/tree"

static void setKeyOrder(Key *key, int order)
{
	char *buffer;
	asprintf (&buffer, "%d", order);
	keySetMeta (key, "order", buffer);
	free (buffer);
}

static const char *getLensPath(Plugin *handle)
{
	KeySet *config = elektraPluginGetConfig (handle);
	Key* lensPathKey = ksLookupByName (config, "/lens", 0);
	return keyString (lensPathKey);
}

static int loadFile(FILE *fh, char **content)
{
	// allocate file content buffer
	fseek (fh, 0, SEEK_END);
	long fileSize = ftell (fh);
	rewind (fh);
	*content = malloc (fileSize * sizeof(char));

	if (*content == 0) return -1;

	fread (*content, sizeof(char), fileSize, fh);
	// REVIEW TODO: call feof(3) or ferror(3) to check for error

	return 0;
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
		// REVIEW TODO: call feof(3) or ferror(3) to check for error,
		// even on error it could be > 1
	}

	return ret;
}

static int saveTree(augeas* augeasHandle, Key** keyArray, size_t arraySize,
		const char* lensPath, size_t prefixSize)
{
	int ret = 0;

	/* erase the existing tree */
	aug_rm (augeasHandle, AUGEAS_TREE_ROOT);

	/* convert the Elektra KeySet to an Augeas tree */
	for (size_t i = 0; i < arraySize; i++)
	{
		Key *key = keyArray[i];
		char *nodeName;
		asprintf (&nodeName, AUGEAS_TREE_ROOT "/%s",
				(keyName (key) + prefixSize));
		aug_set (augeasHandle, nodeName, keyString (key));
		free (nodeName);
	}

	ret = aug_text_retrieve (augeasHandle, lensPath, AUGEAS_CONTENT_ROOT,
	AUGEAS_TREE_ROOT, AUGEAS_OUTPUT_ROOT);

	return ret;
}

// TODO: using the global state currentOrder is not very clean
static int convertToKeys(augeas *handle, KeySet *ks, const Key *rootKey,
		const char *treePath, int *currentOrder)
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
		char *curr = matches[i];
		const char *value = 0;
		result = aug_get (handle, curr, &value);

		if (result < 0) break;

		/* build an Elektra key */
		Key *key = keyDup (rootKey);
		keySetString (key, value);
		char *baseName = (strrchr (curr, '/') + 1);
		keyAddBaseName (key, baseName);
		(*currentOrder)++;
		setKeyOrder (key, *currentOrder);
		result = ksAppendKey (ks, key);

		if (result < 0) break;

		/* handle the subtree */
		// TODO: fix static order
		result = convertToKeys (handle, ks, key, curr, currentOrder);

		if (result < 0) break;

		free (curr);
	}

	for (; i < numMatches; i++)
	{
		free (matches[i]);
	}

	free (matches);

	return result;
}

static void reportAugeasError(augeas* augeasHandle, Key* parentKey)
{
	const char* message;
	if (aug_error (augeasHandle) != 0)
	{
		message = aug_error_message (augeasHandle);
	}
	else
	{
		const char *value = 0;
		aug_get (augeasHandle, "/augeas/text"AUGEAS_TREE_ROOT"/error/message", &value);

		if (value)
		{
			message = value;
		}
		else
		{
			message = "No specific reason was reported";
		}
	}

	ELEKTRA_SET_ERROR (85, parentKey, message);
}

int compareKeysByOrder(const void *a, const void *b)
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
		ELEKTRA_SET_ERROR (85, parentKey, errormessage);
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
	// REVIEW TODO: method too long
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

	/* retrieve the lens to use */
	const char* lensPath = getLensPath (handle);
	if (!lensPath)
	{
		ELEKTRA_SET_ERROR (86, parentKey, keyName (parentKey));
		errno = errnosave;
		return -1;
	}

	/* open the file */
	char* content;
	augeas *augeasHandle = elektraPluginGetData (handle);
	FILE *fh = fopen (keyValue (parentKey), "r");

	if (fh == 0)
	{
		ELEKTRA_SET_ERROR (9, parentKey, strerror (errno));
		errno = errnosave;
		return -1;
	}

	/* load its contents into a string */
	ret = loadFile (fh, &content);
	fclose (fh);

	if (ret < 0)
	{
		ELEKTRA_SET_ERROR (76, parentKey, strerror (errno));
		errno = errnosave;
		return -1;
	}

	/* convert the string into an augeas tree */
	ret = loadTree (augeasHandle, lensPath, content);
	free (content);

	if (ret < 0)
	{
		reportAugeasError (augeasHandle, parentKey);
		errno = errnosave;
		return -1;
	}

	/* convert the augeas tree to an Elektra KeySet */
	ksClear (returned);
	KeySet *append = ksNew (ksGetSize (returned) * 2, KS_END);

	Key *key = keyDup (parentKey);
	ksAppendKey (append, key);

	int order = 1;
	ret = convertToKeys (augeasHandle, append, key, AUGEAS_TREE_ROOT, &order);
	if (ret < 0)
	{
		ksDel (append);
		return -1;
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

	size_t prefixSize = keyGetNameSize (parentKey);
	const char *lensPath = getLensPath (handle);

	if (!lensPath)
	{
		ELEKTRA_SET_ERROR (86, parentKey, keyName (parentKey));
		errno = errnosave;
		return -1;
	}

	FILE *fh = fopen (keyValue (parentKey), "w");

	if (fh == 0)
	{
		ELEKTRA_SET_ERROR (9, parentKey, strerror (errno));
		errno = errnosave;
		return -1;
	}

	/* build an array of keys ordered by the order MetaKey */
	Key **keyArray;
	size_t arraySize = ksGetSize (returned);
	keyArray = calloc (arraySize, sizeof(Key *));

	if (keyArray == 0)
	{
		fclose (fh);
		ELEKTRA_SET_ERROR (87, parentKey, strerror (errno));
		errno = errnosave;
		return -1;
	}

	ksRewind (returned);
	size_t index = 0;

	Key *key;
	while ((key = ksNext (returned)) != 0)
	{
		keyArray[index] = key;
		++index;
	}

	qsort (keyArray, arraySize, sizeof(Key *), compareKeysByOrder);

	int ret = 0;

	ret = saveTree (augeasHandle, keyArray, arraySize, lensPath, prefixSize);

	if (ret < 0)
	{
		fclose (fh);
		reportAugeasError (augeasHandle, parentKey);
		errno = errnosave;
		return -1;
	}

	/* write the Augeas tree to the file */
	ret = saveFile (augeasHandle, fh);

	if (ret < 0)
	{
		fclose (fh);
		ELEKTRA_SET_ERROR (75, parentKey, strerror (errno));
		errno = errnosave;
		return -1;
	}

	free (keyArray);
	fclose (fh);
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

