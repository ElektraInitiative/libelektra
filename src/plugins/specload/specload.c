/**
 * @file
 *
 * @brief Source for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./specload.h"

#include <elektra/core/errors.h>
#include <elektra/ease/array.h>
#include <elektra/plugin/invoke.h>
#include <elektra/utility/format.h>
#include <internal/macros/attributes.h>
#include <internal/pluginload/module.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <utime.h>

// keep #ifdef in sync with kdb export
#ifdef _WIN32
#define STDIN_FILENAME ("CON")
#define STDOUT_FILENAME ("CON")
#else
#define STDIN_FILENAME ("/dev/stdin")
#define STDOUT_FILENAME ("/dev/stdout")
#endif

struct change
{
	const char * meta;
	bool add;
	bool edit;
	bool remove;
};

// TODO: allow more changes
static const struct change allowedChanges[] = { { "meta:/description", true, true, true }, { "meta:/opt/help", true, true, true },
						{ "meta:/comment", true, true, true },	   { "meta:/order", true, true, true },
						{ "meta:/rationale", true, true, true },   { "meta:/requirement", true, true, true },
						{ "meta:/example", true, true, true },	   { NULL, false, false, false } };

static bool readConfig (KeySet * conf, char ** directFilePtr, char ** appPtr, char *** argvPtr, Key * errorKey);
static bool loadSpec (KeySet * returned, const char * directFile, const char * app, char * argv[], Key * parentKey,
		      ElektraInvokeHandle * quickDump);
static int isChangeAllowed (Key * oldKey, Key * newKey);
int keyCompareMeta (const Key * k1, const Key * k2);
static KeySet * calculateMetaDiff (Key * oldKey, Key * newKey);

static inline void freeArgv (char ** argv)
{
	if (argv != NULL)
	{
		size_t index = 0;
		while (argv[index] != NULL)
		{
			elektraFree (argv[index]);
			++index;
		}
		elektraFree (argv);
	}
}

static int copyError (Key * dest, Key * src)
{
	const Key * metaKey = keyGetMeta (src, "error");
	if (!metaKey) return 0;
	keySetMeta (dest, keyName (metaKey), keyString (metaKey));


	KeySet * metaKeys = keyMeta ((Key *) metaKey);

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		metaKey = ksAtCursor (metaKeys, it);
		if (strncmp (keyName (metaKey), "error/", 6) != 0) break;
		keySetMeta (dest, keyName (metaKey), keyString (metaKey));
	}
	return 1;
}

int elektraSpecloadOpen (Plugin * handle, Key * errorKey)
{
	Specload * specload = elektraMalloc (sizeof (Specload));

	KeySet * conf = elektraPluginGetConfig (handle);
	if (ksLookupByName (conf, "system:/module", 0) != NULL || ksLookupByName (conf, "system:/sendspec", 0) != NULL)
	{
		elektraFree (specload);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (!readConfig (conf, &specload->directFile, &specload->app, &specload->argv, errorKey))
	{
		elektraFree (specload);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	specload->quickDumpConfig = ksNew (0, KS_END);
	specload->quickDump = elektraInvokeOpen ("quickdump", specload->quickDumpConfig, errorKey);

	if (!specload->quickDump)
	{
		elektraFree (specload);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	elektraPluginSetData (handle, specload);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadClose (Plugin * handle, Key * errorKey)
{
	Specload * specload = elektraPluginGetData (handle);

	if (specload != NULL)
	{
		elektraInvokeClose (specload->quickDump, errorKey);

		ksDel (specload->quickDumpConfig);

		if (specload->directFile != NULL)
		{
			elektraFree (specload->directFile);
		}

		if (specload->app != NULL)
		{
			elektraFree (specload->app);
		}

		freeArgv (specload->argv);

		elektraFree (specload);
		elektraPluginSetData (handle, NULL);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Sends the given specification (@p spec) over stdout, to be received by the process using specload.
 *
 * Note: To use this function with elektraInvoke2Args, call elektraInvokeOpen with a config containing
 * the key 'system:/sendspec'. This postpones the check for an existent app until elektraSpecloadGet is called.
 *
 * @param handle    A specload plugin handle.
 * @param spec      The specification to send.
 * @param parentKey The parent key under which the target specload instance was mounted. Value unused.
 *
 * @retval #ELEKTRA_PLUGIN_STATUS_SUCCESS on success
 * @retval #ELEKTRA_PLUGIN_STATUS_ERROR on error
 */
int elektraSpecloadSendSpec (Plugin * handle ELEKTRA_UNUSED, KeySet * spec, Key * parentKey)
{
	if (spec == NULL || parentKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	Key * errorKey = keyNew ("/", KEY_END);

	KeySet * quickDumpConf = ksNew (0, KS_END);

	if (keyGetMeta (parentKey, "system:/elektra/quickdump/noparent") != NULL)
	{
		ksAppendKey (quickDumpConf, keyNew ("system:/noparent", KEY_END));
	}

	ElektraInvokeHandle * quickDump = elektraInvokeOpen ("quickdump", quickDumpConf, errorKey);

	Key * quickDumpParent = keyNew (keyName (parentKey), KEY_VALUE, STDOUT_FILENAME, KEY_END);

	int result = elektraInvoke2Args (quickDump, "set", spec, quickDumpParent);

	elektraInvokeClose (quickDump, errorKey);
	keyDel (errorKey);
	keyDel (quickDumpParent);
	ksDel (quickDumpConf);

	return result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraSpecloadGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/specload"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/specload", KEY_VALUE, "specload plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports", KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports/open", KEY_FUNC, elektraSpecloadOpen, KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports/close", KEY_FUNC, elektraSpecloadClose, KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports/get", KEY_FUNC, elektraSpecloadGet, KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports/set", KEY_FUNC, elektraSpecloadSet, KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports/checkconf", KEY_FUNC, elektraSpecloadCheckConf, KEY_END),
			       keyNew ("system:/elektra/modules/specload/exports/sendspec", KEY_FUNC, elektraSpecloadSendSpec, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/specload/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (keyGetNamespace (parentKey) != KEY_NS_SPEC)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "This plugin can only be used for the spec namespace");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Specload * specload = elektraPluginGetData (handle);

	KeySet * spec = ksNew (0, KS_END);

	if (!loadSpec (spec, specload->directFile, specload->app, specload->argv, parentKey, specload->quickDump))
	{
		ksDel (spec);
		ELEKTRA_SET_INSTALLATION_ERROR (
			parentKey, "Couldn't load the base specification. Make sure the app is available and the arguments are correct");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * overlayFile = keyString (parentKey);
	if (overlayFile[0] != '/')
	{
		char * path = elektraFormat ("%s/%s", KDB_DB_SPEC, overlayFile);
		keySetString (parentKey, path);
		elektraFree (path);
	}

	if (access (keyString (parentKey), F_OK) != -1)
	{
		if (elektraInvoke2Args (specload->quickDump, "get", spec, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			ksDel (spec);
			ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "Couldn't load the overlay specification");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ksAppend (returned, spec);
	ksDel (spec);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (keyGetNamespace (parentKey) != KEY_NS_SPEC)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "This plugin can only be used for the spec namespace");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Specload * specload = elektraPluginGetData (handle);

	KeySet * spec = ksNew (0, KS_END);
	if (!loadSpec (spec, specload->directFile, specload->app, specload->argv, parentKey, specload->quickDump))
	{
		ksDel (spec);
		ELEKTRA_SET_INSTALLATION_ERROR (
			parentKey, "Couldn't load the base specification. Make sure the app is available and the arguments are correct");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * overlayFile = keyString (parentKey);
	if (overlayFile[0] != '/')
	{
		char * path = elektraFormat ("%s/%s", KDB_DB_SPEC, overlayFile);
		keySetString (parentKey, path);
		elektraFree (path);
	}

	KeySet * oldData = ksNew (ksGetSize (returned), KS_END);
	if (access (keyString (parentKey), F_OK) != -1)
	{
		if (elektraInvoke2Args (specload->quickDump, "get", oldData, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			ksDel (oldData);
			ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "Couldn't load the overlay specification");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	KeySet * overrides = ksNew (0, KS_END);

	Key * new;
	Key * old;

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		new = ksAtCursor (returned, it);

		/* internal cursor of oldData should not change because of KDB_O_POP argument */
		old = ksLookup (oldData, new, KDB_O_POP);

		if (old == NULL)
		{
			old = ksLookup (spec, new, 0);
		}

		int changeAllowed = isChangeAllowed (old, new);
		keyDel (old);

		if (changeAllowed < 0)
		{
			ELEKTRA_SET_RESOURCE_ERROR (parentKey, "This kind of change is not allowed");
			ksDel (overrides);
			ksDel (oldData);
			ksDel (spec);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (changeAllowed > 0)
		{
			ksAppendKey (overrides, new);
		}
	}
	ksDel (spec);

	// check if remaining old keys can be removed

	for (elektraCursor it = 0; it < ksGetSize (oldData); ++it)
	{
		old = ksAtCursor (oldData, it);
		if (isChangeAllowed (old, NULL) > 0)
		{
			ksDel (overrides);
			ksDel (oldData);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	ksDel (oldData);

	int result = elektraInvoke2Args (specload->quickDump, "set", overrides, parentKey);
	ksDel (overrides);
	return result;
}

int elektraSpecloadCheckConf (Key * errorKey, KeySet * conf)
{
	char * directFile;
	char * app;
	char ** argv;

	if (!readConfig (conf, &directFile, &app, &argv, errorKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	bool directFileMode = directFile != NULL;

	KeySet * quickDumpConfig = ksNew (0, KS_END);
	ElektraInvokeHandle * quickDump = elektraInvokeOpen ("quickdump", quickDumpConfig, errorKey);

	KeySet * spec = ksNew (0, KS_END);

	bool result = loadSpec (spec, directFile, app, argv, errorKey, quickDump);

	elektraInvokeClose (quickDump, errorKey);
	ksDel (quickDumpConfig);
	elektraFree (directFile);
	elektraFree (app);
	freeArgv (argv);
	ksDel (spec);

	if (!result)
	{
		if (directFileMode)
		{
			ELEKTRA_SET_INSTALLATION_ERROR (
				errorKey, "Couldn't load the specification. Make sure the specified file is a valid quickdump file");
		}
		else
		{
			ELEKTRA_SET_INSTALLATION_ERROR (
				errorKey, "Couldn't load the specification. Make sure the app is available and the arguments are correct");
		}
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

bool readConfig (KeySet * conf, char ** directFilePtr, char ** appPtr, char *** argvPtr, Key * errorKey)
{
	Key * fileKey = ksLookupByName (conf, "/file", 0);

	if (fileKey != NULL)
	{
		const char * directFile = keyString (fileKey);

		if (directFile[0] != '/')
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "The value of the file config key '%s' is not an absolute path",
								 directFile);
			return false;
		}

		if (access (directFile, R_OK) != 0)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "File '%s' doesn't exist or cannot be read", directFile);
			return false;
		}

		*directFilePtr = elektraStrDup (directFile);
		*appPtr = NULL;
		*argvPtr = NULL;

		return true;
	}

	Key * appKey = ksLookupByName (conf, "/app", 0);

	if (appKey == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "You need to set an application using the app config key");
		return false;
	}

	const char * app = keyString (appKey);

	if (app[0] != '/')
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "The value of the app config key '%s' is not an absolute path", app);
		return false;
	}

	if (access (app, X_OK) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "App '%s' doesn't exist or is not executable", app);
		return false;
	}

	KeySet * args;
	if (ksLookupByName (conf, "/app/args", 0) == NULL)
	{
		args = ksNew (1, keyNew ("user:/app/args/#0", KEY_VALUE, "--elektra-spec", KEY_END), KS_END);
	}
	else
	{
		Key * parentKey = keyNew ("/app/args", KEY_END);
		args = elektraArrayGet (parentKey, conf);
		keyDel (parentKey);
	}

	ssize_t size = ksGetSize (args);
	char ** argv = elektraMalloc ((size + 2) * sizeof (char *));
	argv[0] = elektraStrDup (app);

	size_t index = 1;
	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (args); ++it)
	{
		cur = ksAtCursor (args, it);
		argv[index] = elektraStrDup (keyString (cur));
		++index;
	}
	argv[index] = NULL;
	ksDel (args);

	*directFilePtr = NULL;
	*appPtr = elektraStrDup (app);
	*argvPtr = argv;

	return true;
}

bool loadSpec (KeySet * returned, const char * directFile, const char * app, char * argv[], Key * parentKey,
	       ElektraInvokeHandle * quickDump)
{
	if (directFile != NULL)
	{
		Key * quickDumpParent = keyNew (keyName (parentKey), KEY_VALUE, directFile, KEY_END);
		int result = elektraInvoke2Args (quickDump, "get", returned, quickDumpParent);

		if (result != ELEKTRA_PLUGIN_STATUS_SUCCESS)
		{
			copyError (parentKey, quickDumpParent);
		}
		keyDel (quickDumpParent);

		return result == ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	pid_t pid;
	int fd[2];

	if (pipe (fd) != 0)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app. Reason: %s", strerror (errno));
		return false;
	}

	pid = fork ();

	if (pid == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app. Reason: %s", strerror (errno));
		return false;
	}

	if (pid == 0)
	{
		// child
		if (dup2 (fd[1], STDOUT_FILENO) == -1)
		{
			exit (EXIT_FAILURE);
		}

		close (fd[0]);
		close (fd[1]);

		execv (app, argv);

		exit (EXIT_FAILURE);
	}

	// parent
	close (fd[1]);

	int stdin_copy = dup (STDIN_FILENO);

	if (dup2 (fd[0], STDIN_FILENO) == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app. Reason: %s", strerror (errno));
		return false;
	}

	close (fd[0]);

	Key * quickDumpParent = keyNew (keyName (parentKey), KEY_VALUE, STDIN_FILENAME, KEY_END);

	int result = elektraInvoke2Args (quickDump, "get", returned, quickDumpParent);

	if (result != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		copyError (parentKey, quickDumpParent);
	}
	keyDel (quickDumpParent);

	if (dup2 (stdin_copy, STDIN_FILENO) == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not execute app. Reason: %s", strerror (errno));
		return false;
	}
	close (stdin_copy);

	return result == ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/**
 * Checks whether the @p oldKey can be changed into @p newKey safely.
 *
 * Both @p oldKey and @p newKey may be NULL, to represent adding and
 * removing Keys respectively.
 *
 * @param oldKey old Key, or NULL for added Key
 * @param newKey new Key, or NULL for removed Key
 *
 * @retval 0  no change detected
 * @retval 1  change allowed
 * @retval -1 change forbidden
 * @retval -2 error, e.g. different keynames
 */
int isChangeAllowed (Key * oldKey, Key * newKey)
{
	if (oldKey == newKey)
	{
		/* same key (pointer) */
		return 0;
	}

	if (oldKey && newKey)
	{
		/* no null key is present */

		if (strcmp (keyName (oldKey), keyName (newKey)) != 0)
		{
			/* different names */
			return -2;
		}

		ssize_t oldKeyValueSize = keyGetValueSize (oldKey);
		ssize_t newKeyValueSize = keyGetValueSize (newKey);

		if (oldKeyValueSize != newKeyValueSize || memcmp (keyValue (oldKey), keyValue (newKey), oldKeyValueSize) != 0)
		{
			/* different values */
			return -1;
		}

		if (keyCompareMeta (oldKey, newKey) == 0)
		{
			/* equal names, values and metadata --> consider keys as equal */
			return 0;
		}
	}

	if (oldKey == NULL)
	{
		if (keyIsBinary (newKey) ? keyValue (newKey) != NULL : strlen (keyString (newKey)) > 0)
		{
			// adding values not allowed
			return -1;
		}

		oldKey = keyNew (keyName (newKey), KEY_END);
	}
	else
	{
		oldKey = keyDup (oldKey, KEY_CP_ALL);
	}

	if (newKey == NULL)
	{
		if (keyIsBinary (oldKey) ? keyValue (oldKey) != NULL : strlen (keyString (oldKey)) > 0)
		{
			// removing values not allowed
			return -1;
		}

		newKey = keyNew (keyName (oldKey), KEY_END);
	}
	else
	{
		newKey = keyDup (newKey, KEY_CP_ALL);
	}

	KeySet * metaDiff = calculateMetaDiff (oldKey, newKey);

	keyDel (oldKey);
	keyDel (newKey);

	for (int i = 0; allowedChanges[i].meta != NULL; ++i)
	{
		struct change cur = allowedChanges[i];
		Key * diff = ksLookupByName (metaDiff, cur.meta, KDB_O_POP);

		if (diff == NULL)
		{
			continue;
		}

		if (strcmp (keyString (diff), "add") == 0 && !cur.add)
		{
			keyDel (diff);
			ksDel (metaDiff);
			// add not allowed
			return -1;
		}

		if (strcmp (keyString (diff), "edit") == 0 && !cur.edit)
		{
			keyDel (diff);
			ksDel (metaDiff);
			// edit not allowed
			return -1;
		}

		if (strcmp (keyString (diff), "remove") == 0 && !cur.remove)
		{
			keyDel (diff);
			ksDel (metaDiff);
			// remove not allowed
			return -1;
		}

		keyDel (diff);
	}

	size_t size = ksGetSize (metaDiff);

	ksDel (metaDiff);

	return size == 0 ? 1 : -1;
}

/**
 * @brief Compares metadata of two keys
 *
 * @retval -1 if there is a difference
 * @retval 0 if metadata is identical
 */
int keyCompareMeta (const Key * k1, const Key * k2)
{
	Key * key1 = (Key *) k1;
	Key * key2 = (Key *) k2;

	KeySet * metaKeys1 = keyMeta (key1);
	KeySet * metaKeys2 = keyMeta (key2);

	if (ksGetSize (metaKeys1) != ksGetSize (metaKeys2)) return -1;

	for (elektraCursor it = 0; it < ksGetSize (metaKeys1); ++it)
	{
		const Key * meta1 = ksAtCursor (metaKeys1, it);
		const Key * meta2 = ksAtCursor (metaKeys2, it);

		if (strcmp (keyName (meta1), keyName (meta2))) return -1;
		if (strcmp (keyString (meta1), keyString (meta2))) return -1;
	}

	return 0;
}

/**
 * Calculate a diff for the metadata of two keys.
 *
 * For each meta key that is different between @p oldKey and @p newKey,
 * a key will be created in the resulting KeySet. The name of this key
 * is the name of the metakey. The value of the key is determined as follows:
 * <ul>
 *   <li>If @p oldKey has a meta key not present in @p newKey: value = "remove"</li>
 *   <li>If @p newKey has a meta key not present in @p oldKey: value = "add"</li>
 *   <li>If metakey is present in both @p oldKey and @p newKey, but its value changed: value = "edit"</li>
 * </ul>
 * Additionally the old and new values are stored in the metakeys `old` and `new` respectively.
 *
 * @param oldKey the old key
 * @param newKey the new key
 * @return a KeySet (has to be `ksDel`ed) containing the diff
 */
KeySet * calculateMetaDiff (Key * oldKey, Key * newKey)
{
	KeySet * result = ksNew (0, KS_END);

	KeySet * oldMetaKeys = keyMeta (oldKey);
	KeySet * newMetaKeys = keyMeta (newKey);

	const Key * oldMeta = ksAtCursor (oldMetaKeys, 0);
	const Key * newMeta = ksAtCursor (newMetaKeys, 0);

	elektraCursor itOldMeta = 1;
	elektraCursor itNewMeta = 1;

	while (oldMeta != NULL && newMeta != NULL)
	{
		const char * oldName = keyName (oldMeta);
		const char * newName = keyName (newMeta);

		int cmp = elektraStrCmp (oldName, newName);
		if (cmp < 0)
		{
			// oldKey has to "catch up"
			ksAppendKey (result, keyNew (oldName, KEY_VALUE, "remove", KEY_META, "old", keyString (oldMeta), KEY_END));
			oldMeta = ksAtCursor (oldMetaKeys, itOldMeta++);
		}
		else if (cmp > 0)
		{
			// newKey has to "catch up"
			ksAppendKey (result, keyNew (newName, KEY_VALUE, "add", KEY_META, "new", keyString (newMeta), KEY_END));
			newMeta = ksAtCursor (newMetaKeys, itNewMeta++);
		}
		else
		{
			// same name
			ksAppendKey (result, keyNew (oldName, KEY_VALUE, "edit", KEY_META, "old", keyString (oldMeta), KEY_META, "new",
						     keyString (newMeta), KEY_END));
			oldMeta = ksAtCursor (oldMetaKeys, itOldMeta++);
			newMeta = ksAtCursor (newMetaKeys, itNewMeta++);
		}
	}

	// remaining metadata in oldKey was removed
	while ((oldMeta = ksAtCursor (oldMetaKeys, itOldMeta++)) != NULL)
	{
		ksAppendKey (result, keyNew (keyName (oldMeta), KEY_VALUE, "remove", KEY_META, "old", keyString (oldMeta), KEY_END));
	}

	// remaining metadata in newKey was added
	while ((newMeta = ksAtCursor (newMetaKeys, itNewMeta++)) != NULL)
	{
		ksAppendKey (result, keyNew (keyName (newMeta), KEY_VALUE, "add", KEY_META, "new", keyString (newMeta), KEY_END));
	}

	return result;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("specload",
				    ELEKTRA_PLUGIN_OPEN,	&elektraSpecloadOpen,
				    ELEKTRA_PLUGIN_CLOSE,	&elektraSpecloadClose,
				    ELEKTRA_PLUGIN_GET,	&elektraSpecloadGet,
				    ELEKTRA_PLUGIN_SET,	&elektraSpecloadSet,
				    ELEKTRA_PLUGIN_END);
	// clang-format on
}
