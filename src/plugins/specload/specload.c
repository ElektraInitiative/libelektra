/**
 * @file
 *
 * @brief Source for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "specload.h"

#include <kdberrors.h>
#include <kdbhelper.h>

#include <kdbease.h>
#include <kdbinvoke.h>
#include <kdbmodule.h>
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
static struct change allowedChanges[] = { { "meta:/description", true, true, true }, { "meta:/opt/help", true, true, true },
					  { "meta:/comment", true, true, true },     { "meta:/order", true, true, true },
					  { "meta:/rationale", true, true, true },   { "meta:/requirement", true, true, true },
					  { "meta:/example", true, true, true },     { NULL, false, false, false } };

static bool readConfig (ElektraKeyset * conf, char ** directFilePtr, char ** appPtr, char *** argvPtr, ElektraKey * errorKey);
static bool loadSpec (ElektraKeyset * returned, const char * directFile, const char * app, char * argv[], ElektraKey * parentKey,
		      ElektraInvokeHandle * quickDump);
static int isChangeAllowed (ElektraKey * oldKey, ElektraKey * newKey);
static ElektraKeyset * calculateMetaDiff (ElektraKey * oldKey, ElektraKey * newKey);

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

static int copyError (ElektraKey * dest, ElektraKey * src)
{
	elektraKeyRewindMeta (src);
	const ElektraKey * metaKey = elektraKeyGetMeta (src, "error");
	if (!metaKey) return 0;
	elektraKeySetMeta (dest, elektraKeyName (metaKey), elektraKeyString (metaKey));
	while ((metaKey = elektraKeyNextMeta (src)) != NULL)
	{
		if (strncmp (elektraKeyName (metaKey), "error/", 6) != 0) break;
		elektraKeySetMeta (dest, elektraKeyName (metaKey), elektraKeyString (metaKey));
	}
	return 1;
}

int elektraSpecloadOpen (Plugin * handle, ElektraKey * errorKey)
{
	Specload * specload = elektraMalloc (sizeof (Specload));

	ElektraKeyset * conf = elektraPluginGetConfig (handle);
	if (elektraKeysetLookupByName (conf, "system:/module", 0) != NULL || elektraKeysetLookupByName (conf, "system:/sendspec", 0) != NULL)
	{
		elektraFree (specload);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (!readConfig (conf, &specload->directFile, &specload->app, &specload->argv, errorKey))
	{
		elektraFree (specload);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	specload->quickDumpConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	specload->quickDump = elektraInvokeOpen ("quickdump", specload->quickDumpConfig, errorKey);

	if (!specload->quickDump)
	{
		elektraFree (specload);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	elektraPluginSetData (handle, specload);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadClose (Plugin * handle, ElektraKey * errorKey)
{
	Specload * specload = elektraPluginGetData (handle);

	if (specload != NULL)
	{
		elektraInvokeClose (specload->quickDump, errorKey);

		elektraKeysetDel (specload->quickDumpConfig);

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
int elektraSpecloadSendSpec (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * spec, ElektraKey * parentKey)
{
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);

	ElektraKeyset * quickDumpConf = elektraKeysetNew (0, ELEKTRA_KS_END);

	if (elektraKeyGetMeta (parentKey, "system:/elektra/quickdump/noparent") != NULL)
	{
		elektraKeysetAppendKey (quickDumpConf, elektraKeyNew ("system:/noparent", ELEKTRA_KEY_END));
	}

	ElektraInvokeHandle * quickDump = elektraInvokeOpen ("quickdump", quickDumpConf, errorKey);

	ElektraKey * quickDumpParent = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, STDOUT_FILENAME, ELEKTRA_KEY_END);

	int result = elektraInvoke2Args (quickDump, "set", spec, quickDumpParent);

	elektraInvokeClose (quickDump, errorKey);
	elektraKeyDel (errorKey);
	elektraKeyDel (quickDumpParent);
	elektraKeysetDel (quickDumpConf);

	return result == ELEKTRA_PLUGIN_STATUS_SUCCESS ? ELEKTRA_PLUGIN_STATUS_SUCCESS : ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraSpecloadGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/specload"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/specload", ELEKTRA_KEY_VALUE, "specload plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports/open", ELEKTRA_KEY_FUNC, elektraSpecloadOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports/close", ELEKTRA_KEY_FUNC, elektraSpecloadClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports/get", ELEKTRA_KEY_FUNC, elektraSpecloadGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports/set", ELEKTRA_KEY_FUNC, elektraSpecloadSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports/checkconf", ELEKTRA_KEY_FUNC, elektraSpecloadCheckConf, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/specload/exports/sendspec", ELEKTRA_KEY_FUNC, elektraSpecloadSendSpec, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/specload/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (elektraKeyGetNamespace (parentKey) != ELEKTRA_NS_SPEC)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "This plugin can only be used for the spec namespace");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Specload * specload = elektraPluginGetData (handle);

	ElektraKeyset * spec = elektraKeysetNew (0, ELEKTRA_KS_END);

	if (!loadSpec (spec, specload->directFile, specload->app, specload->argv, parentKey, specload->quickDump))
	{
		elektraKeysetDel (spec);
		ELEKTRA_SET_INSTALLATION_ERROR (
			parentKey, "Couldn't load the base specification. Make sure the app is available and the arguments are correct");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * overlayFile = elektraKeyString (parentKey);
	if (overlayFile[0] != '/')
	{
		char * path = elektraFormat ("%s/%s", KDB_DB_SPEC, overlayFile);
		elektraKeySetString (parentKey, path);
		elektraFree (path);
	}

	if (access (elektraKeyString (parentKey), F_OK) != -1)
	{
		if (elektraInvoke2Args (specload->quickDump, "get", spec, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			elektraKeysetDel (spec);
			ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "Couldn't load the overlay specification");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	elektraKeysetAppend (returned, spec);
	elektraKeysetDel (spec);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (elektraKeyGetNamespace (parentKey) != ELEKTRA_NS_SPEC)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "This plugin can only be used for the spec namespace");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Specload * specload = elektraPluginGetData (handle);

	ElektraKeyset * spec = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (!loadSpec (spec, specload->directFile, specload->app, specload->argv, parentKey, specload->quickDump))
	{
		elektraKeysetDel (spec);
		ELEKTRA_SET_INSTALLATION_ERROR (
			parentKey, "Couldn't load the base specification. Make sure the app is available and the arguments are correct");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * overlayFile = elektraKeyString (parentKey);
	if (overlayFile[0] != '/')
	{
		char * path = elektraFormat ("%s/%s", KDB_DB_SPEC, overlayFile);
		elektraKeySetString (parentKey, path);
		elektraFree (path);
	}

	ElektraKeyset * oldData = elektraKeysetNew (elektraKeysetGetSize (returned), ELEKTRA_KS_END);
	if (access (elektraKeyString (parentKey), F_OK) != -1)
	{
		if (elektraInvoke2Args (specload->quickDump, "get", oldData, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			elektraKeysetDel (oldData);
			ELEKTRA_SET_INSTALLATION_ERROR (parentKey, "Couldn't load the overlay specification");
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	ElektraKeyset * overrides = elektraKeysetNew (0, ELEKTRA_KS_END);

	elektraCursor cursor = elektraKeysetGetCursor (returned);
	elektraKeysetRewind (returned);
	ElektraKey * new;
	ElektraKey * old;
	while ((new = elektraKeysetNext (returned)) != NULL)
	{
		old = elektraKeysetLookup (oldData, new, ELEKTRA_KDB_O_POP);
		if (old == NULL)
		{
			old = elektraKeysetLookup (spec, new, 0);
		}

		int changeAllowed = isChangeAllowed (old, new);
		elektraKeyDel (old);

		if (changeAllowed < 0)
		{
			ELEKTRA_SET_RESOURCE_ERROR (parentKey, "This kind of change is not allowed");
			elektraKeysetSetCursor (returned, cursor);
			elektraKeysetDel (overrides);
			elektraKeysetDel (oldData);
			elektraKeysetDel (spec);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (changeAllowed > 0)
		{
			elektraKeysetAppendKey (overrides, new);
		}
	}
	elektraKeysetDel (spec);

	// check if remaining old keys can be removed
	while ((old = elektraKeysetNext (oldData)) != NULL)
	{
		if (isChangeAllowed (old, NULL) > 0)
		{
			elektraKeysetSetCursor (returned, cursor);
			elektraKeysetDel (overrides);
			elektraKeysetDel (oldData);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	elektraKeysetDel (oldData);

	elektraKeysetSetCursor (returned, cursor);

	int result = elektraInvoke2Args (specload->quickDump, "set", overrides, parentKey);
	elektraKeysetDel (overrides);
	return result;
}

int elektraSpecloadCheckConf (ElektraKey * errorKey, ElektraKeyset * conf)
{
	char * directFile;
	char * app;
	char ** argv;

	if (!readConfig (conf, &directFile, &app, &argv, errorKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	bool directFileMode = directFile != NULL;

	ElektraKeyset * quickDumpConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraInvokeHandle * quickDump = elektraInvokeOpen ("quickdump", quickDumpConfig, errorKey);

	ElektraKeyset * spec = elektraKeysetNew (0, ELEKTRA_KS_END);

	bool result = loadSpec (spec, directFile, app, argv, errorKey, quickDump);

	elektraInvokeClose (quickDump, errorKey);
	elektraKeysetDel (quickDumpConfig);
	elektraFree (directFile);
	elektraFree (app);
	freeArgv (argv);
	elektraKeysetDel (spec);

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

bool readConfig (ElektraKeyset * conf, char ** directFilePtr, char ** appPtr, char *** argvPtr, ElektraKey * errorKey)
{
	ElektraKey * fileKey = elektraKeysetLookupByName (conf, "/file", 0);

	if (fileKey != NULL)
	{
		const char * directFile = elektraKeyString (fileKey);

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

	ElektraKey * appKey = elektraKeysetLookupByName (conf, "/app", 0);

	if (appKey == NULL)
	{
		ELEKTRA_SET_INSTALLATION_ERROR (errorKey, "You need to set an application using the app config key");
		return false;
	}

	const char * app = elektraKeyString (appKey);

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

	ElektraKeyset * args;
	if (elektraKeysetLookupByName (conf, "/app/args", 0) == NULL)
	{
		args = elektraKeysetNew (1, elektraKeyNew ("user:/app/args/#0", ELEKTRA_KEY_VALUE, "--elektra-spec", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		ElektraKey * parentKey = elektraKeyNew ("/app/args", ELEKTRA_KEY_END);
		args = elektraArrayGet (parentKey, conf);
		elektraKeyDel (parentKey);
	}

	ssize_t size = elektraKeysetGetSize (args);
	char ** argv = elektraMalloc ((size + 2) * sizeof (char *));
	argv[0] = elektraStrDup (app);

	size_t index = 1;
	elektraKeysetRewind (args);
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (args)) != NULL)
	{
		argv[index] = elektraStrDup (elektraKeyString (cur));
		++index;
	}
	argv[index] = NULL;
	elektraKeysetDel (args);

	*directFilePtr = NULL;
	*appPtr = elektraStrDup (app);
	*argvPtr = argv;

	return true;
}

bool loadSpec (ElektraKeyset * returned, const char * directFile, const char * app, char * argv[], ElektraKey * parentKey,
	       ElektraInvokeHandle * quickDump)
{
	if (directFile != NULL)
	{
		ElektraKey * quickDumpParent = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, directFile, ELEKTRA_KEY_END);
		int result = elektraInvoke2Args (quickDump, "get", returned, quickDumpParent);

		if (result != ELEKTRA_PLUGIN_STATUS_SUCCESS)
		{
			copyError (parentKey, quickDumpParent);
		}
		elektraKeyDel (quickDumpParent);

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

	ElektraKey * quickDumpParent = elektraKeyNew (elektraKeyName (parentKey), ELEKTRA_KEY_VALUE, STDIN_FILENAME, ELEKTRA_KEY_END);

	int result = elektraInvoke2Args (quickDump, "get", returned, quickDumpParent);

	if (result != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		copyError (parentKey, quickDumpParent);
	}
	elektraKeyDel (quickDumpParent);

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
int isChangeAllowed (ElektraKey * oldKey, ElektraKey * newKey)
{
	if (oldKey == newKey)
	{
		// same key (pointer)
		return 0;
	}

	elektraKeyFlags changes = elektraKeyCompare (oldKey, newKey);
	if (changes == 0)
	{
		// equal keys
		return 0;
	}

	if (changes != ELEKTRA_KEY_NULL && changes != ELEKTRA_KEY_META)
	{
		// only metadata changes allowed
		return -1;
	}

	if ((changes & ELEKTRA_KEY_NAME) != 0)
	{
		// different key names
		return -2;
	}

	if (oldKey == NULL)
	{
		if (elektraKeyIsBinary (newKey) ? elektraKeyValue (newKey) != NULL : strlen (elektraKeyString (newKey)) > 0)
		{
			// adding values not allowed
			return -1;
		}

		oldKey = elektraKeyNew (elektraKeyName (newKey), ELEKTRA_KEY_END);
	}
	else
	{
		oldKey = elektraKeyDup (oldKey, ELEKTRA_KEY_CP_ALL);
	}

	if (newKey == NULL)
	{
		if (elektraKeyIsBinary (oldKey) ? elektraKeyValue (oldKey) != NULL : strlen (elektraKeyString (oldKey)) > 0)
		{
			// removing values not allowed
			return -1;
		}

		newKey = elektraKeyNew (elektraKeyName (oldKey), ELEKTRA_KEY_END);
	}
	else
	{
		newKey = elektraKeyDup (newKey, ELEKTRA_KEY_CP_ALL);
	}

	ElektraKeyset * metaDiff = calculateMetaDiff (oldKey, newKey);

	elektraKeyDel (oldKey);
	elektraKeyDel (newKey);

	for (int i = 0; allowedChanges[i].meta != NULL; ++i)
	{
		struct change cur = allowedChanges[i];
		ElektraKey * diff = elektraKeysetLookupByName (metaDiff, cur.meta, ELEKTRA_KDB_O_POP);

		if (diff == NULL)
		{
			continue;
		}

		if (strcmp (elektraKeyString (diff), "add") == 0 && !cur.add)
		{
			elektraKeyDel (diff);
			elektraKeysetDel (metaDiff);
			// add not allowed
			return -1;
		}

		if (strcmp (elektraKeyString (diff), "edit") == 0 && !cur.edit)
		{
			elektraKeyDel (diff);
			elektraKeysetDel (metaDiff);
			// edit not allowed
			return -1;
		}

		if (strcmp (elektraKeyString (diff), "remove") == 0 && !cur.remove)
		{
			elektraKeyDel (diff);
			elektraKeysetDel (metaDiff);
			// remove not allowed
			return -1;
		}

		elektraKeyDel (diff);
	}

	size_t size = elektraKeysetGetSize (metaDiff);

	elektraKeysetDel (metaDiff);

	return size == 0 ? 1 : -1;
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
ElektraKeyset * calculateMetaDiff (ElektraKey * oldKey, ElektraKey * newKey)
{
	ElektraKeyset * result = elektraKeysetNew (0, ELEKTRA_KS_END);

	elektraKeyRewindMeta (oldKey);
	elektraKeyRewindMeta (newKey);

	const ElektraKey * oldMeta = elektraKeyNextMeta (oldKey);
	const ElektraKey * newMeta = elektraKeyNextMeta (newKey);

	while (oldMeta != NULL && newMeta != NULL)
	{
		const char * oldName = elektraKeyName (oldMeta);
		const char * newName = elektraKeyName (newMeta);

		int cmp = elektraStrCmp (oldName, newName);
		if (cmp < 0)
		{
			// oldKey has to "catch up"
			elektraKeysetAppendKey (result, elektraKeyNew (oldName, ELEKTRA_KEY_VALUE, "remove", ELEKTRA_KEY_META, "old", elektraKeyString (oldMeta), ELEKTRA_KEY_END));
			oldMeta = elektraKeyNextMeta (oldKey);
		}
		else if (cmp > 0)
		{
			// newKey has to "catch up"
			elektraKeysetAppendKey (result, elektraKeyNew (newName, ELEKTRA_KEY_VALUE, "add", ELEKTRA_KEY_META, "new", elektraKeyString (newMeta), ELEKTRA_KEY_END));
			newMeta = elektraKeyNextMeta (newKey);
		}
		else
		{
			// same name
			elektraKeysetAppendKey (result, elektraKeyNew (oldName, ELEKTRA_KEY_VALUE, "edit", ELEKTRA_KEY_META, "old", elektraKeyString (oldMeta), ELEKTRA_KEY_META, "new",
						     elektraKeyString (newMeta), ELEKTRA_KEY_END));
			oldMeta = elektraKeyNextMeta (oldKey);
			newMeta = elektraKeyNextMeta (newKey);
		}
	}

	// remaining metadata in oldKey was removed
	while ((oldMeta = elektraKeyNextMeta (oldKey)) != NULL)
	{
		elektraKeysetAppendKey (result, elektraKeyNew (elektraKeyName (oldMeta), ELEKTRA_KEY_VALUE, "remove", ELEKTRA_KEY_META, "old", elektraKeyString (oldMeta), ELEKTRA_KEY_END));
	}

	// remaining metadata in newKey was added
	while ((newMeta = elektraKeyNextMeta (newKey)) != NULL)
	{
		elektraKeysetAppendKey (result, elektraKeyNew (elektraKeyName (newMeta), ELEKTRA_KEY_VALUE, "add", ELEKTRA_KEY_META, "new", elektraKeyString (newMeta), ELEKTRA_KEY_END));
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
