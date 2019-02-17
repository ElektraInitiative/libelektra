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
#include <unistd.h>

// keep #ifdef in sync with kdb export
#ifdef _WIN32
#define STDIN_FILENAME ("CON")
#else
#define STDIN_FILENAME ("/dev/stdin")
#endif

static bool getAppAndArgs (KeySet * conf, char ** appPtr, char *** argvPtr, Key * errorKey);
static bool loadSpec (KeySet * returned, const char * app, char * argv[], Key * parentKey, ElektraInvokeHandle * quickDump);
static int isChangeAllowed (Key * oldKey, Key * newKey);

static inline void freeArgv (char ** argv)
{
	size_t index = 0;
	while (argv[index] != NULL)
	{
		elektraFree (argv[index]);
		++index;
	}
	elektraFree (argv);
}

static int copyError (Key * dest, Key * src)
{
	keyRewindMeta (src);
	const Key * metaKey = keyGetMeta (src, "error");
	if (!metaKey) return 0;
	keySetMeta (dest, keyName (metaKey), keyString (metaKey));
	while ((metaKey = keyNextMeta (src)) != NULL)
	{
		if (strncmp (keyName (metaKey), "error/", 6) != 0) break;
		keySetMeta (dest, keyName (metaKey), keyString (metaKey));
	}
	return 1;
}

int elektraSpecloadOpen (Plugin * handle, Key * errorKey)
{
	Specload * specload = elektraMalloc (sizeof (Specload));

	KeySet * conf = elektraPluginGetConfig (handle);
	if (!getAppAndArgs (conf, &specload->app, &specload->argv, errorKey))
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

	elektraInvokeClose (specload->quickDump, errorKey);

	ksDel (specload->quickDumpConfig);
	elektraFree (specload->app);
	freeArgv (specload->argv);

	elektraFree (specload);
	elektraPluginSetData (handle, NULL);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/specload"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/specload", KEY_VALUE, "specload plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/specload/exports", KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/open", KEY_FUNC, elektraSpecloadOpen, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/close", KEY_FUNC, elektraSpecloadClose, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/get", KEY_FUNC, elektraSpecloadGet, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/set", KEY_FUNC, elektraSpecloadSet, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/checkconf", KEY_FUNC, elektraSpecloadCheckConfig, KEY_END),
#include ELEKTRA_README (specload)
			       keyNew ("system/elektra/modules/specload/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	if (keyGetNamespace (parentKey) != KEY_NS_SPEC)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, parentKey, "This plugin can only be used for the spec namespace.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Specload * specload = elektraPluginGetData (handle);

	KeySet * spec = ksNew (0, KS_END);

	if (!loadSpec (spec, specload->app, specload->argv, parentKey, specload->quickDump))
	{
		ksDel (spec);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, parentKey,
				   "Couldn't load the base specification. Make sure the app is available and the arguments are correct.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (elektraInvoke2Args (specload->quickDump, "get", spec, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		ksDel (spec);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, parentKey, "Couldn't load the overlay specification.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ksAppend (returned, spec);
	ksDel (spec);

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (keyGetNamespace (parentKey) != KEY_NS_SPEC)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, parentKey, "This plugin can only be used for the spec namespace.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Specload * specload = elektraPluginGetData (handle);

	KeySet * oldData = ksNew (ksGetSize (returned), KS_END);
	if (elektraInvoke2Args (specload->quickDump, "get", oldData, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		ksDel (oldData);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, parentKey, "Couldn't load the overlay specification.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	KeySet * overrides = ksNew (0, KS_END);

	cursor_t cursor = ksGetCursor (returned);
	Key * new;
	Key * old;
	while ((new = ksNext (returned)) != NULL)
	{
		old = ksLookup (oldData, new, KDB_O_POP);

		int changeAllowed = isChangeAllowed (old, new);

		if (changeAllowed < 0)
		{
			ksSetCursor (returned, cursor);
			ksDel (overrides);
			ksDel (oldData);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}

		if (changeAllowed == 1)
		{
			ksAppendKey (overrides, new);
		}
	}

	// check if remaining old keys can be removed
	while ((old = ksNext (oldData)) != NULL)
	{
		if (!isChangeAllowed (old, NULL))
		{
			ksSetCursor (returned, cursor);
			ksDel (overrides);
			ksDel (oldData);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	ksDel (oldData);

	ksSetCursor (returned, cursor);

	int result = elektraInvoke2Args (specload->quickDump, "set", overrides, parentKey);
	ksDel (overrides);
	return result;
}

int elektraSpecloadCheckConfig (Key * errorKey, KeySet * conf)
{
	char * app;
	char ** argv;

	if (!getAppAndArgs (conf, &app, &argv, errorKey))
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	KeySet * quickDumpConfig = ksNew (0, KS_END);
	ElektraInvokeHandle * quickDump = elektraInvokeOpen ("quickdump", quickDumpConfig, errorKey);

	KeySet * spec = ksNew (0, KS_END);

	bool result = loadSpec (spec, app, argv, errorKey, quickDump);

	elektraInvokeClose (quickDump, errorKey);
	ksDel (quickDumpConfig);
	elektraFree (app);
	freeArgv (argv);
	ksDel (spec);

	if (!result)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, errorKey,
				   "Couldn't load the specification. Make sure the app is available and the arguments are correct.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

bool getAppAndArgs (KeySet * conf, char ** appPtr, char *** argvPtr, Key * errorKey)
{
	Key * appKey = ksLookupByName (conf, "/app", 0);

	if (appKey == NULL)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_SPECLOAD, errorKey, "You need to set an application using the app config key.");
		return false;
	}

	const char * app = keyString (appKey);

	if (app[0] != '/')
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, errorKey, "The value of the app config key ('%s') is not an absolute path.",
				    app);
		return false;
	}

	if (access (app, X_OK) != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, errorKey, "'%s' doesn't exist or is not executable.", app);
		return false;
	}

	KeySet * args;
	if (ksLookupByName (conf, "/app/args", 0) == NULL)
	{
		args = ksNew (1, keyNew ("user/app/args/#0", KEY_VALUE, "--elektra-spec", KEY_END), KS_END);
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
	ksRewind (args);
	Key * cur;
	while ((cur = ksNext (args)) != NULL)
	{
		argv[index] = elektraStrDup (keyString (cur));
		++index;
	}
	argv[index] = NULL;
	ksDel (args);

	*appPtr = elektraStrDup (app);
	*argvPtr = argv;

	return true;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (specload)
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

bool loadSpec (KeySet * returned, const char * app, char * argv[], Key * parentKey, ElektraInvokeHandle * quickDump)
{
	pid_t pid;
	int fd[2];

	if (pipe (fd) != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, parentKey, "Could not execute app: %s", strerror (errno));
		return NULL;
	}

	pid = fork ();

	if (pid == -1)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, parentKey, "Could not execute app: %s", strerror (errno));
		return NULL;
	}

	if (pid == 0)
	{
		// child
		if (dup2 (fd[1], STDOUT_FILENO) == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, parentKey, "Could not execute app: %s", strerror (errno));
			return NULL;
		}

		close (fd[0]);
		close (fd[1]);

		execv (app, argv);

		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, parentKey, "Could not execute app: %s", strerror (errno));
		return NULL;
	}

	// parent
	close (fd[1]);

	int stdin_copy = dup (STDIN_FILENO);

	if (dup2 (fd[0], STDIN_FILENO) == -1)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, parentKey, "Could not execute app: %s", strerror (errno));
		return NULL;
	}

	close (fd[0]);

	Key * quickDumpParent = keyNew ("", KEY_VALUE, STDIN_FILENAME, KEY_END);

	int result = elektraInvoke2Args (quickDump, "get", returned, quickDumpParent);

	if (result != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		copyError (parentKey, quickDumpParent);
	}
	keyDel (quickDumpParent);

	if (dup2 (stdin_copy, STDIN_FILENO) == -1)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_SPECLOAD, parentKey, "Could not execute app: %s", strerror (errno));
		return NULL;
	}
	close (stdin_copy);

	return result;
}

/**
 * @retval 0  no change detected
 * @retval 1  change allowed
 * @retval -1 change not allowed
 */
int isChangeAllowed (Key * oldKey, Key * newKey)
{
	if (oldKey == newKey)
	{
		return 1;
	}

	if (oldKey == NULL || newKey == NULL)
	{
		// TODO: add and remove disabled
		return -1;
	}

	// TODO: changes disabled
	return keyCompare (oldKey, newKey) == 0 ? 1 : 0;
}
