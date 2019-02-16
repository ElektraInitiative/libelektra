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

static KeySet * loadSpec (const char * app, char * argv[], Key * errorKey, KeySet * conf);

int elektraSpecloadOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraSpecloadGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/specload"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/specload", KEY_VALUE, "specload plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/specload/exports", KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/open", KEY_FUNC, elektraSpecloadOpen, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/close", KEY_FUNC, elektraSpecloadClose, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/get", KEY_FUNC, elektraSpecloadGet, KEY_END),
			       keyNew ("system/elektra/modules/specload/exports/checkconf", KEY_FUNC, elektraSpecloadCheckConfig, KEY_END),
#include ELEKTRA_README (specload)
			       keyNew ("system/elektra/modules/specload/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraSpecloadCheckConfig (Key * errorKey, KeySet * conf)
{
	Key * appKey = ksLookupByName (conf, "/app", 0);

	// FIXME: correct error codes
	if (appKey == NULL)
	{
		ELEKTRA_SET_ERROR (5, errorKey, "You need to set an application using the app config key.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	const char * app = keyString (appKey);

	if (app[0] != '/')
	{
		ELEKTRA_SET_ERRORF (5, errorKey, "The value of the app config key ('%s') is not an absolute path.", app);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	if (access (app, X_OK) != 0)
	{
		ELEKTRA_SET_ERRORF (5, errorKey, "'%s' doesn't exist or is not executable.", app);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	Key * parentKey = keyNew ("/app/args", KEY_END);
	KeySet * args = elektraArrayGet (parentKey, conf);
	keyDel (parentKey);

	char ** argv = elektraMalloc (ksGetSize (args) * sizeof (char *));

	size_t appLen = elektraStrLen (app);

	size_t argDataSize = appLen + 1;
	char * argData = elektraMalloc (argDataSize);
	char * arg = argData;

	strncpy (arg, app, appLen);
	arg += appLen;

	size_t index = 0;
	ksRewind (args);
	Key * cur;
	while ((cur = ksNext (args)) != NULL)
	{
		const char * value = keyString (cur);
		size_t len = elektraStrLen (value);
		argDataSize += len;
		elektraRealloc ((void **) argData, argDataSize);
		strncpy (arg, value, len);

		argv[index] = arg;
		arg += len;
	}

	KeySet * spec = loadSpec (app, argv, errorKey, conf);

	elektraFree (argv);
	ksDel (args);

	if (spec == NULL)
	{
		ELEKTRA_SET_ERROR (5, errorKey,
				   "Couldn't load the specification. Make sure the app is available and the arguments are correct.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	ksDel (spec);

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (specload)
{
	// clang-format off
	return elektraPluginExport ("specload",
		ELEKTRA_PLUGIN_OPEN,	&elektraSpecloadOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraSpecloadClose,
		ELEKTRA_PLUGIN_GET,	&elektraSpecloadGet,
		ELEKTRA_PLUGIN_END);
	// clang-format on
}

KeySet * loadSpec (const char * app, char * argv[], Key * errorKey, KeySet * conf)
{
	pid_t pid;
	int fd[2];

	pipe (fd);

	pid = fork ();

	if (pid == 0)
	{
		// child
		dup2 (fd[1], STDOUT_FILENO);
		close (fd[0]);
		close (fd[1]);
		execv (app, argv);
		// TODO: Failed to execute app
		return NULL;
	}

	// parent
	close (fd[1]);

	int stdin_copy = dup (STDIN_FILENO);

	dup2 (fd[0], STDIN_FILENO);
	close (fd[0]);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * ks = ksNew (0, KS_END);

	KeySet * quickDumpConf = ksDup (conf);
	ElektraInvokeHandle * quickDump = elektraInvokeOpen ("quickdump", quickDumpConf, errorKey);

	Key * quickDumpParent = keyNew ("system/elektra/modules/quickdump", STDIN_FILENAME, KEY_END);

	if (elektraInvoke2Args (quickDump, "get", ks, quickDumpParent) != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		elektraInvokeClose (quickDump, NULL);
		ksDel (quickDumpConf);
		ksDel (modules);
		ksDel (ks);
		dup2 (stdin_copy, STDIN_FILENO);
		close (stdin_copy);
		return NULL;
	}

	elektraInvokeClose (quickDump, errorKey);
	ksDel (quickDumpConf);
	ksDel (modules);
	ksDel (ks);
	dup2 (stdin_copy, STDIN_FILENO);
	close (stdin_copy);

	return ks;
}


// -------
// Import helpers
// -------
