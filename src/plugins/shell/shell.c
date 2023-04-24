/**
 * @file
 *
 * @brief Source for shell plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./shell.h"
#include <elektra/kdb/errors.h>
#include <errno.h>
#include <internal/utility/old_helper.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>


static int executeCommand (const char * cmdline)
{
	char * cmd[4];
	cmd[0] = "/bin/sh";
	cmd[1] = "-c";
	cmd[2] = (char *) cmdline;
	cmd[3] = NULL;

	pid_t pid = fork ();
	if (pid == 0)
	{
		return execv ("/bin/sh", cmd);
	}
	else if (pid > 0)
	{
		int status;
		wait (&status);
		return WEXITSTATUS (status);
	}
	else
	{
		return -1;
	}
}

int elektraShellGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/shell"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/shell", KEY_VALUE, "shell plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/shell/exports", KEY_END),
			       keyNew ("system:/elektra/modules/shell/exports/get", KEY_FUNC, elektraShellGet, KEY_END),
			       keyNew ("system:/elektra/modules/shell/exports/commit", KEY_FUNC, elektraShellCommit, KEY_END),
			       keyNew ("system:/elektra/modules/shell/exports/error", KEY_FUNC, elektraShellError, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/shell/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	KeySet * config = elektraPluginGetConfig (handle);
	Key * cmdKey = ksLookupByName (config, "/execute/get", KDB_O_NONE);
	Key * expectedReturnKey = ksLookupByName (config, "/execute/get/return", KDB_O_NONE);
	if (cmdKey == NULL)
		return 1;
	else
	{
		int retVal = executeCommand (keyString (cmdKey));
		if (retVal == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Launching childprocess failed. Reason: %s", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (keyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
								       "Return value of '%s' doesn't match expected exit. Reason: %s",
								       keyString (cmdKey), keyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraShellCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * cmdKey = ksLookupByName (config, "/execute/set", KDB_O_NONE);
	Key * expectedReturnKey = ksLookupByName (config, "/execute/set/return", KDB_O_NONE);
	if (cmdKey == NULL)
		return 1;
	else
	{
		int retVal = executeCommand (keyString (cmdKey));
		if (retVal == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Launching childprocess failed. Reason: %s", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (keyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
								       "Return value of '%s' doesn't match expected exit. Reason: %s",
								       keyString (cmdKey), keyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraShellError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (handle);
	Key * cmdKey = ksLookupByName (config, "/execute/error", KDB_O_NONE);
	Key * expectedReturnKey = ksLookupByName (config, "/execute/error/return", KDB_O_NONE);
	if (cmdKey == NULL)
		return 1;
	else
	{
		int retVal = executeCommand (keyString (cmdKey));
		if (retVal == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Launching childprocess failed. Reason: %s", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (keyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
								       "Return value of '%s' doesn't match expected exit. Reason: %s",
								       keyString (cmdKey), keyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("shell", ELEKTRA_PLUGIN_GET, &elektraShellGet, ELEKTRA_PLUGIN_COMMIT, &elektraShellCommit,
				    ELEKTRA_PLUGIN_ERROR, &elektraShellError, ELEKTRA_PLUGIN_END);
}
