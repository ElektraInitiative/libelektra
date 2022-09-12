/**
 * @file
 *
 * @brief Source for shell plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "shell.h"
#include <errno.h>
#include <kdberrors.h>
#include <kdbhelper.h>
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

int elektraShellGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/shell"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/shell", ELEKTRA_KEY_VALUE, "shell plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/shell/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/shell/exports/get", ELEKTRA_KEY_FUNC, elektraShellGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/shell/exports/set", ELEKTRA_KEY_FUNC, elektraShellSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/shell/exports/error", ELEKTRA_KEY_FUNC, elektraShellError, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/shell/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * cmdKey = elektraKeysetLookupByName (config, "/execute/get", ELEKTRA_KDB_O_NONE);
	ElektraKey * expectedReturnKey = elektraKeysetLookupByName (config, "/execute/get/return", ELEKTRA_KDB_O_NONE);
	if (cmdKey == NULL)
		return 1;
	else
	{
		int retVal = executeCommand (elektraKeyString (cmdKey));
		if (retVal == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Launching childprocess failed. Reason: %s", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (elektraKeyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
								       "Return value of '%s' doesn't match expected exit. Reason: %s",
								       elektraKeyString (cmdKey), elektraKeyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraShellSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * cmdKey = elektraKeysetLookupByName (config, "/execute/set", ELEKTRA_KDB_O_NONE);
	ElektraKey * expectedReturnKey = elektraKeysetLookupByName (config, "/execute/set/return", ELEKTRA_KDB_O_NONE);
	if (cmdKey == NULL)
		return 1;
	else
	{
		int retVal = executeCommand (elektraKeyString (cmdKey));
		if (retVal == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Launching childprocess failed. Reason: %s", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (elektraKeyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
								       "Return value of '%s' doesn't match expected exit. Reason: %s",
								       elektraKeyString (cmdKey), elektraKeyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraShellError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * cmdKey = elektraKeysetLookupByName (config, "/execute/error", ELEKTRA_KDB_O_NONE);
	ElektraKey * expectedReturnKey = elektraKeysetLookupByName (config, "/execute/error/return", ELEKTRA_KDB_O_NONE);
	if (cmdKey == NULL)
		return 1;
	else
	{
		int retVal = executeCommand (elektraKeyString (cmdKey));
		if (retVal == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Launching childprocess failed. Reason: %s", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (elektraKeyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (parentKey,
								       "Return value of '%s' doesn't match expected exit. Reason: %s",
								       elektraKeyString (cmdKey), elektraKeyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	return elektraPluginExport ("shell", ELEKTRA_PLUGIN_GET, &elektraShellGet, ELEKTRA_PLUGIN_SET, &elektraShellSet,
				    ELEKTRA_PLUGIN_ERROR, &elektraShellError, ELEKTRA_PLUGIN_END);
}
