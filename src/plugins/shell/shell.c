/**
 * @file
 *
 * @brief Source for shell plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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
	cmd[2] = (char *)cmdline;
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

int elektraShellGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/shell"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/shell", KEY_VALUE, "shell plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/shell/exports", KEY_END),
			       keyNew ("system/elektra/modules/shell/exports/get", KEY_FUNC, elektraShellGet, KEY_END),
			       keyNew ("system/elektra/modules/shell/exports/set", KEY_FUNC, elektraShellSet, KEY_END),
			       keyNew ("system/elektra/modules/shell/exports/error", KEY_FUNC, elektraShellError, KEY_END),
#include ELEKTRA_README (shell)
			       keyNew ("system/elektra/modules/shell/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
			ELEKTRA_SET_ERRORF (144, parentKey, "launching childprocess failed with %s\n", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (keyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_ERRORF (144, parentKey, "return value of %s doesn't match expected exit %s\n",
						    keyString (cmdKey), keyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraShellSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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
			ELEKTRA_SET_ERRORF (144, parentKey, "launching childprocess failed with %s\n", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (keyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_ERRORF (144, parentKey, "return value of %s doesn't match expected exit %s\n",
						    keyString (cmdKey), keyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

int elektraShellError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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
			ELEKTRA_SET_ERRORF (144, parentKey, "launching childprocess failed with %s\n", strerror (errno));
			return -1;
		}
		else if (expectedReturnKey)
		{
			if (atoi (keyString (expectedReturnKey)) != retVal)
			{
				ELEKTRA_SET_ERRORF (144, parentKey, "return value of %s doesn't match expected exit %s\n",
						    keyString (cmdKey), keyString (expectedReturnKey));
				return -1;
			}
		}
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (shell)
{
	return elektraPluginExport ("shell", ELEKTRA_PLUGIN_GET, &elektraShellGet, ELEKTRA_PLUGIN_SET, &elektraShellSet,
				    ELEKTRA_PLUGIN_ERROR, &elektraShellError, ELEKTRA_PLUGIN_END);
}
