/**
 * @file
 *
 * @brief Source for lock plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "lock.h"

#include <kdbhelper.h>
#include <kdbprivate.h>
#include <kdberrors.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char * createLockFilePath (const char * filePath)
{
	char * lockFilePath = malloc (elektraStrLen (filePath) + elektraStrLen (LOCK_FILE_POSTFIX) + 1);
	if (!lockFilePath)
	{
		return NULL;
	}
	strcpy (lockFilePath, filePath);
	strcat (lockFilePath, LOCK_FILE_POSTFIX);
	return lockFilePath;
}

int elektraLockGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/lock"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/lock", KEY_VALUE, "lock plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/lock/exports", KEY_END),
			       keyNew ("system/elektra/modules/lock/exports/get", KEY_FUNC, elektraLockGet, KEY_END),
			       keyNew ("system/elektra/modules/lock/exports/set", KEY_FUNC, elektraLockSet, KEY_END),
#include ELEKTRA_README (lock)
			       keyNew ("system/elektra/modules/lock/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	char * lockFilePath = createLockFilePath (keyString (parentKey));
	if (!lockFilePath)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "malloc fail in %s\n", "get");
		return -1;
	}
	FILE * lockFile;

	while (1)
	{
		lockFile = fopen (lockFilePath, "r");
		if (lockFile)
		{
			fclose (lockFile);
		} else {
			break;
		}
	}
	lockFile = fopen (lockFilePath, "w");
	if (!lockFile)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "can not write lock file %s\n", lockFilePath);
		free (lockFilePath);
		return -1;
	}
	fclose (lockFile);
	free (lockFilePath);
	return 1; // success
}

int elektraLockSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	char * lockFilePath = createLockFilePath (keyString (parentKey));
	if (!lockFilePath)
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "malloc fail in %s\n", "set");
		return -1;
	}
	FILE * lockFile = fopen (lockFilePath, "r");
	if (lockFile)
	{
		fclose (lockFile);
		if (!remove (lockFilePath))
		{
			ELEKTRA_SET_ERRORF (145, parentKey, "removing lock file %s failed\n", lockFilePath);
			return -1;
		}
	} else
	{
		ELEKTRA_SET_ERRORF (145, parentKey, "lock file %s not present at set\n", lockFilePath);
		free (lockFilePath);
		return -1;
	}
	free (lockFilePath);
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (lock)
{
	// clang-format off
	return elektraPluginExport ("lock",
		ELEKTRA_PLUGIN_GET,	&elektraLockGet,
		ELEKTRA_PLUGIN_SET,	&elektraLockSet,
		ELEKTRA_PLUGIN_END);
}
