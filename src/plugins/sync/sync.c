/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "sync.h"

#include <kdberrors.h>

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define ERROR_SIZE 1024


int elektraSyncGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/sync"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/sync", KEY_VALUE, "sync plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/sync/exports", KEY_END),
			       keyNew ("system/elektra/modules/sync/exports/get", KEY_FUNC, elektraSyncGet, KEY_END),
			       keyNew ("system/elektra/modules/sync/exports/set", KEY_FUNC, elektraSyncSet, KEY_END),
#include ELEKTRA_README (sync)
			       keyNew ("system/elektra/modules/sync/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraSyncSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	/* set all keys */
	const char * configFile = keyString (parentKey);
	if (!strcmp (configFile, "")) return 0; // no underlying config file
	int fd = open (configFile, O_RDWR);
	if (fd == -1)
	{
		ELEKTRA_SET_ERRORF (89, parentKey, "Could not open config file %s because \"%s\"", configFile, strerror (errno));
		return -1;
	}
	if (fsync (fd) == -1)
	{
		ELEKTRA_SET_ERRORF (89, parentKey, "Could not fsync config file %s because \"%s\"", configFile, strerror (errno));
		close (fd);
		return -1;
	}
	close (fd);

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (sync)
{
	// clang-format off
	return elektraPluginExport("sync",
		ELEKTRA_PLUGIN_GET,	&elektraSyncGet,
		ELEKTRA_PLUGIN_SET,	&elektraSyncSet,
		ELEKTRA_PLUGIN_END);
}

