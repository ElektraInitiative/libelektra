/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "sync.h"

#include <kdberrors.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define ERROR_SIZE 1024


int elektraSyncGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/sync"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/sync", ELEKTRA_KEY_VALUE, "sync plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/sync/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/sync/exports/get", ELEKTRA_KEY_FUNC, elektraSyncGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/sync/exports/commit", ELEKTRA_KEY_FUNC, elektraSyncCommit, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/sync/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	/* get all keys */

	return 1; /* success */
}

int elektraSyncCommit (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey)
{
	/* set all keys */
	const char * configFile = keyString (parentKey);
	if (!strcmp (configFile, "")) return 0; // no underlying config file

		// Syncing requires different functions for mingw vs. POSIX builds.
		// For mingw, we need to fflush(), for POSIX we need to fsync().
		// See https://stackoverflow.com/a/41615150
		// Using fsync(fileno(fd)) does not work!
#ifdef __MINGW32__
	FILE * fd = NULL;
	// For mingw, we need to use mode "wc" and fflush(). See https://stackoverflow.com/a/57090195 .
	const char * fileMode = "wc";
	fd = fopen (configFile, fileMode);
	if (fd == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open config file %s. Reason: %s", configFile, strerror (errno));
		return -1;
	}
	if (fflush (fd) == EOF)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not fsync/fflush config file %s. Reason: %s", configFile, strerror (errno));
		fclose (fd);
		return -1;
	}
	fclose (fd);
#else
	int fd = open (configFile, O_RDWR);
	if (fd == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open config file %s. Reason: %s", configFile, strerror (errno));
		return -1;
	}
	if (fsync (fd) == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not fsync config file %s. Reason: %s", configFile, strerror (errno));
		close (fd);
		return -1;
	}
	close (fd);
#endif

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("sync",
		ELEKTRA_PLUGIN_GET,	&elektraSyncGet,
		ELEKTRA_PLUGIN_COMMIT,	&elektraSyncCommit,
		ELEKTRA_PLUGIN_END);
}

