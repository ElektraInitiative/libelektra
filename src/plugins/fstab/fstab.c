/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "fstab.h"

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif

#include <internal/utility/logger.h>

#define MAX_NUMBER_SIZE 10

/** @param name is a buffer with KDB_MAX_PATH_LENGTH space.
 * @param fstabEntry will be used to get the name:
 * @param swapIndex will count up for every swap
 *
 *   - mnt_type will be checked if it is swap
 *
 * TODO Improvements:
 *   - no counting up of swap?
 *   - handle mountpoints none?
 *
 * Some logic to define the filesystem name when it is not
 * so obvious.
 */
void elektraFstabFsName (char * fsname, struct mntent * fstabEntry, unsigned int * swapIndex)
{

	if (!strcmp (fstabEntry->mnt_type, "swap"))
	{
		sprintf (fsname, "swap%02d", *swapIndex);
		++(*swapIndex);
	}
	else if (!strcmp (fstabEntry->mnt_dir, "none"))
	{
		strcpy (fsname, fstabEntry->mnt_type);
	}
	else
	{
		// Otherwise take dir as-is
		strcpy (fsname, fstabEntry->mnt_dir);
	}
}

int elektraFstabGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0;
	Key * key;
	Key * dir;
	FILE * fstab = 0;

	ELEKTRA_LOG ("get fstab %s from %s\n", keyName (parentKey), keyString (parentKey));

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/fstab"))
	{
		// clang-format off
		KeySet *moduleConfig = ksNew (50,
			keyNew ("system:/elektra/modules/fstab",
				KEY_VALUE, "fstab plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/fstab/exports", KEY_END),
			keyNew ("system:/elektra/modules/fstab/exports/get",
				KEY_FUNC, elektraFstabGet,
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/exports/set",
				KEY_FUNC, elektraFstabSet,
				KEY_END),
#include "readme_fstab.c"
			keyNew ("system:/elektra/modules/fstab/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs",
				KEY_VALUE, "The configuration which is needed",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct",
				KEY_VALUE, "list FStab",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab",
				KEY_META, "check/type", "null empty",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/device",
				KEY_META, "check/type", "string",
				KEY_META, "check/path", "device",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/mpoint",
				KEY_META, "check/type", "string",
				KEY_META, "check/path", "directory",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/type",
				KEY_META, "check/type", "FSType",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/options",
				KEY_META, "check/type", "string",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/dumpfreq",
				KEY_META, "check/type", "unsigned_short",
				KEY_END),
			keyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/passno",
				KEY_META, "check/type", "unsigned_short",
				KEY_END),
			KS_END);
		// clang-format on
		ksAppend (returned, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	key = keyDup (parentKey, KEY_CP_ALL);
	ksAppendKey (returned, key);
	nr_keys++;

	fstab = setmntent (keyString (parentKey), "r");
	if (fstab == 0)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}

	struct mntent * fstabEntry;
	char fsname[KDB_MAX_PATH_LENGTH];
	char buffer[MAX_NUMBER_SIZE];
	unsigned int swapIndex = 0;
	while ((fstabEntry = getmntent (fstab)))
	{
		nr_keys += 7;
		elektraFstabFsName (fsname, fstabEntry, &swapIndex);

		/* Include only the filesystem pseudo-names */
		dir = keyDup (parentKey, KEY_CP_ALL);
		keyAddBaseName (dir, fsname);
		keySetString (dir, "");
		keySetComment (dir, "");
		keySetComment (dir, "Filesystem pseudo-name");
		ksAppendKey (returned, dir);

		key = keyDup (dir, KEY_CP_ALL);
		keyAddBaseName (key, "device");
		keySetString (key, fstabEntry->mnt_fsname);
		keySetComment (key, "Device or Label");
		ksAppendKey (returned, key);

		key = keyDup (dir, KEY_CP_ALL);
		keyAddBaseName (key, "mpoint");
		keySetString (key, fstabEntry->mnt_dir);
		keySetComment (key, "Mount point");
		ksAppendKey (returned, key);

		key = keyDup (dir, KEY_CP_ALL);
		keyAddBaseName (key, "type");
		keySetString (key, fstabEntry->mnt_type);
		keySetComment (key, "Filesystem type.");
		ksAppendKey (returned, key);

		key = keyDup (dir, KEY_CP_ALL);
		keyAddBaseName (key, "options");
		keySetString (key, fstabEntry->mnt_opts);
		keySetComment (key, "Filesystem specific options");
		ksAppendKey (returned, key);

		key = keyDup (dir, KEY_CP_ALL);
		keyAddBaseName (key, "dumpfreq");
		snprintf (buffer, MAX_NUMBER_SIZE, "%d", fstabEntry->mnt_freq);
		keySetString (key, buffer);
		keySetComment (key, "Dump frequency in days");
		ksAppendKey (returned, key);

		key = keyDup (dir, KEY_CP_ALL);
		keyAddBaseName (key, "passno");
		snprintf (buffer, MAX_NUMBER_SIZE, "%d", fstabEntry->mnt_passno);
		keySetString (key, buffer);
		keySetComment (key, "Pass number on parallel fsck");
		ksAppendKey (returned, key);
	}

	endmntent (fstab);

	errno = errnosave;
	return nr_keys;
}


int elektraFstabSet (Plugin * handle ELEKTRA_UNUSED, KeySet * ks, Key * parentKey)
{
	int errnosave = errno;
	FILE * fstab = 0;
	Key * key = 0;
	const void * rootname = 0;
	struct mntent fstabEntry;

	ELEKTRA_LOG ("set fstab %s to file %s\n", keyName (parentKey), keyString (parentKey));

	fstab = setmntent (keyString (parentKey), "w");

	if (fstab == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	memset (&fstabEntry, 0, sizeof (struct mntent));

	/* start with 2nd element (skip parent key) */
	for (elektraCursor it = 1; it < ksGetSize (ks); ++it)
	{
		key = ksAtCursor (ks, it);
		const char * basename = keyBaseName (key);
		ELEKTRA_LOG ("key: %s %s\n", keyName (key), basename);
		if (!strcmp (basename, "device"))
		{
			fstabEntry.mnt_fsname = (char *) keyValue (key);
		}
		else if (!strcmp (basename, "mpoint"))
		{
			fstabEntry.mnt_dir = (char *) keyValue (key);
		}
		else if (!strcmp (basename, "type"))
		{
			fstabEntry.mnt_type = (char *) keyValue (key);
		}
		else if (!strcmp (basename, "options"))
		{
			fstabEntry.mnt_opts = (char *) keyValue (key);
		}
		else if (!strcmp (basename, "dumpfreq"))
		{
			fstabEntry.mnt_freq = atoi ((char *) keyValue (key));
		}
		else if (!strcmp (basename, "passno"))
		{
			fstabEntry.mnt_passno = atoi ((char *) keyValue (key));
		}
		else
		{ // new rootname
			if (!rootname)
			{
				rootname = keyValue (key);
			}
			else
			{
				rootname = keyValue (key);
				ELEKTRA_LOG ("first: %s   %s   %s   %s   %d %d\n", fstabEntry.mnt_fsname, fstabEntry.mnt_dir,
					     fstabEntry.mnt_type, fstabEntry.mnt_opts, fstabEntry.mnt_freq, fstabEntry.mnt_passno);
				addmntent (fstab, &fstabEntry);
				memset (&fstabEntry, 0, sizeof (struct mntent));
			}
		}
	}

	if (rootname)
	{
		ELEKTRA_LOG ("last: %s   %s   %s   %s   %d %d\n", fstabEntry.mnt_fsname, fstabEntry.mnt_dir, fstabEntry.mnt_type,
			     fstabEntry.mnt_opts, fstabEntry.mnt_freq, fstabEntry.mnt_passno);
		addmntent (fstab, &fstabEntry);
	}

	endmntent (fstab);
	errno = errnosave;
	return 1;
}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("fstab",
		ELEKTRA_PLUGIN_GET,            &elektraFstabGet,
		ELEKTRA_PLUGIN_SET,            &elektraFstabSet,
		ELEKTRA_PLUGIN_END);
}


