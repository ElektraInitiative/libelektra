/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "fstab.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdblogger.h>

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

int elektraFstabGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	int errnosave = errno;
	ssize_t nr_keys = 0;
	ElektraKey * key;
	ElektraKey * dir;
	FILE * fstab = 0;

	ELEKTRA_LOG ("get fstab %s from %s\n", elektraKeyName (parentKey), elektraKeyString (parentKey));

	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/fstab"))
	{
		// clang-format off
		ElektraKeyset *moduleConfig = elektraKeysetNew (50,
			elektraKeyNew ("system:/elektra/modules/fstab",
				ELEKTRA_KEY_VALUE, "fstab plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/exports/get",
				ELEKTRA_KEY_FUNC, elektraFstabGet,
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/exports/set",
				ELEKTRA_KEY_FUNC, elektraFstabSet,
				ELEKTRA_KEY_END),
#include "readme_fstab.c"
			elektraKeyNew ("system:/elektra/modules/fstab/infos/version",
				ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs",
				ELEKTRA_KEY_VALUE, "The configuration which is needed",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct",
				ELEKTRA_KEY_VALUE, "list FStab",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab",
				ELEKTRA_KEY_META, "check/type", "null empty",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/device",
				ELEKTRA_KEY_META, "check/type", "string",
				ELEKTRA_KEY_META, "check/path", "device",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/mpoint",
				ELEKTRA_KEY_META, "check/type", "string",
				ELEKTRA_KEY_META, "check/path", "directory",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/type",
				ELEKTRA_KEY_META, "check/type", "FSType",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/options",
				ELEKTRA_KEY_META, "check/type", "string",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/dumpfreq",
				ELEKTRA_KEY_META, "check/type", "unsigned_short",
				ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/fstab/config/needs/struct/FStab/passno",
				ELEKTRA_KEY_META, "check/type", "unsigned_short",
				ELEKTRA_KEY_END),
			ELEKTRA_KS_END);
		// clang-format on
		elektraKeysetAppend (returned, moduleConfig);
		elektraKeysetDel (moduleConfig);
		return 1;
	}

	key = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	elektraKeysetAppendKey (returned, key);
	nr_keys++;

	fstab = setmntent (elektraKeyString (parentKey), "r");
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
		dir = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (dir, fsname);
		elektraKeySetString (dir, "");
		keySetComment (dir, "");
		keySetComment (dir, "Filesystem pseudo-name");
		elektraKeysetAppendKey (returned, dir);

		key = elektraKeyDup (dir, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (key, "device");
		elektraKeySetString (key, fstabEntry->mnt_fsname);
		keySetComment (key, "Device or Label");
		elektraKeysetAppendKey (returned, key);

		key = elektraKeyDup (dir, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (key, "mpoint");
		elektraKeySetString (key, fstabEntry->mnt_dir);
		keySetComment (key, "Mount point");
		elektraKeysetAppendKey (returned, key);

		key = elektraKeyDup (dir, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (key, "type");
		elektraKeySetString (key, fstabEntry->mnt_type);
		keySetComment (key, "Filesystem type.");
		elektraKeysetAppendKey (returned, key);

		key = elektraKeyDup (dir, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (key, "options");
		elektraKeySetString (key, fstabEntry->mnt_opts);
		keySetComment (key, "Filesystem specific options");
		elektraKeysetAppendKey (returned, key);

		key = elektraKeyDup (dir, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (key, "dumpfreq");
		snprintf (buffer, MAX_NUMBER_SIZE, "%d", fstabEntry->mnt_freq);
		elektraKeySetString (key, buffer);
		keySetComment (key, "Dump frequency in days");
		elektraKeysetAppendKey (returned, key);

		key = elektraKeyDup (dir, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddBaseName (key, "passno");
		snprintf (buffer, MAX_NUMBER_SIZE, "%d", fstabEntry->mnt_passno);
		elektraKeySetString (key, buffer);
		keySetComment (key, "Pass number on parallel fsck");
		elektraKeysetAppendKey (returned, key);
	}

	endmntent (fstab);

	errno = errnosave;
	return nr_keys;
}


int elektraFstabSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * ks, ElektraKey * parentKey)
{
	int errnosave = errno;
	FILE * fstab = 0;
	ElektraKey * key = 0;
	const void * rootname = 0;
	struct mntent fstabEntry;

	ELEKTRA_LOG ("set fstab %s to file %s\n", elektraKeyName (parentKey), elektraKeyString (parentKey));

	elektraKeysetRewind (ks);
	if ((key = elektraKeysetNext (ks)) != 0)
	{
		/*skip parent key*/
	}

	fstab = setmntent (elektraKeyString (parentKey), "w");

	if (fstab == 0)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}

	memset (&fstabEntry, 0, sizeof (struct mntent));

	while ((key = elektraKeysetNext (ks)) != 0)
	{
		const char * basename = elektraKeyBaseName (key);
		ELEKTRA_LOG ("key: %s %s\n", elektraKeyName (key), basename);
		if (!strcmp (basename, "device"))
		{
			fstabEntry.mnt_fsname = (char *) elektraKeyValue (key);
		}
		else if (!strcmp (basename, "mpoint"))
		{
			fstabEntry.mnt_dir = (char *) elektraKeyValue (key);
		}
		else if (!strcmp (basename, "type"))
		{
			fstabEntry.mnt_type = (char *) elektraKeyValue (key);
		}
		else if (!strcmp (basename, "options"))
		{
			fstabEntry.mnt_opts = (char *) elektraKeyValue (key);
		}
		else if (!strcmp (basename, "dumpfreq"))
		{
			fstabEntry.mnt_freq = atoi ((char *) elektraKeyValue (key));
		}
		else if (!strcmp (basename, "passno"))
		{
			fstabEntry.mnt_passno = atoi ((char *) elektraKeyValue (key));
		}
		else
		{ // new rootname
			if (!rootname)
			{
				rootname = elektraKeyValue (key);
			}
			else
			{
				rootname = elektraKeyValue (key);
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


