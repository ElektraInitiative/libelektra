/**
 * @file
 *
 * @brief Implementation of kdb mountpoint command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <mountpoint-mount.h>
#include <mountpoint-remount.h>
#include <mountpoint-umount.h>
#include <mountpoint.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbmerge.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "mountpoint"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

command mountSubcommands[] = {
	{ "mount", addMountSpec, execMount },
	{ "remount", addRemountSpec, execRemount },
	{ "umount", addUmountSpec, execUmount },
};

void addMountpointSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Manage kdb mounts.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));

	for (unsigned long i = 0; i < sizeof (mountSubcommands) / sizeof (mountSubcommands[0]); ++i)
	{
		mountSubcommands[i].addSpec (spec);
	}
}

int execMountpoint (KeySet * options, Key * errorKey)
{
	const char * subcommand = keyString (ksLookupByName (options, CLI_BASE_KEY "/" COMMAND_NAME, 0));

	for (unsigned long i = 0; i < sizeof (mountSubcommands) / sizeof (mountSubcommands[0]); ++i)
	{
		if (elektraStrCmp (subcommand, mountSubcommands[i].name) == 0)
		{
			return mountSubcommands[i].exec (options, errorKey);
		}
	}
}

KeySet * getMountConfig (KDB * handle, Key * errorKey)
{
	Key * parent = keyNew (MOUNTPOINTS_PATH, KEY_END);
	KeySet * mountInfo = ksNew (0, KS_END);
	kdbGet (handle, mountInfo, parent);

	// TODO: maybe print warnings(or add them to error key)

	keyDel (parent);
	return mountInfo;
}
