/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests_plugin.h>


void test_readfstab (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/fstab", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = 0;
	PLUGIN_OPEN ("fstab");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	printf ("Reading fstab using file: %s\n", file);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	// output_keyset(ks);

	ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/fstab/\\//device", 0);
	exit_if_fail (key, "rootfs device not found");
	succeed_if (strcmp ("/dev/sda1", elektraKeyValue (key)) == 0, "device not correct");

	key = elektraKeysetLookupByName (ks, "user:/tests/fstab/\\/media\\/ext4/device", 0);
	exit_if_fail (key, "media device not found");
	succeed_if (strcmp ("/dev/sdg1", elektraKeyValue (key)) == 0, "device not correct");

	exit_if_fail (key = elektraKeysetLookupByName (ks, "user:/tests/fstab/\\/media\\/ext4/dumpfreq", 0), "rootfs device not found");
	succeed_if (strcmp ("0", elektraKeyValue (key)) == 0, "dumpfreq not correct");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_writefstab (const char * file)
{
	ElektraKeyset * conf = 0;
	PLUGIN_OPEN ("fstab");

	printf ("Writing fstab using file: %s\n", file);

	ElektraKeyset * ks = elektraKeysetNew (
		22, elektraKeyNew ("user:/tests/filesystems", ELEKTRA_KEY_VALUE, "filesystems", ELEKTRA_KEY_COMMENT, "", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\/", ELEKTRA_KEY_VALUE, "the root fs", ELEKTRA_KEY_COMMENT, "pseudo name", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\//device", ELEKTRA_KEY_VALUE, "/dev/sda6", ELEKTRA_KEY_COMMENT, "Device or Label", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\//dumpfreq", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_COMMENT, "Dump frequency in days", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\//mpoint", ELEKTRA_KEY_VALUE, "/", ELEKTRA_KEY_COMMENT, "Moint point", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\//options", ELEKTRA_KEY_VALUE, "defaults,errors=remount-ro", ELEKTRA_KEY_COMMENT,
			"Fileuser/tests specific options. See mount(8)", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\//passno", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_COMMENT, "Pass number on parallel fsck", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/\\//type", ELEKTRA_KEY_VALUE, "jfs", ELEKTRA_KEY_COMMENT, "Fileuser/tests type. See fs(5)", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00", ELEKTRA_KEY_VALUE, "non-swapfs", ELEKTRA_KEY_COMMENT, "pseudo name", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00/device", ELEKTRA_KEY_VALUE, "/dev/sda10", ELEKTRA_KEY_COMMENT, "Device or Label", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00/dumpfreq", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_COMMENT, "Dump frequency in days", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00/mpoint", ELEKTRA_KEY_VALUE, "none", ELEKTRA_KEY_COMMENT, "Moint point", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00/options", ELEKTRA_KEY_VALUE, "sw", ELEKTRA_KEY_COMMENT,
			"Fileuser/tests specific options. See mount(8)", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00/passno", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_COMMENT, "Pass number on parallel fsck", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/filesystems/swap00/type", ELEKTRA_KEY_VALUE, "swap", ELEKTRA_KEY_COMMENT, "Fileuser/tests type. See fs(5)", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/filesystems", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_regex_to_line_files (srcdir_file (file), elektraKeyString (parentKey)), "files do not match as expected");

	elektraUnlink (elektraKeyString (parentKey));
	elektraKeyDel (parentKey);

	elektraKeysetDel (ks);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("FSTAB       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_readfstab ("fstab/fstab");
	test_writefstab ("fstab/fstab-write");

	print_result ("testmod_fstab");

	return nbError;
}
