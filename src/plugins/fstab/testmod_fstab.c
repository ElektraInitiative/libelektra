/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
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
	Key * parentKey = keyNew ("user:/tests/fstab", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = 0;
	PLUGIN_OPEN ("fstab");

	KeySet * ks = ksNew (0, KS_END);

	printf ("Reading fstab using file: %s\n", file);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	// output_keyset(ks);

	Key * key = ksLookupByName (ks, "user:/tests/fstab/\\//device", 0);
	exit_if_fail (key, "rootfs device not found");
	succeed_if (strcmp ("/dev/sda1", keyValue (key)) == 0, "device not correct");

	key = ksLookupByName (ks, "user:/tests/fstab/\\/media\\/ext4/device", 0);
	exit_if_fail (key, "media device not found");
	succeed_if (strcmp ("/dev/sdg1", keyValue (key)) == 0, "device not correct");

	exit_if_fail (key = ksLookupByName (ks, "user:/tests/fstab/\\/media\\/ext4/dumpfreq", 0), "rootfs device not found");
	succeed_if (strcmp ("0", keyValue (key)) == 0, "dumpfreq not correct");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void test_writefstab (const char * file)
{
	KeySet * conf = 0;
	PLUGIN_OPEN ("fstab");

	printf ("Writing fstab using file: %s\n", file);

	KeySet * ks = ksNew (
		22, keyNew ("user:/tests/filesystems", KEY_VALUE, "filesystems", KEY_META, "comment/#0", "", KEY_END),
		keyNew ("user:/tests/filesystems/\\/", KEY_VALUE, "the root fs", KEY_META, "comment/#0", "pseudo name", KEY_END),
		keyNew ("user:/tests/filesystems/\\//device", KEY_VALUE, "/dev/sda6", KEY_META, "comment/#0", "Device or Label", KEY_END),
		keyNew ("user:/tests/filesystems/\\//dumpfreq", KEY_VALUE, "0", KEY_META, "comment/#0", "Dump frequency in days", KEY_END),
		keyNew ("user:/tests/filesystems/\\//mpoint", KEY_VALUE, "/", KEY_META, "comment/#0", "Moint point", KEY_END),
		keyNew ("user:/tests/filesystems/\\//options", KEY_VALUE, "defaults,errors=remount-ro", KEY_META, "comment/#0",
			"Fileuser/tests specific options. See mount(8)", KEY_END),
		keyNew ("user:/tests/filesystems/\\//passno", KEY_VALUE, "1", KEY_META, "comment/#0", "Pass number on parallel fsck",
			KEY_END),
		keyNew ("user:/tests/filesystems/\\//type", KEY_VALUE, "jfs", KEY_META, "comment/#0", "Fileuser/tests type. See fs(5)",
			KEY_END),
		keyNew ("user:/tests/filesystems/swap00", KEY_VALUE, "non-swapfs", KEY_META, "comment/#0", "pseudo name", KEY_END),
		keyNew ("user:/tests/filesystems/swap00/device", KEY_VALUE, "/dev/sda10", KEY_META, "comment/#0", "Device or Label",
			KEY_END),
		keyNew ("user:/tests/filesystems/swap00/dumpfreq", KEY_VALUE, "0", KEY_META, "comment/#0", "Dump frequency in days",
			KEY_END),
		keyNew ("user:/tests/filesystems/swap00/mpoint", KEY_VALUE, "none", KEY_META, "comment/#0", "Moint point", KEY_END),
		keyNew ("user:/tests/filesystems/swap00/options", KEY_VALUE, "sw", KEY_META, "comment/#0",
			"Fileuser/tests specific options. See mount(8)", KEY_END),
		keyNew ("user:/tests/filesystems/swap00/passno", KEY_VALUE, "0", KEY_META, "comment/#0", "Pass number on parallel fsck",
			KEY_END),
		keyNew ("user:/tests/filesystems/swap00/type", KEY_VALUE, "swap", KEY_META, "comment/#0", "Fileuser/tests type. See fs(5)",
			KEY_END),
		KS_END);

	Key * parentKey = keyNew ("user:/tests/filesystems", KEY_VALUE, elektraFilename (), KEY_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_regex_to_line_files (srcdir_file (file), keyString (parentKey)), "files do not match as expected");

	elektraUnlink (keyString (parentKey));
	keyDel (parentKey);

	ksDel (ks);

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
