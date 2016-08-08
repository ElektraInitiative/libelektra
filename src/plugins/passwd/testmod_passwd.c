/**
 * @file
 *
 * @brief Tests for passwd plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

void test_read ()
{
	Key * parentKey = keyNew ("user/tests/passwd-read", KEY_VALUE, srcdir_file ("passwd/passwd_in"), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/index", KEY_VALUE, "name", KEY_END), KS_END);
	PLUGIN_OPEN ("passwd");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet failed\n");
	Key * key = ksLookupByName (ks, "user/tests/passwd-read/root/uid", 0);
	succeed_if (key, "key root/uid not found\n");
	succeed_if (strcmp (keyString (key), "32") == 0, "root/uid doesn't match\n");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_write ()
{
	Key * parentKey = keyNew ("user/tests/passwd-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (30, keyNew ("system/index", KEY_VALUE, "name", KEY_END), KS_END);
	PLUGIN_OPEN ("passwd");
	KeySet * ks = ksNew (30, keyNew ("user/tests/passwd-write/root", KEY_BINARY, KEY_END),
			     keyNew ("user/tests/passwd-write/root/gecos", KEY_VALUE, "root", KEY_END),
			     keyNew ("user/tests/passwd-write/root/gid", KEY_VALUE, "1024", KEY_END),
			     keyNew ("user/tests/passwd-write/root/home", KEY_VALUE, "/root", KEY_END),
			     keyNew ("user/tests/passwd-write/root/passwd", KEY_VALUE, "x", KEY_END),
			     keyNew ("user/tests/passwd-write/root/shell", KEY_VALUE, "/bin/zsh", KEY_END),
			     keyNew ("user/tests/passwd-write/root/uid", KEY_VALUE, "1024", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "failed to write passwd file\n");
	succeed_if (compare_line_files (srcdir_file ("passwd/passwd_out"), keyString (parentKey)), "files do nat match as expected\n");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void test_read_write ()
{
	Key * parentKeyRead = keyNew ("user/tests/passwd-write", KEY_VALUE, srcdir_file ("passwd/passwd_in"), KEY_END);
	Key * parentKeyWrite = keyNew ("user/tests/passwd-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system/index", KEY_VALUE, "name", KEY_END), KS_END);
	PLUGIN_OPEN ("passwd");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKeyRead) == 1, "kdbGet failed\n");
	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKeyWrite) == 1, "kdbSet failed\n");
	succeed_if (compare_line_files (srcdir_file ("passwd/passwd_in"), keyString (parentKeyWrite)), "files do not match as expected\n");
	ksDel (ks);
	keyDel (parentKeyRead);
	keyDel (parentKeyWrite);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("PASSWD     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

#if HAS_FGETPWENT
	test_read ();
#endif
	test_write ();
#if HAS_FGETPWENT
	test_read_write ();
#endif

	printf ("\ntestmod_passwd RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
