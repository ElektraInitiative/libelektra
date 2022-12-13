/**
 * @file
 *
 * @brief Tests for mozprefs plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>


static void test_readPref (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/pref-read", KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("mozprefs");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key = ksLookupByName (ks, "user:/tests/pref-read/user/a/user/key", KDB_O_NONE);
	exit_if_fail (key, "Key a.user.key not found");
	succeed_if (!strcmp (keyString (key), "usertest"), "Key a.user.key contains invalid data");
	key = ksLookupByName (ks, "user:/tests/pref-read/lock/a/lock/key", KDB_O_NONE);
	exit_if_fail (key, "Key a.lock.key not found");
	succeed_if (!strcmp (keyString (key), "true"), "Key a.lock.key contains invalid data");
	key = ksLookupByName (ks, "user:/tests/pref-read/pref/a/default/key", KDB_O_NONE);
	exit_if_fail (key, "Key a.default.key not found");
	succeed_if (!strcmp (keyString (key), "1"), "Key a.default.key contains invalid data");
	key = ksLookupByName (ks, "user:/tests/pref-read/sticky/a/sticky/key", KDB_O_NONE);
	exit_if_fail (key, "Key a.sticky.key not found");
	succeed_if (!strcmp (keyString (key), "false"), "Key a.sticky.key contains invalid data");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_writePref (char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/pref-write", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("mozprefs");

	KeySet * ks = ksNew (
		30, keyNew ("user:/tests/pref-write/user/a/user/key", KEY_VALUE, "usertest", KEY_META, "type", "string", KEY_END),
		keyNew ("user:/tests/pref-write/lock/a/lock/key", KEY_VALUE, "true", KEY_META, "type", "boolean", KEY_END),
		keyNew ("user:/tests/pref-write/pref/a/default/key", KEY_VALUE, "1", KEY_META, "type", "integer", KEY_END),
		keyNew ("user:/tests/pref-write/sticky/a/sticky/key", KEY_VALUE, "false", KEY_META, "type", "boolean", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_setMetaMustFail (void)
{
	char const * const prefix = "user:/mozprefs/tests/writeMeta";

	Key * parentKey = keyNew (prefix, KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (1, keyNew ("user:/mozprefs/tests/writeMeta", KEY_VALUE, "abcd", KEY_META, "abcd", "abcd", KEY_END), KS_END);

	PLUGIN_OPEN ("mozprefs");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "Attempting to write a Meta Key did not fail");
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("PREFS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);


	test_readPref ("mozprefs/prefs.js");
	test_writePref ("mozprefs/prefs.js");
	test_setMetaMustFail()
	print_result ("testmod_mozprefs");

	return nbError;
}
