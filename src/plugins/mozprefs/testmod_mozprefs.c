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
	ElektraKey * parentKey = keyNew ("user:/tests/pref-read", ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);

	PLUGIN_OPEN ("mozprefs");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key = ksLookupByName (ks, "user:/tests/pref-read/user/a/user/key", ELEKTRA_KDB_O_NONE);
	exit_if_fail (key, "Key a.user.key not found");
	succeed_if (!strcmp (keyString (key), "usertest"), "Key a.user.key contains invalid data");
	key = ksLookupByName (ks, "user:/tests/pref-read/lock/a/lock/key", ELEKTRA_KDB_O_NONE);
	exit_if_fail (key, "Key a.lock.key not found");
	succeed_if (!strcmp (keyString (key), "true"), "Key a.lock.key contains invalid data");
	key = ksLookupByName (ks, "user:/tests/pref-read/pref/a/default/key", ELEKTRA_KDB_O_NONE);
	exit_if_fail (key, "Key a.default.key not found");
	succeed_if (!strcmp (keyString (key), "1"), "Key a.default.key contains invalid data");
	key = ksLookupByName (ks, "user:/tests/pref-read/sticky/a/sticky/key", ELEKTRA_KDB_O_NONE);
	exit_if_fail (key, "Key a.sticky.key not found");
	succeed_if (!strcmp (keyString (key), "false"), "Key a.sticky.key contains invalid data");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_writePref (char * fileName)
{
	ElektraKey * parentKey = keyNew ("user:/tests/pref-write", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);

	PLUGIN_OPEN ("mozprefs");

	ElektraKeyset * ks = ksNew (
		30, keyNew ("user:/tests/pref-write/user/a/user/key", ELEKTRA_KEY_VALUE, "usertest", ELEKTRA_KEY_META, "type", "string", ELEKTRA_KEY_END),
		keyNew ("user:/tests/pref-write/lock/a/lock/key", ELEKTRA_KEY_VALUE, "true", ELEKTRA_KEY_META, "type", "boolean", ELEKTRA_KEY_END),
		keyNew ("user:/tests/pref-write/pref/a/default/key", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_META, "type", "integer", ELEKTRA_KEY_END),
		keyNew ("user:/tests/pref-write/sticky/a/sticky/key", ELEKTRA_KEY_VALUE, "false", ELEKTRA_KEY_META, "type", "boolean", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("PREFS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);


	test_readPref ("mozprefs/prefs.js");
	test_writePref ("mozprefs/prefs.js");

	print_result ("testmod_mozprefs");

	return nbError;
}
