/**
 * @file
 *
 * @brief Tests for cachefilter plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <tests_internal.h>

#include <kdbinternal.h>
#include <kdbconfig.h>

#include <tests_plugin.h>

static KeySet * createTestKeysToCache ()
{
	return ksNew (3,
	        keyNew ("user/tests/cachefilter/will/be/cached/key1", KEY_VALUE, "cached1", KEY_END),
            keyNew ("user/tests/cachefilter/will/be/cached/key2", KEY_VALUE, "cached2", KEY_END),
            keyNew ("user/tests/cachefilter/will/be/cached", KEY_VALUE, "cached", KEY_END),
    KS_END);
}

static KeySet * createTestKeysToNotCache ()
{
    return ksNew (3,
		    keyNew ("user/tests/cachefilter/will/not/be/cached/key1", KEY_VALUE, "not-cached1", KEY_END),
		    keyNew ("user/tests/cachefilter/will/not/be/cached/key2", KEY_VALUE, "not-cached2", KEY_END),
		    keyNew ("user/tests/cachefilter/will/not/be/cached", KEY_VALUE, "not-cached", KEY_END),
    KS_END);
}

static void compareKeySets (KeySet * ks, KeySet * expected)
{
	succeed_if (ksGetSize (expected) == ksGetSize (ks), "KeySet on set does not contain the same amount of keys");
	Key * current;
	ksRewind (expected);
	while ((current = ksNext (expected)))
	{
		Key * key = ksLookup (ks, current, KDB_O_NONE);
		succeed_if (key, "Expected key was not found in KeySet");
		succeed_if (!strcmp (keyString (key), keyString (current)), "Value of key was modified");
	}
}

static void test_successfulCache ()
{
	Key * parentKey = keyNew ("user/tests/cachefilter/will/not/be/cached", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("cachefilter");

	KeySet * ks = createTestKeysToCache ();
	ksAppend(ks, createTestKeysToNotCache ());

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize(ks) == 3, "wrong number of keys in result, expected 3");

	KeySet * expected = createTestKeysToNotCache ();
	compareKeySets (ks, expected);
	
	succeed_if (plugin->kdbSet(plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize(ks) == 6, "wrong number of keys in result, expected 6");
	
	keyDel (parentKey);
	ksDel (expected);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("CACHEFILTER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_successfulCache ();

	printf ("\ntestmod_cachefilter RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
