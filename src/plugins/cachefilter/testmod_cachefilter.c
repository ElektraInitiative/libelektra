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

#include <kdbconfig.h>
#include <kdbinternal.h>

#include <tests_plugin.h>

static KeySet * createTestKeysToCache ()
{
	return ksNew (3, keyNew ("user/tests/cachefilter/will/be/cached/key1", KEY_VALUE, "cached1", KEY_END),
		      keyNew ("user/tests/cachefilter/will/be/cached/key2", KEY_VALUE, "cached2", KEY_END),
		      keyNew ("user/tests/cachefilter/will/be/cached", KEY_VALUE, "cached", KEY_END), KS_END);
}

static KeySet * createTestKeysToCache2 ()
{
	return ksNew (3, keyNew ("user/tests/cachefilter/will/be/cached/key3", KEY_VALUE, "cached1", KEY_END),
		      keyNew ("user/tests/cachefilter/will/be/cached/key4", KEY_VALUE, "cached2", KEY_END),
		      keyNew ("user/tests/cachefilter/will/be/cached/key5", KEY_VALUE, "cached", KEY_END), KS_END);
}

static KeySet * createTestKeysToCache3 ()
{
	return ksNew (3, keyNew ("user/tests/cachefilter/will/be/cached/key6", KEY_VALUE, "cached1", KEY_END),
		      keyNew ("user/tests/cachefilter/will/be/cached/key7", KEY_VALUE, "cached2", KEY_END),
		      keyNew ("user/tests/cachefilter/will/be/cached/key8", KEY_VALUE, "cached", KEY_END), KS_END);
}

static KeySet * createTestKeysToNotCache ()
{
	return ksNew (3, keyNew ("user/tests/cachefilter/will/not/be/cached/key1", KEY_VALUE, "not-cached1", KEY_END),
		      keyNew ("user/tests/cachefilter/will/not/be/cached/key2", KEY_VALUE, "not-cached2", KEY_END),
		      keyNew ("user/tests/cachefilter/will/not/be/cached", KEY_VALUE, "not-cached", KEY_END), KS_END);
}

static void test_successfulCache ()
{
	Key * parentKey = keyNew ("user/tests/cachefilter/will/not/be/cached", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("cachefilter");

	KeySet * ks = createTestKeysToCache ();
	ksAppend (ks, createTestKeysToNotCache ());

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize (ks) == 3, "wrong number of keys in result, expected 3");

	KeySet * expected = createTestKeysToNotCache ();
	compare_keyset (ks, expected);
	ksDel (expected);

	// kdbSet() result >= 0 because nothing had to be done and should be successful
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize (ks) == 6, "wrong number of keys in result, expected 6");

	expected = createTestKeysToNotCache ();
	ksAppend (expected, createTestKeysToCache ());
	compare_keyset (ks, expected);
	ksDel (expected);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_successfulCacheLong ()
{
	Key * parentKey = keyNew ("user/tests/cachefilter/will/not/be/cached", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("cachefilter");

	KeySet * ks = createTestKeysToCache ();
	ksAppend (ks, createTestKeysToNotCache ());

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize (ks) == 3, "wrong number of keys in result, expected 3");

	KeySet * expected = createTestKeysToNotCache ();
	compare_keyset (ks, expected);
	ksDel (expected);

	// request some more keys in order to cache more
	ksAppend (ks, createTestKeysToCache2 ());

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful 2");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize (ks) == 3, "wrong number of keys in result, expected 3");

	expected = createTestKeysToNotCache ();
	compare_keyset (ks, expected);
	ksDel (expected);

	// and even more ...
	ksAppend (ks, createTestKeysToCache3 ());

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful 3");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize (ks) == 3, "wrong number of keys in result, expected 3");

	expected = createTestKeysToNotCache ();
	compare_keyset (ks, expected);
	ksDel (expected);

	// kdbSet() result >= 0 because nothing had to be done and should be successful
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbGet");
	succeed_if (output_warnings (parentKey), "warnings in kdbGet");
	succeed_if (ksGetSize (ks) == 12, "wrong number of keys in result, expected 12");

	expected = createTestKeysToNotCache ();
	ksAppend (expected, createTestKeysToCache ());
	ksAppend (expected, createTestKeysToCache2 ());
	ksAppend (expected, createTestKeysToCache3 ());
	compare_keyset (ks, expected);
	ksDel (expected);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("CACHEFILTER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_successfulCache ();
	test_successfulCacheLong ();

	printf ("\ntestmod_cachefilter RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
