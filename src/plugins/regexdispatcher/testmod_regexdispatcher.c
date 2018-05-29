/**
 * @file
 *
 * @brief Tests for regexdispatcher plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_rangedispatcher (void)
{
	printf ("test_rangedispatcher\n");

	Key * parentKey = keyNew ("user/tests/regexdispatcher", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("regexdispatcher");

	KeySet * ks = ksNew (1, KS_END);
	Key * key1 = keyNew ("/key1", KEY_META, "check/range", "0-5000", KEY_END);
	ksAppendKey (ks, key1);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

	const Key * pKey1 = ksLookupByName (ks, "/key1", KDB_O_NONE);
	const Key * checkRange1 = keyGetMeta (pKey1, "elektra/spec/regex/check/range");

	succeed_if (checkRange1, "the range regex hasn't been generated");
	succeed_if (0 == strcmp (keyString (checkRange1), "[0-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-4][0-9][0-9][0-9]|5000"),
		    "the range regex is invalid");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_enumdispatcher (void)
{
	printf ("test_enumdispatcher\n");

	Key * parentKey = keyNew ("user/tests/regexdispatcher", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("regexdispatcher");

	KeySet * ks = ksNew (1, KS_END);
	Key * key1 = keyNew ("/key1", KEY_META, "check/enum/#1", "a", KEY_META, "check/enum/#2", "b", KEY_END);
	Key * key2 = keyNew ("/key2", KEY_META, "check/enum/#1", "a", KEY_META, "check/enum/#2", "b", KEY_META, "check/enum/multi", ",",
			     KEY_END);
	ksAppendKey (ks, key1);
	ksAppendKey (ks, key2);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

	const Key * pKey1 = ksLookupByName (ks, "/key1", KDB_O_NONE);
	const Key * checkEnum1 = keyGetMeta (pKey1, "elektra/spec/regex/check/enum");

	succeed_if (checkEnum1, "the single enum regex hasn't been generated");
	succeed_if (0 == strcmp (keyString (checkEnum1), "a|b"), "the single enum regex is invalid");

	const Key * pKey2 = ksLookupByName (ks, "/key2", KDB_O_NONE);
	const Key * checkEnum2 = keyGetMeta (pKey2, "elektra/spec/regex/check/enum");

	succeed_if (checkEnum2, "the multi enum regex hasn't been generated");
	succeed_if (0 == strcmp (keyString (checkEnum2), "(a,b)|(b,a)"), "the mutli enum regex is invalid");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_validationdispatcher (void)
{
	printf ("test_validationdispatcher\n");

	Key * parentKey = keyNew ("user/tests/regexdispatcher", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("regexdispatcher");

	KeySet * ks = ksNew (1, KS_END);
	Key * key1 = keyNew ("/key1", KEY_META, "check/validation", "(a|b)", KEY_END);
	Key * key2 = keyNew ("/key2", KEY_META, "check/validation", "(a|b)", KEY_META, "check/validation/invert", "", KEY_END);
	Key * key3 = keyNew ("/key3", KEY_META, "check/validation", "[aA]", KEY_META, "check/validation/ignorecase", "", KEY_END);

	ksAppendKey (ks, key1);
	ksAppendKey (ks, key2);
	ksAppendKey (ks, key3);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

	const Key * pKey1 = ksLookupByName (ks, "/key1", KDB_O_NONE);
	const Key * checkValidation1 = keyGetMeta (pKey1, "elektra/spec/regex/check/validation");

	succeed_if (checkValidation1, "the basic validation regex hasn't been generated");
	printf ("base is %s\n", keyString (checkValidation1));
	// libfa reformats it obviously as its minimized now
	succeed_if (0 == strcmp (keyString (checkValidation1), "[ab]"), "the basic validation regex is invalid");

	const Key * pKey2 = ksLookupByName (ks, "/key2", KDB_O_NONE);
	const Key * checkValidation2 = keyGetMeta (pKey2, "elektra/spec/regex/check/validation");

	succeed_if (checkValidation2, "the inverted validation regex hasn't been generated");
	printf ("inverted is %s\n", keyString (checkValidation2));
	// ([ab](.|)|[^ab])(.|)*|()
	succeed_if (0 == strcmp (keyString (checkValidation2), "(a|b*)"), "the inverted validation regex is invalid");

	const Key * pKey3 = ksLookupByName (ks, "/key3", KDB_O_NONE);
	const Key * checkValidation3 = keyGetMeta (pKey3, "elektra/spec/regex/check/validation");

	succeed_if (checkValidation3, "the case insensitive validation regex hasn't been generated");
	printf ("ignored is %s\n", keyString (checkValidation3));
	succeed_if (0 == strcmp (keyString (checkValidation3), "a"), "the case insensitive validiation regex is invalid");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("REGEXDISPATCHER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_rangedispatcher ();
	test_enumdispatcher ();
	// TODO currently excluded, libfa doesn't seem to work this way and for check/validation we can do it directly
	// test_validationdispatcher ();

	print_result ("testmod_regexdispatcher");

	return nbError;
}
