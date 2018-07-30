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
	const Key * checkRange1 = keyGetMeta (pKey1, "check/validation");

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
	const Key * checkEnum1 = keyGetMeta (pKey1, "check/validation");

	succeed_if (checkEnum1, "the single enum regex hasn't been generated");
	succeed_if (0 == strcmp (keyString (checkEnum1), "a|b"), "the single enum regex is invalid");

	const Key * pKey2 = ksLookupByName (ks, "/key2", KDB_O_NONE);
	const Key * checkEnum2 = keyGetMeta (pKey2, "check/validation");

	succeed_if (checkEnum2, "the multi enum regex hasn't been generated");
	succeed_if (0 == strcmp (keyString (checkEnum2), "(a,b)|(b,a)"), "the mutli enum regex is invalid");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_defaultdispatcher (void)
{
	printf ("test_defaultdispatcher\n");

	Key * parentKey = keyNew ("user/tests/regexdispatcher", KEY_END);
	KeySet * conf = ksNew (0, KS_END);

	PLUGIN_OPEN ("regexdispatcher");

	KeySet * ks = ksNew (1, KS_END);
	Key * key = keyNew ("/key", KEY_META, "default", ".\\+*?[^]$(){}=!<>|:-asfdjklö123", KEY_END);
	ksAppendKey (ks, key);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

	const Key * pKey = ksLookupByName (ks, "/key", KDB_O_NONE);
	const Key * defaultValue = keyGetMeta (pKey, "defaultValue");

	succeed_if (defaultValue, "the default value regex hasn't been generated");
	succeed_if (0 == strcmp (keyString (defaultValue), "\\.\\\\\\+\\*\\?\\[\\^\\]\\$\\(\\)\\{\\}\\=\\!\\<\\>\\|\\:\\-asfdjklö123"), "the default value regex is invalid");

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
	test_defaultdispatcher ();

	print_result ("testmod_regexdispatcher");

	return nbError;
}
