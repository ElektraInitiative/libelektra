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

static void test_basics (void)
{
	printf ("test_basics\n");

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


int main (int argc, char ** argv)
{
	printf ("REGEXDISPATCHER     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_regexdispatcher");

	return nbError;
}
