/**
 * @file
 *
 * @brief Tests for range plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include <kdbconfig.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>


void testInt (const char * value, int ret, const char * rangeString)
{
	Key * parentKey = keyNew ("user/tests/range", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/range/key", KEY_VALUE, value, KEY_META, "check/range", rangeString, KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	fprintf (stderr, "testing: value: %s, expected: %d, got: %d,  range: %s\n", value, ret, rc, rangeString);
	succeed_if (rc == ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testFloat (const char * value, int ret, const char * rangeString)
{
	Key * parentKey = keyNew ("user/tests/range", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/range/key", KEY_VALUE, value, KEY_META, "check/range", rangeString, KEY_META,
					 "check/range/type", "FLOAT", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	fprintf (stderr, "testing: value: %s, expected: %d, got: %d,  range: %s\n", value, ret, rc, rangeString);
	succeed_if (rc == ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("RANGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testInt ("5", 1, "1-10");
	testInt ("10", 1, "1-10");
	testInt ("1", 1, "1-10");
	testInt ("0", -1, "1-10");
	testInt ("-5", -1, "1-10");
	testInt ("5", -1, "1-4");

	testInt ("2", 1, "-1-10");
	testInt ("-2", 1, "-3--1");

	testInt ("5", 1, " 1  - 10");
	testInt ("10", 1, " 1 -  10");
	testInt ("1", 1, " 1   - 10");
	testInt ("0", -1, "1- 10");
	testInt ("-5", -1, "1 -10");
	testInt ("5", -1, " 1 - 4 ");

	testInt ("2", 1, " - 1 - 10");
	testInt ("-2", 1, "-3 --1");

	testInt ("-2", 1, "-3 --  1");
	testInt ("-2", 1, "-3 - -  1");
	testInt ("-2", 1, "-3--  1");

	testInt ("-2", -1, "-31");
	testInt ("-2", -1, "--3--1");
	testInt ("-2", -1, "-3---1");

	testInt ("5", -1, "0-4,6-9");
	testInt ("3", 1, "0-4,6-9");
	testInt ("7", 1, "0-4,6-9");
	testInt ("0", 1, "0-4,6-9");
	testInt ("4", 1, "0-4,6-9");
	testInt ("6", 1, "0-4,6-9");
	testInt ("9", 1, "0-4,6-9");

	testFloat ("0.7", 1, "0.1-0.9");
	testFloat ("0.7", -1, "0.1-0.5");


	printf ("\ntestmod_range RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
