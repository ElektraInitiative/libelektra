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


void test (const char * value, int ret, const char * rangeString)
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

int main (int argc, char ** argv)
{
	printf ("RANGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test ("5", 1, "1-10");
	test ("10", 1, "1-10");
	test ("1", 1, "1-10");
	test ("0", -1, "1-10");
	test ("-5", -1, "1-10");
	test ("5", -1, "1-4");

	test ("2", 1, "-1-10");
	test ("-2", 1, "-3--1");

	test ("5", 1, " 1  - 10");
	test ("10", 1, " 1 -  10");
	test ("1", 1, " 1   - 10");
	test ("0", -1, "1- 10");
	test ("-5", -1, "1 -10");
	test ("5", -1, " 1 - 4 ");

	test ("2", 1, " - 1 - 10");
	test ("-2", 1, "-3 --1");

	test ("-2", 1, "-3 --  1");
	test ("-2", 1, "-3 - -  1");
	test ("-2", 1, "-3--  1");

	test ("5", -1, "0-4,6-9");
	test ("3", 1, "0-4,6-9");
	test ("7", 1, "0-4,6-9");
	test ("0", 1, "0-4,6-9");
	test ("4", 1, "0-4,6-9");
	test ("6", 1, "0-4,6-9");
	test ("9", 1, "0-4,6-9");


	printf ("\ntestmod_range RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
