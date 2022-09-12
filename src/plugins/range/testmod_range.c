/**
 * @file
 *
 * @brief Tests for range plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbconfig.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>


void testInt (const char * value, int ret, const char * rangeString)
{
	ElektraKey * parentKey = keyNew ("user:/tests/range", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (10, keyNew ("user:/tests/range/key", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_META, "check/range", rangeString, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	//	fprintf (stderr, "testing: value: %s, expected: %d, got: %d,  range: %s\n", value, ret, rc, rangeString);
	succeed_if (rc == ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testUInt (const char * value, int ret, const char * rangeString)
{
	ElektraKey * parentKey = keyNew ("user:/tests/range", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (10,
			     keyNew ("user:/tests/range/key", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_META, "check/range", rangeString, ELEKTRA_KEY_META,
				     "check/type", "unsigned long", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	//	fprintf (stderr, "testing: value: %s, expected: %d, got: %d,  range: %s\n", value, ret, rc, rangeString);
	succeed_if (rc == ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testFloat (const char * value, int ret, const char * rangeString)
{
	ElektraKey * parentKey = keyNew ("user:/tests/range", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (10,
			     keyNew ("user:/tests/range/key", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_META, "check/range", rangeString, ELEKTRA_KEY_META,
				     "check/type", "float", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	//	fprintf (stderr, "testing: value: %s, expected: %d, got: %d,  range: %s\n", value, ret, rc, rangeString);
	succeed_if (rc == ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testHex (const char * value, int ret, const char * rangeString)
{
	ElektraKey * parentKey = keyNew ("user:/tests/range", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (10,
			     keyNew ("user:/tests/range/key", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_META, "check/range", rangeString, ELEKTRA_KEY_META,
				     "check/type", "HEX", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	//	fprintf (stderr, "testing: value: %s, expected: %d, got: %d, range: %s\n", value, ret, rc, rangeString);
	succeed_if (rc == ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

void testChar (const char * value, int ret, const char * rangeString)
{
	ElektraKey * parentKey = keyNew ("user:/tests/range", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (10,
			     keyNew ("user:/tests/range/key", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_META, "check/range", rangeString, ELEKTRA_KEY_META,
				     "check/type", "char", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("range");
	ksRewind (ks);
	int rc = plugin->kdbSet (plugin, ks, parentKey);
	//	fprintf (stderr, "testing: value: %s, expected: %d, got: %d, range: %s\n", value, ret, rc, rangeString);
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

	char * old_locale = elektraStrDup (setlocale (LC_ALL, NULL));
	setlocale (LC_ALL, "C");

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

	testInt ("-2", -1, "--3--1");
	testInt ("-2", -1, "-3---1");

	testInt ("5", -1, "0-4,6-9");
	testInt ("3", 1, "0-4,6-9");
	testInt ("7", 1, "0-4,6-9");
	testInt ("0", 1, "0-4,6-9");
	testInt ("4", 1, "0-4,6-9");
	testInt ("6", 1, "0-4,6-9");
	testInt ("9", 1, "0-4,6-9");

	testInt ("0", 1, "0,1-3");
	testInt ("4", 1, "2,3,4,5");
	testInt ("6", -1, "1,2,3,4");
	testInt ("9", 1, "0-7,8,9");

	testInt ("-2", -1, "-31");
	testInt ("-6", 1, "1,2,3,4,5,-6");
	testInt ("-9", -1, "0-7,-8,9");

	testUInt ("4", 1, "1-10");
	testUInt ("-5", -1, "1-10");
	testUInt ("3", 1, "1-4");

	testUInt ("2", -1, "-1-10");
	testUInt ("-2", -1, "-3--1");


	testFloat ("0.7", 1, "0.1-0.9");
	testFloat ("0.7", -1, "0.1-0.5");

	testFloat ("0.7", 1, "-0.8-0.9");


	testHex ("0A", 1, "00-20");
	testHex ("1A", -1, "0A-10");
	testHex ("1A", -1, "00-19,1B-20");
	testHex ("0A", 1, "00-19,1B-20");
	testHex ("1F", 1, "00-19,1B-20");


	testChar ("g", -1, "a-f");
	testChar ("c", 1, "a-f");

	// test edge cases
	char number[256];
	char range[256];
	snprintf (number, 256, "%lld", LLONG_MAX);
	snprintf (range, 256, "%lld - %lld", LLONG_MIN, LLONG_MAX);
	testInt (number, 1, range);
	snprintf (number, 256, "%lld", LLONG_MIN);
	testInt (number, 1, range);

	snprintf (number, 256, "%llu", ULLONG_MAX);
	snprintf (range, 256, "%llu - %llu", 1ULL, ULLONG_MAX);
	testUInt (number, 1, range);
	snprintf (number, 256, "%llu", 1ULL);
	testUInt (number, 1, range);

	setlocale (LC_ALL, old_locale);
	elektraFree (old_locale);
	print_result ("testmod_range");

	return nbError;
}
