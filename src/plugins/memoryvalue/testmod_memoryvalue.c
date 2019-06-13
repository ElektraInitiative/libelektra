/**
 * @file
 * @brief Tests the memory value plugin
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbconfig.h>
#include <kdbtypes.h>
#include <stdlib.h>
#include <string.h>
#include <tests_plugin.h>

static void test_memoryvalue_normalization (const char * memoryvaluestring, const char * memoryvalueexpected)
{
	Key * parentKey = keyNew ("user/tests/memoryvalue", KEY_END);
	Key * hexkey =
		keyNew ("user/test/memoryvalue/testmemval", KEY_VALUE, memoryvaluestring, KEY_META, "check/memoryvalue", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, hexkey, KS_END);

	PLUGIN_OPEN ("memoryvalue");

	succeed_if ((plugin->kdbGet (plugin, ks, parentKey) >= 1), "kdbGet did not succeed");
	Key * foundKey = ksLookupByName (ks, "user/test/memoryvalue/testmemval", 0);
	succeed_if (!strcmp (keyString (foundKey), memoryvalueexpected), "Values dont match");
	printf ("test memoryvalue plugin normalization test - returned value: %s, expected value: %s\n", memoryvalueexpected,
		keyString (foundKey));

	succeed_if ((plugin->kdbSet (plugin, ks, parentKey) >= 1), "kdbSet did not succeed");
	foundKey = ksLookupByName (ks, "user/test/memoryvalue/testmemval", 0);
	succeed_if (!strcmp (keyString (foundKey), memoryvaluestring), "Values dont match");
	printf ("test memoryvalue plugin restoration test - returned value: %s, expected value: %s\n", memoryvaluestring,
		keyString (foundKey));

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_memoryvalue_normalization_error_expected (const char * memoryvaluestring, const char * memoryvalueexpected)
{
	Key * parentKey = keyNew ("user/tests/memoryvalue", KEY_END);
	Key * hexkey =
		keyNew ("user/test/memoryvalue/testmemval", KEY_VALUE, memoryvaluestring, KEY_META, "check/memoryvalue", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, hexkey, KS_END);

	PLUGIN_OPEN ("memoryvalue");
	printf ("Testing usage of false value %s\n", memoryvaluestring);
	int statusCode = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (statusCode < 1, "kdbGet did succeed, despite it should not\n");
	printf ("kdbGet status code %s expected , result: %d\n", memoryvalueexpected, statusCode);

	statusCode = plugin->kdbSet (plugin, ks, parentKey);
	succeed_if (statusCode < 1, "kdbSet did succeed, despite it should not\n");
	printf ("kdbSet status code %s expected, result: %d\n", memoryvalueexpected, statusCode);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_memory_value_validation (const char * memoryvalue, const short e_ret)
{
	Key * parentKey = keyNew ("user/tests/memoryvalue", KEY_END);
	Key * hexkey = keyNew ("user/test/memoryvalue/testvalue", KEY_VALUE, memoryvalue, KEY_META, "check/memoryvalue", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, hexkey, KS_END);

	PLUGIN_OPEN ("memoryvalue");

	int ret = plugin->kdbSet (plugin, ks, parentKey);

	printf ("Test MemoryValue Validity %s, returned value: %d, expected value: %d\n", memoryvalue, ret, e_ret);
	succeed_if (ret == e_ret, "Test failed");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_memoryvalueplugin (void)
{

	test_memory_value_validation ("1Z", -1);
	test_memory_value_validation ("PSOB", -1);
	test_memory_value_validation ("123YMB", -1);
	test_memory_value_validation ("123YGB", -1);
	test_memory_value_validation ("1B", 1);
	test_memory_value_validation ("10 B", 1);

	test_memoryvalue_normalization_error_expected ("MB10GB", "-1");
	test_memoryvalue_normalization_error_expected ("X10B", "-1");


	test_memoryvalue_normalization_error_expected ("1B1B", "-1");
	test_memoryvalue_normalization_error_expected ("1BB", "-1");
	test_memoryvalue_normalization_error_expected ("10GB20MB", "-1");
	test_memoryvalue_normalization_error_expected ("MB10", "-1");

	test_memoryvalue_normalization ("10B", "10");
	test_memoryvalue_normalization ("10MB", "10000000");
	test_memoryvalue_normalization ("10 MB", "10000000");
	test_memoryvalue_normalization ("90 MB", "90000000");
	test_memoryvalue_normalization ("256 MB", "256000000");
	test_memoryvalue_normalization ("10GB", "10000000000");
	test_memoryvalue_normalization ("5TB", "5000000000000");
	test_memoryvalue_normalization ("10PB", "10000000000000000");
	test_memoryvalue_normalization ("100PB", "100000000000000000");
}

int main (int argc, char ** argv)
{
	(void) argc;
	(void) argv;
	test_memoryvalueplugin ();
	print_result ("testmod_memoryvalue");
	return nbError;
}
