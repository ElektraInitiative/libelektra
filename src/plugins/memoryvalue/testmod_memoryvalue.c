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

// the boolean parameter is needed, because we need unsigned longs as expected values, signed longs are too short thus no way to check for
// invalid values with -1
static void test_memoryvalue (const char * memoryvaluestring, kdb_unsigned_long_long_t memoryvalueexpected, bool errorExpected)
{
	Key * parentKey = keyNew ("user/tests/memoryvalue", KEY_END);
	Key * hexkey =
		keyNew ("user/test/memoryvalue/testmemval", KEY_VALUE, memoryvaluestring, KEY_META, "check/memoryvalue", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, KS_END);
	// KeySet * ksGet = ks;

	ksAppendKey (ks, hexkey);

	PLUGIN_OPEN ("memoryvalue");

	succeed_if ((plugin->kdbSet (plugin, ks, parentKey) >= 1 || errorExpected), "kdbSet did not succeed");

	succeed_if ((plugin->kdbGet (plugin, ks, parentKey) >= 1 || errorExpected), "kdbGet did not succeed");

	Key * foundKey = ksLookupByName (ks, "user/test/memoryvalue/testmemval", 0);
	const int n = snprintf (NULL, 0, ELEKTRA_UNSIGNED_LONG_LONG_F, memoryvalueexpected);
	char memvalstring[n + 1];

	snprintf (memvalstring, 20, ELEKTRA_UNSIGNED_LONG_LONG_F, memoryvalueexpected);
	printf ("test memoryvalue plugin %s, returned value: %s, expected value: %s\n", memoryvaluestring, keyString (foundKey),
		errorExpected ? memoryvaluestring : memvalstring);
	// in case of errors the plugin return the initial value
	bool res = errorExpected && !strcmp (keyString (foundKey), memoryvaluestring);
	if (res)
	{
		printf ("error occurred while validating the value, this was expected\n");
	}
	succeed_if (!strcmp (keyString (foundKey), memvalstring) || res, "Values dont match");
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_memoryvalueplugin ()
{
	test_memoryvalue ("1Z", 0, true);
	test_memoryvalue ("!TB", 0, true);
	test_memoryvalue ("A MB", 0, true);
	test_memoryvalue ("PSOB", 0, true);
	test_memoryvalue ("123YMB", 0, true);
	test_memoryvalue ("123YGB", 0, true);
	test_memoryvalue ("1B", 1, false);
	test_memoryvalue ("1B1B", 1, true);
	test_memoryvalue ("1BB", 1, true);
	test_memoryvalue ("10GB20MB", 1, true);
	test_memoryvalue ("MB10", 1, true);
	test_memoryvalue ("MB10GB", 1, true);
	test_memoryvalue ("X10B", 10, true);
	test_memoryvalue ("10B", 10, false);
	test_memoryvalue ("10MB", 10000000, false);
	test_memoryvalue ("10 MB", 10000000, false);
	test_memoryvalue ("90 MB", 90000000, false);
	test_memoryvalue ("256 MB", 256000000, false);
	test_memoryvalue ("10GB", 10000000000, false);
	test_memoryvalue ("5TB", 5000000000000, false);
	test_memoryvalue ("10PB", 10000000000000000, false);
	test_memoryvalue ("100PB", 100000000000000000, false);
}

int main (int argc, char ** argv)
{
	(void) argc;
	(void) argv;
	test_memoryvalueplugin ();
	print_result ("testmod_memoryvalue");
	return nbError;
}
