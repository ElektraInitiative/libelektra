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

static void test_memoryvalueplugin ();

int main (int argc, char ** argv)
{

	test_memoryvalueplugin ();
	print_result ("testmod_memoryvalue");
	return nbError;
}

static void test_memoryvalue (const char * memoryvalue, const int expected_ret)
{
	Key * hexkey =
		keyNew ("user/test/memoryvalue/testmemoryvalue", KEY_VALUE, memoryvalue, KEY_META, "check/memoryvalue", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, KS_END);
	Key * parentKey = keyNew ("user/tests/memoryvalue", KEY_END);

	ksAppendKey (ks, hexkey);
	PLUGIN_OPEN ("memoryvalue");
	int ret = plugin->kdbSet (plugin, ks, parentKey);
	printf ("Test Memoryvalue %s, returned value: %d, expected value: %d\n", memoryvalue, ret, expected_ret);
	succeed_if (ret == expected_ret, "failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_memoryvalueplugin ()
{
	test_memoryvalue ("1Z", -1);
	test_memoryvalue ("!TB", -1);
	test_memoryvalue ("A MB", -1);
	test_memoryvalue ("PSOB", -1);
	test_memoryvalue ("123YMB", -1);
	test_memoryvalue ("123YGB", -1);
	test_memoryvalue ("10B", 10);
	test_memoryvalue ("10MB", 1000000);
	test_memoryvalue ("10 MB", 1000000);
	test_memoryvalue ("10GB", 1000000000);
}
