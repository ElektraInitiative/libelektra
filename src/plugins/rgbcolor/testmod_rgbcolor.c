/**
 * @file
 *
 * @brief Tests for rgbcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbconfig.h>
#include <kdbtypes.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

// static void check_key(Key * key) {
// 	kdb_octet_t colorBytes[4];
// 	keyGetBinary(key, colorBytes, 4);
// }

static void test_color (const char * color, const int expected_ret)
{
	Key * parentKey = keyNew ("user/tests/rgbcolor", KEY_END);
	Key * hexkey = keyNew ("user/test/rgbcolor/testcolor", KEY_VALUE, color, KEY_META, "check/rgbcolor", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, KS_END);

	ksAppendKey (ks, hexkey);

	PLUGIN_OPEN ("rgbcolor");

	int ret = plugin->kdbSet (plugin, ks, parentKey);

	printf ("Test Color %s, returned value: %d, expected value: %d\n", color, ret, expected_ret);
	succeed_if (ret == expected_ret, "failed");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_rgbcolor (void)
{
	test_color ("#0fb", 1);
	test_color ("#fff", 1);
	test_color ("#111", 1);

	test_color ("#fff111", 1);
	test_color ("#a1C2bF", 1);

	test_color ("#abcd", 1);

	test_color ("#aabbccdd", 1);
	test_color ("#ffffffff", 1);
	test_color ("#00000000", 1);

	test_color ("1110", -1);
	test_color ("a1C2bF", -1);
	test_color ("#a1C2bZ", -1);
	test_color ("#00000", -1);
}

// static void test_expansion (void)
// {
// 	PLUGIN_OPEN ("rgbcolor");

// 	int ret = plugin->kdbSet (plugin, ks, parentKey);
// }


int main (int argc, char ** argv)
{
	init (argc, argv);

	test_rgbcolor ();

	print_result ("testmod_rgbcolor");

	return nbError;
}
