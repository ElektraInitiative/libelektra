/**
 * @file
 *
 * @brief Tests for rgbcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbconfig.h>
#include <elektra/kdbtypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <tests_plugin.h>

static void test_normalize_color (const char * color, kdb_unsigned_long_t colorValue)
{
	Key * parentKey = keyNew ("user:/tests/rgbcolor", KEY_END);
	Key * hexkey = keyNew ("user:/test/rgbcolor/testcolor", KEY_VALUE, color, KEY_META, "check/rgbcolor", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, KS_END);
	// KeySet * ksGet = ks;

	ksAppendKey (ks, hexkey);

	PLUGIN_OPEN ("rgbcolor");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "kdbSet did not succeed");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "kdbGet did not succeed");

	Key * foundKey = ksLookupByName (ks, "user:/test/rgbcolor/testcolor", 0);

	char colorStr[11];
	snprintf (colorStr, 11, "%u", colorValue);

	printf ("Test Color Normalization %s, returned value: %s, expected value: %s\n", color, keyString (foundKey), colorStr);
	succeed_if (!strcmp (keyString (foundKey), colorStr), "Values dont match");

	const Key * origValueKey = keyGetMeta (foundKey, "origvalue");
	succeed_if (origValueKey != NULL, "origvalue is not set");
	succeed_if (!strcmp (keyString (origValueKey), color), "origvalue does not match actual original value");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


static void test_color (const char * color, const int expected_ret)
{
	Key * parentKey = keyNew ("user:/tests/rgbcolor", KEY_END);
	Key * hexkey = keyNew ("user:/test/rgbcolor/testcolor", KEY_VALUE, color, KEY_META, "check/rgbcolor", "any", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (20, KS_END);

	ksAppendKey (ks, hexkey);

	PLUGIN_OPEN ("rgbcolor");

	int ret = plugin->kdbSet (plugin, ks, parentKey);

	printf ("Test Color Validity %s, returned value: %d, expected value: %d\n", color, ret, expected_ret);
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

	test_color ("aliceblue", 1);
	test_color ("orange", 1);
	test_color ("red", 1);

	test_color ("nonexistentcolor", -1);
	test_color ("1110", -1);
	test_color ("a1C2bF", -1);
	test_color ("#a1C2bZ", -1);
	test_color ("#00000", -1);

	test_normalize_color ("lightcyan", 0xe0ffffff);
	test_normalize_color ("white", 0xffffffff);
	test_normalize_color ("black", 0xff);
	test_normalize_color ("#fff", 0xffffffff);
	test_normalize_color ("#ffff", 0xffffffff);
	test_normalize_color ("#ffffff", 0xffffffff);
	test_normalize_color ("#ffffffff", 0xffffffff);

	test_normalize_color ("#aabbccdd", 0xaabbccdd);
	test_normalize_color ("#abcd", 0xaabbccdd);

	test_normalize_color ("#abc", 0xaabbccff);
	test_normalize_color ("#aabbcc", 0xaabbccff);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_rgbcolor ();

	print_result ("testmod_rgbcolor");

	return nbError;
}
