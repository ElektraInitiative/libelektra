/**
 * @file
 *
 * @brief Tests for hexcolor plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_color (const char * color, const int expected_ret)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("user/tests/hexcolor", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (10, keyNew ("user/test/hexcolor/testcolor", KEY_VALUE, color, KEY_META, "check/hexcolor", KEY_END), KS_END);

	PLUGIN_OPEN ("hexcolor");

	int ret = plugin->kdbSet (plugin, ks, parentKey);

	printf("Return value: %d, expected: %d" + expected_ret, ret, expected_ret);
	succeed_if (ret == expected_ret, "");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_hexcolor(void) 
{
	test_color ("#fff", 1);
    test_color ("#fff", 1);
	test_color ("#0fb", 1);
	test_color ("#111", 1);

	test_color ("#1110", 0);
	test_color ("1110", 0);
}


int main (int argc, char ** argv)
{
	printf ("HEXCOLOR     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_hexcolor ();

	print_result ("testmod_hexcolor");

	return nbError;
}
