/**
 * @file
 *
 * @brief Tests for hexnumber plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#define ELEKTRA_HEXNUMBER_META_TYPE "hexnumber"

#define CREATE_TEST_KEY(HEX)                                                                                                               \
        (keyNew ("user/tests/hexnumber/" #HEX, KEY_VALUE, #HEX, KEY_META, "type", ELEKTRA_HEXNUMBER_META_TYPE, KEY_END))
#define CHECK_TEST_KEY(HEX, DEC)                                                                                                           \
                succeed_if_same_string (keyString (ksLookupByName (ks, "user/tests/hexnumber/" #HEX, 0)), #DEC)\


static void test_basics (void)
{
	Key * parentKey = keyNew ("user/tests/hexnumber", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("hexnumber");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


static void test_default (void)
{
	Key * parentKey = keyNew ("user/tests/hexnumber", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("hexnumber");
	KeySet * ks = ksNew (30, CREATE_TEST_KEY (0xF), CREATE_TEST_KEY (0xf), CREATE_TEST_KEY (0x14), CREATE_TEST_KEY (0xFFFFFFFFFFFFFFFF),
			     CREATE_TEST_KEY (0x0), CREATE_TEST_KEY (0x2), CREATE_TEST_KEY (-0x2), CREATE_TEST_KEY (-0x1), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	CHECK_TEST_KEY (0xF, 15);
	CHECK_TEST_KEY (0xf, 15);
	CHECK_TEST_KEY (0x14, 20);
	CHECK_TEST_KEY (0xFFFFFFFFFFFFFFFF, 18446744073709551615);
	CHECK_TEST_KEY (0x0, 0);
	CHECK_TEST_KEY (0x2, 2);
	CHECK_TEST_KEY (-0x2, 18446744073709551614);
	CHECK_TEST_KEY (-0x1, 18446744073709551615);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("HEXNUMBER    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_default ();

	print_result ("testmod_hexnumber");

	return nbError;
}
