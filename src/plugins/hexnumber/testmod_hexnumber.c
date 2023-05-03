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

#include <internal/config.h>

#include <tests_plugin.h>

#include "./hexnumber.h"

#define CREATE_TEST_KEY(HEX) (keyNew ("user:/tests/hexnumber/" #HEX, KEY_VALUE, #HEX, KEY_META, "type", "long", KEY_END))
#define CREATE_TEST_KEY_UNITBASE(HEX) (keyNew ("user:/tests/hexnumber/" #HEX, KEY_VALUE, #HEX, KEY_META, "unit/base", "hex", KEY_END))
#define CREATE_TEST_KEY_CUSTOM(HEX, TYPE) (keyNew ("user:/tests/hexnumber/" #HEX, KEY_VALUE, #HEX, KEY_META, "type", TYPE, KEY_END))
#define CHECK_TEST_KEY(HEX, DEC) succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/hexnumber/" #HEX, 0)), #DEC)


static void test_basics (void)
{
	Key * parentKey = keyNew ("user:/tests/hexnumber", KEY_END);
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
	Key * parentKey = keyNew ("user:/tests/hexnumber", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("hexnumber");
	KeySet * ks = ksNew (30, CREATE_TEST_KEY (0xF), CREATE_TEST_KEY (0xf), CREATE_TEST_KEY (0x14), CREATE_TEST_KEY (0xFFFFFFFFFFFFFFFF),
			     CREATE_TEST_KEY (0x0), CREATE_TEST_KEY (0x2), CREATE_TEST_KEY (-0x2), CREATE_TEST_KEY (-0x1),
			     CREATE_TEST_KEY (test), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	CHECK_TEST_KEY (0xF, 15);
	CHECK_TEST_KEY (0xf, 15);
	CHECK_TEST_KEY (0x14, 20);
	CHECK_TEST_KEY (0xFFFFFFFFFFFFFFFF, 18446744073709551615);
	CHECK_TEST_KEY (0x0, 0);
	CHECK_TEST_KEY (0x2, 2);
	CHECK_TEST_KEY (-0x2, -0x2);
	CHECK_TEST_KEY (-0x1, -0x1);
	CHECK_TEST_KEY (test, test);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_customint (void)
{
	Key * parentKey = keyNew ("user:/tests/hexnumber", KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/accept/type/#0", KEY_VALUE, "customint", KEY_END),
			       keyNew ("system:/accept/type/#1", KEY_VALUE, "othercustomint", KEY_END), KS_END);
	PLUGIN_OPEN ("hexnumber");
	KeySet * ks = ksNew (30, CREATE_TEST_KEY_CUSTOM (0x1F, "customint"), CREATE_TEST_KEY_CUSTOM (0xFF, "othercustomint"),
			     CREATE_TEST_KEY_CUSTOM (0x22, "string"), CREATE_TEST_KEY_CUSTOM (0x11, "long"), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	CHECK_TEST_KEY (0x1F, 31);
	CHECK_TEST_KEY (0xFF, 255);
	CHECK_TEST_KEY (0x22, 0x22);
	CHECK_TEST_KEY (0x11, 17);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_unitbase (void)
{
	Key * parentKey = keyNew ("user:/tests/hexnumber", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("hexnumber");
	KeySet * ks = ksNew (30, CREATE_TEST_KEY_UNITBASE (0x1F), CREATE_TEST_KEY_UNITBASE (0xFF), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	CHECK_TEST_KEY (0x1F, 31);
	CHECK_TEST_KEY (0xFF, 255);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_force (void)
{
	Key * parentKey = keyNew ("user:/tests/hexnumber", KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/integertypes", KEY_VALUE, "customint;othercustomint", KEY_END),
			       keyNew ("system:/force", KEY_VALUE, "1", KEY_END), KS_END);
	PLUGIN_OPEN ("hexnumber");
	KeySet * ks = ksNew (30, CREATE_TEST_KEY_CUSTOM (0x1F, "customint"), CREATE_TEST_KEY_CUSTOM (0xFF, "othercustomint"),
			     CREATE_TEST_KEY_CUSTOM (0x22, "string"), KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	CHECK_TEST_KEY (0x1F, 31);
	CHECK_TEST_KEY (0xFF, 255);
	CHECK_TEST_KEY (0x22, 34);

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
	test_customint ();
	test_unitbase ();
	test_force ();

	print_result ("testmod_hexnumber");

	return nbError;
}
