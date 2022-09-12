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

#include "hexnumber.h"

#define CREATE_TEST_KEY(HEX) (keyNew ("user:/tests/hexnumber/" #HEX, ELEKTRA_KEY_VALUE, #HEX, ELEKTRA_KEY_META, "type", "long", ELEKTRA_KEY_END))
#define CREATE_TEST_KEY_UNITBASE(HEX) (keyNew ("user:/tests/hexnumber/" #HEX, ELEKTRA_KEY_VALUE, #HEX, ELEKTRA_KEY_META, "unit/base", "hex", ELEKTRA_KEY_END))
#define CREATE_TEST_KEY_CUSTOM(HEX, TYPE) (keyNew ("user:/tests/hexnumber/" #HEX, ELEKTRA_KEY_VALUE, #HEX, ELEKTRA_KEY_META, "type", TYPE, ELEKTRA_KEY_END))
#define CHECK_TEST_KEY(HEX, DEC) succeed_if_same_string (keyString (ksLookupByName (ks, "user:/tests/hexnumber/" #HEX, 0)), #DEC)


static void test_basics (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/hexnumber", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("hexnumber");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


static void test_default (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/hexnumber", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("hexnumber");
	ElektraKeyset * ks = ksNew (30, CREATE_TEST_KEY (0xF), CREATE_TEST_KEY (0xf), CREATE_TEST_KEY (0x14), CREATE_TEST_KEY (0xFFFFFFFFFFFFFFFF),
			     CREATE_TEST_KEY (0x0), CREATE_TEST_KEY (0x2), CREATE_TEST_KEY (-0x2), CREATE_TEST_KEY (-0x1),
			     CREATE_TEST_KEY (test), ELEKTRA_KS_END);
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
	ElektraKey * parentKey = keyNew ("user:/tests/hexnumber", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (10, keyNew ("system:/accept/type/#0", ELEKTRA_KEY_VALUE, "customint", ELEKTRA_KEY_END),
			       keyNew ("system:/accept/type/#1", ELEKTRA_KEY_VALUE, "othercustomint", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("hexnumber");
	ElektraKeyset * ks = ksNew (30, CREATE_TEST_KEY_CUSTOM (0x1F, "customint"), CREATE_TEST_KEY_CUSTOM (0xFF, "othercustomint"),
			     CREATE_TEST_KEY_CUSTOM (0x22, "string"), CREATE_TEST_KEY_CUSTOM (0x11, "long"), ELEKTRA_KS_END);
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
	ElektraKey * parentKey = keyNew ("user:/tests/hexnumber", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("hexnumber");
	ElektraKeyset * ks = ksNew (30, CREATE_TEST_KEY_UNITBASE (0x1F), CREATE_TEST_KEY_UNITBASE (0xFF), ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	CHECK_TEST_KEY (0x1F, 31);
	CHECK_TEST_KEY (0xFF, 255);

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void test_force (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/hexnumber", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (10, keyNew ("system:/integertypes", ELEKTRA_KEY_VALUE, "customint;othercustomint", ELEKTRA_KEY_END),
			       keyNew ("system:/force", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("hexnumber");
	ElektraKeyset * ks = ksNew (30, CREATE_TEST_KEY_CUSTOM (0x1F, "customint"), CREATE_TEST_KEY_CUSTOM (0xFF, "othercustomint"),
			     CREATE_TEST_KEY_CUSTOM (0x22, "string"), ELEKTRA_KS_END);
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
