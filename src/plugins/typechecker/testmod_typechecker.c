/**
 * @file
 *
 * @brief Tests for the typechecker plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#define LONG_REGEX                                                                                                                         \
	"-[1-9]|-214748364[0-8]|-?[1-9][0-9]|-?[1-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][" \
	"0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9]" \
	"[0-9][0-9]|-?1[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?21[0-3][0-9][0-9][0-"  \
	"9][0-9][0-9][0-9][0-9]|-?214[0-6][0-9][0-9][0-9][0-9][0-9][0-9]|-?2147[0-3][0-9][0-9][0-9][0-9][0-9]|-?21474[0-7][0-9][0-9][0-9]" \
	"[0-9]|-?214748[0-2][0-9][0-9][0-9]|-?2147483[0-5][0-9][0-9]|-?21474836[0-3][0-9]|[0-9]|214748364[0-7]"
#define PARENT_KEY_NAME "user/tests/typechecker"
#define KEY1_NAME PARENT_KEY_NAME "/key1"
#define KEY2_NAME PARENT_KEY_NAME "/key2"
#define KEY3_NAME PARENT_KEY_NAME "/key3"
#define KEY4_NAME PARENT_KEY_NAME "/key4"

static KeySet * keysetWithStandardFunctions (void)
{
	KeySet * ks = ksNew (0, KS_END);

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/fallback/#", KEY_META, "elektra/spec/order", "10", KEY_META,
				 "elektra/spec/impl", "fallback a b = link a b", KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/override/#", KEY_META, "elektra/spec/order", "10", KEY_META,
				 "elektra/spec/impl", "override a b = link a b", KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/check/validation", KEY_META, "elektra/spec/impl",
				 "checkvalidation a b = intersect a b", KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/check/long", KEY_META, "elektra/spec/impl",
				 "checklong a = intersect a (Key :: Key \"" LONG_REGEX "\")", KEY_END));

	return ks;
}

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew (PARENT_KEY_NAME, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("typechecker");

	KeySet * ks = keysetWithStandardFunctions ();

	ksAppendKey (ks, keyNew (KEY1_NAME, KEY_VALUE, "2500", KEY_META, "check/validation",
				 "[0-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-4][0-9][0-9][0-9]|5000", KEY_END));

	ksAppendKey (ks,
		     keyNew (KEY2_NAME, KEY_VALUE, "500", KEY_META, "fallback/#1", KEY1_NAME, KEY_META, "check/validation",
			     "-[1-9]|-1[0-9][0-9]|-2[0-4][0-9]|-250|-?[1-9][0-9]|[0-9]|[1-9][0-9][0-9]|[1-6][0-9][0-9][0-9]|7[0-4][0-9][0-"
			     "9]|7500",
			     KEY_END));

	ksAppendKey (ks, keyNew (KEY3_NAME, KEY_META, "fallback/#1", KEY1_NAME, KEY_META, "fallback/#2", KEY2_NAME, KEY_END));


	ksAppendKey (ks, keyNew (KEY4_NAME, KEY_META, "fallback/#1", KEY1_NAME, KEY_META, "override/#1", KEY2_NAME, KEY_META, "check/long",
				 "", KEY_END));

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error(s) found but none expected");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_invalid_ranges_override (void)
{
	printf ("test invalid ranges fallback\n");

	Key * parentKey = keyNew (PARENT_KEY_NAME, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("typechecker");

	KeySet * ks = keysetWithStandardFunctions ();

	ksAppendKey (ks, keyNew (KEY1_NAME, KEY_VALUE, "2500", KEY_META, "check/validation",
				 "[0-9]|[1-9][0-9]|[1-9][0-9][0-9]|[1-4][0-9][0-9][0-9]|5000", KEY_END));

	ksAppendKey (ks, keyNew (KEY2_NAME, KEY_VALUE, "500", KEY_META, "override/#1", KEY1_NAME, KEY_META, "check/validation",
				 "[7-9][2-9][0-9][0-9]|10000", KEY_END));

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbSet was not successful");
	succeed_if (!output_error (parentKey), "no errors found but one is expected");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("TYPECHECKER  TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_invalid_ranges_override ();

	print_result ("testmod_typechecker");

	return nbError;
}
