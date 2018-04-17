/**
 * @file
 *
 * @brief Tests for haskelltemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

/*
#@META check/range = 0-5000
key1 = 2500

#@META check/range = 6000-7000
#@META fallback/#1 = key1
key2 = 6500

#@META comment = implicitly they are .* aka anything, how to treat fallback inference?
#@META fallback/#1 = key1
#@META fallback/#2 = key2
key3 = 

#@META elektra/spec/type = RegexContains b a => Key a -> Key b -> Key a
#@META elektra/spec/impl = fallback (Key Nothing) b = b \n fallback a _ = a
elektra/spec/fallback/# = 

#@META elektra/spec/type = RegexContains b a => Key b -> Key a -> Key b
#@META elektra/spec/impl = override (Key Nothing) b = b \n override a _ = a
elektra/spec/override/# = 

#@META spec/type = RegexIntersects a b => Key a -> Range b -> Key (RegexIntersection a b)
#@META spec/impl = checkRange a _ = a
elektra/spec/check/range = b

#@META elektra/spec/type = RegexIntersects a => Key a -> Key (RegexIntersection a ".")
#@META elektra/spec/impl = checkLong a = a
elektra/spec/check/long =
*/

#define LONG_REGEX "-[1-9]|-214748364[0-8]|-?[1-9][0-9]|-?[1-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?[1-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?1[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?20[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?21[0-3][0-9][0-9][0-9][0-9][0-9][0-9][0-9]|-?214[0-6][0-9][0-9][0-9][0-9][0-9][0-9]|-?2147[0-3][0-9][0-9][0-9][0-9][0-9]|-?21474[0-7][0-9][0-9][0-9][0-9]|-?214748[0-2][0-9][0-9][0-9]|-?2147483[0-5][0-9][0-9]|-?21474836[0-3][0-9]|[0-9]|214748364[0-7]"
#define PARENT_KEY_NAME "user/tests/typechecker"
#define KEY1_NAME PARENT_KEY_NAME "/key1"
#define KEY2_NAME PARENT_KEY_NAME "/key2"
#define KEY3_NAME PARENT_KEY_NAME "/key3"

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew (PARENT_KEY_NAME, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("typechecker");

	KeySet * ks = ksNew (0, KS_END);

	ksAppendKey (ks, keyNew (KEY1_NAME,
		KEY_VALUE, "2500",
		KEY_META, "check/range", "0-5000",
		KEY_END));

	ksAppendKey (ks, keyNew (KEY2_NAME,
		KEY_VALUE, "500",
		KEY_META, "fallback/#1", KEY1_NAME,
		KEY_META, "check/range", "-250-7500",
		KEY_END));

	ksAppendKey (ks, keyNew (KEY3_NAME,
		KEY_META, "fallback/#1", KEY1_NAME,
		KEY_META, "fallback/#2", KEY2_NAME,
		KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/fallback/#",
		KEY_META, "elektra/spec/type", "RegexContains b a => Key b :: . -> Key a -> Key a",
		KEY_META, "elektra/spec/impl", "fallback a (Key P.Nothing) = a \n fallback _ a = a",
		KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/override/#",
		KEY_META, "elektra/spec/type", "RegexContains b a => Key b :: . -> Key a -> Key a",
		KEY_META, "elektra/spec/impl", "override (Key P.Nothing) b = b \n override a _ = a",
		KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/check/range",
		KEY_META, "elektra/spec/type", "RegexIntersects a b => P.Proxy b :: Range . -> Key a -> Key (RegexIntersection a b)",
		KEY_META, "elektra/spec/impl", "checkrange _ a = a",
		KEY_END));

	ksAppendKey (ks, keyNew (PARENT_KEY_NAME "/elektra/spec/check/long",
		KEY_META, "elektra/spec/type", "RegexIntersects a \"" LONG_REGEX "\" => Key a -> Key (RegexIntersection a \"" LONG_REGEX "\")",
		KEY_META, "elektra/spec/impl", "checklong a = a",
		KEY_END));

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbError was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

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

	print_result ("testmod_typechecker");

	return nbError;
}
