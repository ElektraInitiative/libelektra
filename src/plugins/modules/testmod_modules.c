/**
 * @file
 *
 * @brief Tests for modules plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("system:/elektra/modules", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("modules");

	KeySet * expected =
		ksNew (4, keyNew ("/foo", KEY_END), keyNew ("/boo", KEY_VALUE, "123", KEY_END), keyNew ("/bar", KEY_VALUE, "abc", KEY_END),
		       keyNew ("/baz", KEY_VALUE, "xyz", KEY_META, "meta", "test", KEY_END), KS_END);

	KeySet * definition = ksDeepDup (expected);
	succeed_if (plugin->kdbInit (plugin, definition, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbInit failed");
	ksDel (definition);

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	compare_keyset (expected, ks);


	keyDel (parentKey);
	ksDel (ks);
	ksDel (expected);
	PLUGIN_CLOSE ();
}

static void test_wrongParent (void)
{
	printf ("test wrong parent\n");

	Key * parentKey = keyNew ("user:/tests/modules", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("modules");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "should not accept wrong parent");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("MODULES     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_wrongParent ();

	print_result ("testmod_modules");

	return nbError;
}
