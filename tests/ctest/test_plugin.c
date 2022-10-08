/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

KeySet * set_pluginconf (void)
{
	return ksNew (10, keyNew ("system:/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system:/more", KEY_END),
		      keyNew ("system:/more/config", KEY_END), keyNew ("system:/more/config/below", KEY_END),
		      keyNew ("system:/path", KEY_END), keyNew ("user:/anything", KEY_VALUE, "plugin", KEY_END),
		      keyNew ("user:/more", KEY_END), keyNew ("user:/more/config", KEY_END), keyNew ("user:/more/config/below", KEY_END),
		      keyNew ("user:/path", KEY_END), KS_END);
}

static void test_simple (void)
{
	printf ("Test plugin\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Plugin * plugin = elektraPluginOpen (KDB_DEFAULT_STORAGE, modules, set_pluginconf (), 0);
	exit_if_fail (plugin, "KDB_DEFAULT_STORAGE: " KDB_DEFAULT_STORAGE " plugin could not be loaded");

	KeySet * test_config = set_pluginconf ();
	KeySet * config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

static void test_name (void)
{
	printf ("Test name\n");
	KeySet * modules = ksNew (0, KS_END);
	Key * errorKey = keyNew ("/", KEY_END);
	;

	succeed_if (elektraPluginOpen (0, modules, set_pluginconf (), errorKey) == 0, "should fail with no name");
	succeed_if (elektraPluginOpen ("", modules, set_pluginconf (), errorKey) == 0, "should fail with no name");
	succeed_if (elektraPluginOpen ("/", modules, set_pluginconf (), errorKey) == 0, "should fail with slashes only");
	succeed_if (elektraPluginOpen ("//", modules, set_pluginconf (), errorKey) == 0, "should fail with slashes only");
	succeed_if (elektraPluginOpen ("/////////////", modules, set_pluginconf (), errorKey) == 0, "should fail with slashes only");
	// output_errors (errorKey);

	keyDel (errorKey);
	ksDel (modules);
}

int main (int argc, char ** argv)
{
	printf (" PLUGINS  TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_simple ();
	test_name ();

	printf ("\ntest_plugin RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
