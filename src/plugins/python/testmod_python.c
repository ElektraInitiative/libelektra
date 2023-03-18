/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/macros/utils.h>
#include <stdlib.h>

#ifdef HAVE_KDBCONFIG_H
#include <internal/kdb/config.h>
#endif

#include <tests_plugin.h>

static void init_env (void)
{
	setenv ("PYTHONDONTWRITEBYTECODE", "1", 1);
}

// test simple variable passing
static void test_variable_passing (void)
{
	printf ("Testing simple variable passing...\n");

	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file ("python/python_plugin.py"), KEY_END),
			       keyNew ("user:/shutdown", KEY_VALUE, "1", KEY_END), keyNew ("user:/print", KEY_END),
			       keyNew ("user:/python/path", KEY_VALUE, ".", KEY_END), KS_END);
	PLUGIN_OPEN (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME));

	Key * parentKey = keyNew ("user:/from_c", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	exit_if_fail (ksGetSize (ks) == 1, "keyset size is still 0");
	succeed_if_same_string (keyName (ksAtCursor (ks, 0)), "user:/from_python");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

// test loading python twice
static void test_two_scripts (void)
{
	printf ("Testing loading of two active python plugins...\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * conf = ksNew (2, keyNew ("user:/script", KEY_VALUE, srcdir_file ("python/python_plugin.py"), KEY_END),
			       keyNew ("user:/shutdown", KEY_VALUE, "1", KEY_END), keyNew ("user:/python/path", KEY_VALUE, ".", KEY_END),
			       keyNew ("user:/print", KEY_END), KS_END);

	KeySet * conf2 = ksNew (2, keyNew ("user:/script", KEY_VALUE, srcdir_file ("python/python_plugin2.py"), KEY_END),
				keyNew ("user:/shutdown", KEY_VALUE, "1", KEY_END), keyNew ("user:/python/path", KEY_VALUE, ".", KEY_END),
				keyNew ("user:/print", KEY_END), KS_END);

	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * plugin = elektraPluginOpen (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME), modules, conf, errorKey);
	succeed_if (output_warnings (errorKey), "warnings in kdbOpen");
	succeed_if (output_error (errorKey), "errors in kdbOpen");
	exit_if_fail (plugin != NULL, "unable to load python plugin");
	keyDel (errorKey);

	Key * errorKey2 = keyNew ("/", KEY_END);
	Plugin * plugin2 = elektraPluginOpen (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME), modules, conf2, errorKey2);
	succeed_if (output_warnings (errorKey2), "warnings in kdbOpen");
	succeed_if (output_error (errorKey2), "errors in kdbOpen");
	exit_if_fail (plugin2 != NULL, "unable to load python plugin again");
	keyDel (errorKey2);

	elektraPluginClose (plugin2, 0);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

// simple return value test
static void test_fail (void)
{
	printf ("Testing return values from python functions...\n");

	KeySet * conf = ksNew (2, keyNew ("user:/script", KEY_VALUE, srcdir_file ("python/python_plugin_fail.py"), KEY_END),
			       keyNew ("user:/shutdown", KEY_VALUE, "1", KEY_END), keyNew ("user:/python/path", KEY_VALUE, ".", KEY_END),
			       keyNew ("user:/print", KEY_END), KS_END);
	PLUGIN_OPEN (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME));

	Key * parentKey = keyNew ("user:/tests/from_c", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "call to kdbGet didn't fail");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "call to kdbSet didn't fail");
	succeed_if (plugin->kdbError (plugin, ks, parentKey) == -1, "call to kdbError didn't fail");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

// test script with wrong class name
static void test_wrong (void)
{
	printf ("Testing python script with wrong class name...\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * conf = ksNew (2, keyNew ("user:/script", KEY_VALUE, srcdir_file ("python/python_plugin_wrong.py"), KEY_END),
			       keyNew ("user:/shutdown", KEY_VALUE, "1", KEY_END), keyNew ("user:/python/path", KEY_VALUE, ".", KEY_END),
			       keyNew ("user:/print", KEY_END), KS_END);

	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * plugin = elektraPluginOpen (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME), modules, conf, errorKey);
	succeed_if (!output_warnings (errorKey), "we expect some warnings");
	succeed_if (!output_error (errorKey), "we expect some errors");
	succeed_if (plugin == NULL, "python plugin shouldn't be loadable");
	keyDel (errorKey);

	elektraModulesClose (modules, 0);
	ksDel (modules);
}

int main (int argc, char ** argv)
{
	printf ("PYTHON      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	init_env ();

	test_variable_passing ();
	test_two_scripts ();

	printf ("\n");
	printf ("========================================================================\n");
	printf ("NOTE: The following errors are intended. We're testing error conditions!\n");
	printf ("========================================================================\n");
	test_fail ();
	test_wrong ();

	print_result ("test_python");

	return nbError;
}
