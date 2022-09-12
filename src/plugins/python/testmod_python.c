/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbmacros.h>
#include <stdlib.h>

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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

	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file ("python/python_plugin.py"), ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/shutdown", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("user:/print", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/python/path", ELEKTRA_KEY_VALUE, ".", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME));

	ElektraKey * parentKey = elektraKeyNew ("user:/from_c", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	exit_if_fail (elektraKeysetGetSize (ks) == 1, "keyset size is still 0");
	succeed_if_same_string (elektraKeyName (elektraKeysetHead (ks)), "user:/from_python");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

// test loading python twice
static void test_two_scripts (void)
{
	printf ("Testing loading of two active python plugins...\n");

	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);

	ElektraKeyset * conf = elektraKeysetNew (2, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file ("python/python_plugin.py"), ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/shutdown", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("user:/python/path", ELEKTRA_KEY_VALUE, ".", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/print", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * conf2 = elektraKeysetNew (2, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file ("python/python_plugin2.py"), ELEKTRA_KEY_END),
				elektraKeyNew ("user:/shutdown", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("user:/python/path", ELEKTRA_KEY_VALUE, ".", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/print", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME), modules, conf, errorKey);
	succeed_if (output_warnings (errorKey), "warnings in kdbOpen");
	succeed_if (output_error (errorKey), "errors in kdbOpen");
	exit_if_fail (plugin != NULL, "unable to load python plugin");
	elektraKeyDel (errorKey);

	ElektraKey * errorKey2 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin2 = elektraPluginOpen (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME), modules, conf2, errorKey2);
	succeed_if (output_warnings (errorKey2), "warnings in kdbOpen");
	succeed_if (output_error (errorKey2), "errors in kdbOpen");
	exit_if_fail (plugin2 != NULL, "unable to load python plugin again");
	elektraKeyDel (errorKey2);

	elektraPluginClose (plugin2, 0);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
}

// simple return value test
static void test_fail (void)
{
	printf ("Testing return values from python functions...\n");

	ElektraKeyset * conf = elektraKeysetNew (2, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file ("python/python_plugin_fail.py"), ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/shutdown", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("user:/python/path", ELEKTRA_KEY_VALUE, ".", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/print", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME));

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/from_c", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == -1, "call to kdbGet didn't fail");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == -1, "call to kdbSet didn't fail");
	succeed_if (plugin->kdbError (plugin, ks, parentKey) == -1, "call to kdbError didn't fail");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

// test script with wrong class name
static void test_wrong (void)
{
	printf ("Testing python script with wrong class name...\n");

	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);

	ElektraKeyset * conf = elektraKeysetNew (2, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file ("python/python_plugin_wrong.py"), ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/shutdown", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("user:/python/path", ELEKTRA_KEY_VALUE, ".", ELEKTRA_KEY_END),
			       elektraKeyNew ("user:/print", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (ELEKTRA_STRINGIFY (PYTHON_PLUGIN_NAME), modules, conf, errorKey);
	succeed_if (!output_warnings (errorKey), "we expect some warnings");
	succeed_if (!output_error (errorKey), "we expect some errors");
	succeed_if (plugin == NULL, "python plugin shouldn't be loadable");
	elektraKeyDel (errorKey);

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
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
