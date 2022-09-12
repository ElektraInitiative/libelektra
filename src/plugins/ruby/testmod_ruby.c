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

#define PLUGIN_NAME "ruby"

#define SCRIPTS_DIR "ruby_test_scripts/"


static void test_plugin_open_without_script (void)
{
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_script_not_found (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "does_not_eXiSt.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (errorKey, "warnings/#0/description")), "Plugin Misbehavior");

	const char * exp_warning_msg = "Ruby Exception: LoadError: cannot load such file -- ";
	succeed_if (strncmp (elektraKeyString (elektraKeyGetMeta (errorKey, "warnings/#0/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	elektraKeyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_invalid_script (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "invalid.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (errorKey, "warnings/#0/description")), "Plugin Misbehavior");

	const char * exp_warning_msg = "Error in Ruby-plugin, didn't call Kdb::Plugin.define";
	succeed_if (strncmp (elektraKeyString (elektraKeyGetMeta (errorKey, "warnings/#0/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	elektraKeyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_not_a_script (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "not_a_ruby_script.txt"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (errorKey, "warnings/#0/description")), "Plugin Misbehavior");

	const char * exp_warning_msg = "Ruby Exception: SyntaxError:";
	succeed_if (strncmp (elektraKeyString (elektraKeyGetMeta (errorKey, "warnings/#0/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	elektraKeyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_simple_get (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple_get.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKey * parentKey = elektraKeyNew ("user:/rubytest", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	succeed_if (elektraKeysetGetSize (ks) == 5, "unexpected key set size");

	ElektraKey * head = elektraKeysetHead (ks);
	ElektraKey * tail = elektraKeysetTail (ks);
	succeed_if_same_string (elektraKeyString (head), "myvalue0");
	succeed_if_same_string (elektraKeyString (tail), "myvalue4");

	elektraKeysetDel (ks);

	PLUGIN_CLOSE ();
}

static void test_get_with_exception (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "get_with_exception.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKey * parentKey = elektraKeyNew ("user:/rubytest", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) < 0, "call to kdbGet was successful but it should not");

	const char * exp_error_msg = "Ruby Exception: RuntimeError: Throwing that expected exception";
	succeed_if (strncmp (elektraKeyString (elektraKeyGetMeta (parentKey, "error/reason")), exp_error_msg, strlen (exp_error_msg)) == 0,
		    "unexpected error message");

	succeed_if (elektraKeysetGetSize (ks) == 5, "unexpected key set size");

	ElektraKey * head = elektraKeysetHead (ks);
	ElektraKey * tail = elektraKeysetTail (ks);
	succeed_if_same_string (elektraKeyString (head), "myvalue0");
	succeed_if_same_string (elektraKeyString (tail), "myvalue4");

	elektraKeysetDel (ks);

	PLUGIN_CLOSE ();
}


static void test_simple_set (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple_set.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKey * parentKey = elektraKeyNew ("user:/rubytest", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue1", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue2", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue3", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue4", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue5", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 5, "call to kdbSet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	elektraKeysetDel (ks);

	PLUGIN_CLOSE ();
}

static void set_and_test_state (Plugin * plugin, ElektraKeyset * ksSet, ElektraKeyset * ksGet)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/rubytest", ELEKTRA_KEY_END);
	succeed_if (plugin->kdbSet (plugin, ksSet, parentKey) == 1, "call to kdbSet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	succeed_if (plugin->kdbGet (plugin, ksGet, parentKey) == 0, "call to kdbGet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);
	elektraKeyDel (parentKey);
}

static void test_statefull (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "statefull.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKeyset * ksSet = elektraKeysetNew (5, elektraKeyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue1", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue2", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue3", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue4", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue5", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ksGet = elektraKeysetNew (0, ELEKTRA_KS_END);

	set_and_test_state (plugin, ksSet, ksGet);

	succeed_if (elektraKeysetGetSize (ksGet) == 5, "unexpected key set size");

	ElektraKey * head = elektraKeysetHead (ksGet);
	ElektraKey * tail = elektraKeysetTail (ksGet);
	succeed_if_same_string (elektraKeyString (head), "myvalue1");
	succeed_if_same_string (elektraKeyString (tail), "myvalue5");

	elektraKeysetDel (ksSet);
	elektraKeysetDel (ksGet);

	PLUGIN_CLOSE ();
}


static void test_two_plugin_instances (void)
{
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "statefull.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin1 = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey1);

	succeed_if (plugin1 != NULL, "could not open plugin instance 1");

	ElektraKey * errorKey2 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin2 = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey2);

	succeed_if (plugin2 != NULL, "could not open plugin instance 1");

	// Set and test state for plugin1
	ElektraKeyset * ksSet1 = elektraKeysetNew (5, elektraKeyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue1", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue2", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue3", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue4", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue5", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ksGet1 = elektraKeysetNew (0, ELEKTRA_KS_END);

	set_and_test_state (plugin1, ksSet1, ksGet1);

	succeed_if (elektraKeysetGetSize (ksGet1) == 5, "unexpected key set size");

	ElektraKey * head1 = elektraKeysetHead (ksGet1);
	ElektraKey * tail1 = elektraKeysetTail (ksGet1);
	succeed_if_same_string (elektraKeyString (head1), "myvalue1");
	succeed_if_same_string (elektraKeyString (tail1), "myvalue5");

	// Set and test state for plugin2
	ElektraKeyset * ksSet2 = elektraKeysetNew (5, elektraKeyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue_1", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue_2", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue_3", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue_4", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue_5", ELEKTRA_KEY_END),
				 elektraKeyNew ("user:/rubytest/key6", ELEKTRA_KEY_VALUE, "myvalue_6", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ksGet2 = elektraKeysetNew (0, ELEKTRA_KS_END);

	set_and_test_state (plugin2, ksSet2, ksGet2);

	succeed_if (elektraKeysetGetSize (ksGet2) == 6, "unexpected key set size");

	ElektraKey * head2 = elektraKeysetHead (ksGet2);
	ElektraKey * tail2 = elektraKeysetTail (ksGet2);
	succeed_if_same_string (elektraKeyString (head2), "myvalue_1");
	succeed_if_same_string (elektraKeyString (tail2), "myvalue_6");

	// clean up
	elektraKeysetDel (ksSet1);
	elektraKeysetDel (ksGet1);
	elektraKeysetDel (ksSet2);
	elektraKeysetDel (ksGet2);

	elektraKeyDel (errorKey1);
	elektraKeyDel (errorKey2);
	elektraPluginClose (plugin1, 0);
	// this results in a segfault
	// elektraPluginClose (plugin2, 0);
	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
}

int main (int argc, char ** argv)
{
	printf ("RUBY         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	// some debugging
	char * ld_library_path = getenv ("LD_LIBRARY_PATH");
	char * rubylib = getenv ("RUBYLIB");

	printf ("env LD_LIBRARY_PATH: %s\n", ld_library_path);
	printf ("env RUBYLIB: %s\n", rubylib);

	// Plugin open
	//
	test_plugin_open_without_script ();

	test_plugin_open_script_not_found ();
	test_plugin_open_not_a_script ();
	test_plugin_open_invalid_script ();

	test_plugin_open ();

	// Plugin get
	//
	test_simple_get ();
	test_get_with_exception ();

	// Plugin set
	//
	test_simple_set ();

	test_statefull ();

	test_two_plugin_instances ();

	print_result ("test_ruby");

	return nbError;
}
