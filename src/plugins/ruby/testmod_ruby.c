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
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_script_not_found (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "does_not_eXiSt.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = keyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (keyString (keyGetMeta (errorKey, "warnings/#0/description")), "Plugin Misbehavior");

	const char * exp_warning_msg = "Ruby Exception: LoadError: cannot load such file -- ";
	succeed_if (strncmp (keyString (keyGetMeta (errorKey, "warnings/#0/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	keyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_invalid_script (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "invalid.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = keyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (keyString (keyGetMeta (errorKey, "warnings/#0/description")), "Plugin Misbehavior");

	const char * exp_warning_msg = "Error in Ruby-plugin, didn't call Kdb::Plugin.define";
	succeed_if (strncmp (keyString (keyGetMeta (errorKey, "warnings/#0/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	keyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_not_a_script (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "not_a_ruby_script.txt"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = keyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (keyString (keyGetMeta (errorKey, "warnings/#0/description")), "Plugin Misbehavior");

	const char * exp_warning_msg = "Ruby Exception: SyntaxError:";
	succeed_if (strncmp (keyString (keyGetMeta (errorKey, "warnings/#0/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	keyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_simple_get (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple_get.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKey * parentKey = keyNew ("user:/rubytest", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	succeed_if (ksGetSize (ks) == 5, "unexpected key set size");

	ElektraKey * head = ksHead (ks);
	ElektraKey * tail = ksTail (ks);
	succeed_if_same_string (keyString (head), "myvalue0");
	succeed_if_same_string (keyString (tail), "myvalue4");

	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void test_get_with_exception (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "get_with_exception.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKey * parentKey = keyNew ("user:/rubytest", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) < 0, "call to kdbGet was successful but it should not");

	const char * exp_error_msg = "Ruby Exception: RuntimeError: Throwing that expected exception";
	succeed_if (strncmp (keyString (keyGetMeta (parentKey, "error/reason")), exp_error_msg, strlen (exp_error_msg)) == 0,
		    "unexpected error message");

	succeed_if (ksGetSize (ks) == 5, "unexpected key set size");

	ElektraKey * head = ksHead (ks);
	ElektraKey * tail = ksTail (ks);
	succeed_if_same_string (keyString (head), "myvalue0");
	succeed_if_same_string (keyString (tail), "myvalue4");

	ksDel (ks);

	PLUGIN_CLOSE ();
}


static void test_simple_set (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple_set.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKey * parentKey = keyNew ("user:/rubytest", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (5, keyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue1", ELEKTRA_KEY_END),
			     keyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue2", ELEKTRA_KEY_END),
			     keyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue3", ELEKTRA_KEY_END),
			     keyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue4", ELEKTRA_KEY_END),
			     keyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue5", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 5, "call to kdbSet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void set_and_test_state (Plugin * plugin, ElektraKeyset * ksSet, ElektraKeyset * ksGet)
{
	ElektraKey * parentKey = keyNew ("user:/rubytest", ELEKTRA_KEY_END);
	succeed_if (plugin->kdbSet (plugin, ksSet, parentKey) == 1, "call to kdbSet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	succeed_if (plugin->kdbGet (plugin, ksGet, parentKey) == 0, "call to kdbGet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);
	keyDel (parentKey);
}

static void test_statefull (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "statefull.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	ElektraKeyset * ksSet = ksNew (5, keyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue1", ELEKTRA_KEY_END),
				keyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue2", ELEKTRA_KEY_END),
				keyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue3", ELEKTRA_KEY_END),
				keyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue4", ELEKTRA_KEY_END),
				keyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue5", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ksGet = ksNew (0, ELEKTRA_KS_END);

	set_and_test_state (plugin, ksSet, ksGet);

	succeed_if (ksGetSize (ksGet) == 5, "unexpected key set size");

	ElektraKey * head = ksHead (ksGet);
	ElektraKey * tail = ksTail (ksGet);
	succeed_if_same_string (keyString (head), "myvalue1");
	succeed_if_same_string (keyString (tail), "myvalue5");

	ksDel (ksSet);
	ksDel (ksGet);

	PLUGIN_CLOSE ();
}


static void test_two_plugin_instances (void)
{
	ElektraKeyset * conf = ksNew (1, keyNew ("user:/script", ELEKTRA_KEY_VALUE, srcdir_file (SCRIPTS_DIR "statefull.rb"), ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey1 = keyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin1 = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey1);

	succeed_if (plugin1 != NULL, "could not open plugin instance 1");

	ElektraKey * errorKey2 = keyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin2 = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey2);

	succeed_if (plugin2 != NULL, "could not open plugin instance 1");

	// Set and test state for plugin1
	ElektraKeyset * ksSet1 = ksNew (5, keyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue1", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue2", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue3", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue4", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue5", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ksGet1 = ksNew (0, ELEKTRA_KS_END);

	set_and_test_state (plugin1, ksSet1, ksGet1);

	succeed_if (ksGetSize (ksGet1) == 5, "unexpected key set size");

	ElektraKey * head1 = ksHead (ksGet1);
	ElektraKey * tail1 = ksTail (ksGet1);
	succeed_if_same_string (keyString (head1), "myvalue1");
	succeed_if_same_string (keyString (tail1), "myvalue5");

	// Set and test state for plugin2
	ElektraKeyset * ksSet2 = ksNew (5, keyNew ("user:/rubytest/key1", ELEKTRA_KEY_VALUE, "myvalue_1", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key2", ELEKTRA_KEY_VALUE, "myvalue_2", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key3", ELEKTRA_KEY_VALUE, "myvalue_3", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key4", ELEKTRA_KEY_VALUE, "myvalue_4", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key5", ELEKTRA_KEY_VALUE, "myvalue_5", ELEKTRA_KEY_END),
				 keyNew ("user:/rubytest/key6", ELEKTRA_KEY_VALUE, "myvalue_6", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ksGet2 = ksNew (0, ELEKTRA_KS_END);

	set_and_test_state (plugin2, ksSet2, ksGet2);

	succeed_if (ksGetSize (ksGet2) == 6, "unexpected key set size");

	ElektraKey * head2 = ksHead (ksGet2);
	ElektraKey * tail2 = ksTail (ksGet2);
	succeed_if_same_string (keyString (head2), "myvalue_1");
	succeed_if_same_string (keyString (tail2), "myvalue_6");

	// clean up
	ksDel (ksSet1);
	ksDel (ksGet1);
	ksDel (ksSet2);
	ksDel (ksGet2);

	keyDel (errorKey1);
	keyDel (errorKey2);
	elektraPluginClose (plugin1, 0);
	// this results in a segfault
	// elektraPluginClose (plugin2, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
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
