/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdlib.h>

#ifdef HAVE_KDBCONFIG_H
#include <internal/config.h>
#endif

#include <tests_plugin.h>

#define PLUGIN_NAME "ruby"

#define SCRIPTS_DIR "ruby_test_scripts/"


static void test_plugin_open_without_script (void)
{
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open (void)
{
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_script_not_found (void)
{
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "does_not_eXiSt.rb"), KEY_END), KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("/", KEY_END);
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
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "invalid.rb"), KEY_END), KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("/", KEY_END);
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
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "not_a_ruby_script.txt"), KEY_END), KS_END);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("/", KEY_END);
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
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple_get.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	Key * parentKey = keyNew ("user:/rubytest", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	succeed_if (ksGetSize (ks) == 5, "unexpected key set size");

	Key * head = ksAtCursor (ks, 0);
	Key * tail = ksAtCursor (ks, ksGetSize (ks) - 1);

	succeed_if_same_string (keyString (head), "myvalue0");
	succeed_if_same_string (keyString (tail), "myvalue4");

	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void test_get_with_exception (void)
{
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "get_with_exception.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	Key * parentKey = keyNew ("user:/rubytest", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) < 0, "call to kdbGet was successful but it should not");

	const char * exp_error_msg = "Ruby Exception: RuntimeError: Throwing that expected exception";
	succeed_if (strncmp (keyString (keyGetMeta (parentKey, "error/reason")), exp_error_msg, strlen (exp_error_msg)) == 0,
		    "unexpected error message");

	succeed_if (ksGetSize (ks) == 5, "unexpected key set size");

	Key * head = ksAtCursor (ks, 0);
	Key * tail = ksAtCursor (ks, ksGetSize (ks) - 1);
	succeed_if_same_string (keyString (head), "myvalue0");
	succeed_if_same_string (keyString (tail), "myvalue4");

	ksDel (ks);

	PLUGIN_CLOSE ();
}


static void test_simple_set (void)
{
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "simple_set.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	Key * parentKey = keyNew ("user:/rubytest", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user:/rubytest/key1", KEY_VALUE, "myvalue1", KEY_END),
			     keyNew ("user:/rubytest/key2", KEY_VALUE, "myvalue2", KEY_END),
			     keyNew ("user:/rubytest/key3", KEY_VALUE, "myvalue3", KEY_END),
			     keyNew ("user:/rubytest/key4", KEY_VALUE, "myvalue4", KEY_END),
			     keyNew ("user:/rubytest/key5", KEY_VALUE, "myvalue5", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 5, "call to kdbSet was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void set_and_test_state (Plugin * plugin, KeySet * ksSet, KeySet * ksGet)
{
	Key * parentKey = keyNew ("user:/rubytest", KEY_END);
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
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "statefull.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	KeySet * ksSet = ksNew (5, keyNew ("user:/rubytest/key1", KEY_VALUE, "myvalue1", KEY_END),
				keyNew ("user:/rubytest/key2", KEY_VALUE, "myvalue2", KEY_END),
				keyNew ("user:/rubytest/key3", KEY_VALUE, "myvalue3", KEY_END),
				keyNew ("user:/rubytest/key4", KEY_VALUE, "myvalue4", KEY_END),
				keyNew ("user:/rubytest/key5", KEY_VALUE, "myvalue5", KEY_END), KS_END);

	KeySet * ksGet = ksNew (0, KS_END);

	set_and_test_state (plugin, ksSet, ksGet);

	succeed_if (ksGetSize (ksGet) == 5, "unexpected key set size");

	Key * head = ksAtCursor (ksGet, 0);
	Key * tail = ksAtCursor (ksGet, ksGetSize (ksGet) - 1);
	succeed_if_same_string (keyString (head), "myvalue1");
	succeed_if_same_string (keyString (tail), "myvalue5");

	ksDel (ksSet);
	ksDel (ksGet);

	PLUGIN_CLOSE ();
}


static void test_two_plugin_instances (void)
{
	KeySet * conf = ksNew (1, keyNew ("user:/script", KEY_VALUE, srcdir_file (SCRIPTS_DIR "statefull.rb"), KEY_END), KS_END);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey1 = keyNew ("/", KEY_END);
	Plugin * plugin1 = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey1);

	succeed_if (plugin1 != NULL, "could not open plugin instance 1");

	Key * errorKey2 = keyNew ("/", KEY_END);
	Plugin * plugin2 = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey2);

	succeed_if (plugin2 != NULL, "could not open plugin instance 1");

	// Set and test state for plugin1
	KeySet * ksSet1 = ksNew (5, keyNew ("user:/rubytest/key1", KEY_VALUE, "myvalue1", KEY_END),
				 keyNew ("user:/rubytest/key2", KEY_VALUE, "myvalue2", KEY_END),
				 keyNew ("user:/rubytest/key3", KEY_VALUE, "myvalue3", KEY_END),
				 keyNew ("user:/rubytest/key4", KEY_VALUE, "myvalue4", KEY_END),
				 keyNew ("user:/rubytest/key5", KEY_VALUE, "myvalue5", KEY_END), KS_END);

	KeySet * ksGet1 = ksNew (0, KS_END);

	set_and_test_state (plugin1, ksSet1, ksGet1);

	succeed_if (ksGetSize (ksGet1) == 5, "unexpected key set size");

	Key * head1 = ksAtCursor (ksGet1, 0);
	Key * tail1 = ksAtCursor (ksGet1, ksGetSize (ksGet1) - 1);

	succeed_if_same_string (keyString (head1), "myvalue1");
	succeed_if_same_string (keyString (tail1), "myvalue5");

	// Set and test state for plugin2
	KeySet * ksSet2 = ksNew (5, keyNew ("user:/rubytest/key1", KEY_VALUE, "myvalue_1", KEY_END),
				 keyNew ("user:/rubytest/key2", KEY_VALUE, "myvalue_2", KEY_END),
				 keyNew ("user:/rubytest/key3", KEY_VALUE, "myvalue_3", KEY_END),
				 keyNew ("user:/rubytest/key4", KEY_VALUE, "myvalue_4", KEY_END),
				 keyNew ("user:/rubytest/key5", KEY_VALUE, "myvalue_5", KEY_END),
				 keyNew ("user:/rubytest/key6", KEY_VALUE, "myvalue_6", KEY_END), KS_END);

	KeySet * ksGet2 = ksNew (0, KS_END);

	set_and_test_state (plugin2, ksSet2, ksGet2);

	succeed_if (ksGetSize (ksGet2) == 6, "unexpected key set size");

	Key * head2 = ksAtCursor (ksGet2, 0);
	Key * tail2 = ksAtCursor (ksGet2, ksGetSize (ksGet2) - 1);
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
