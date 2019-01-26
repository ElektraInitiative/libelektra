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


static void test_plugin_open_without_script (void)
{
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/simple.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_script_not_found (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/does_not_eXiSt.rb"), KEY_END), KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("", KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (keyString (keyGetMeta (errorKey, "warnings/#00/description")), "ruby warning");

	const char * exp_warning_msg = "Ruby Exception: LoadError: cannot load such file -- ";
	succeed_if (strncmp (keyString (keyGetMeta (errorKey, "warnings/#00/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	keyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_invalid_script (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/invalid.rb"), KEY_END), KS_END);

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("", KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (keyString (keyGetMeta (errorKey, "warnings/#00/description")), "ruby warning");

	const char * exp_warning_msg = "Error in Ruby-plugin, didn't call Kdb::Plugin.define";
	succeed_if (strncmp (keyString (keyGetMeta (errorKey, "warnings/#00/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	keyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_plugin_open_not_a_script (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/not_a_ruby_script.txt"), KEY_END), KS_END);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("", KEY_END);
	Plugin * plugin = elektraPluginOpen (PLUGIN_NAME, modules, conf, errorKey);

	succeed_if_same_string (keyString (keyGetMeta (errorKey, "warnings/#00/description")), "ruby warning");

	const char * exp_warning_msg = "Ruby Exception: SyntaxError:";
	succeed_if (strncmp (keyString (keyGetMeta (errorKey, "warnings/#00/reason")), exp_warning_msg, strlen (exp_warning_msg)) == 0,
		    "unexpected warning message");

	keyDel (errorKey);
	PLUGIN_CLOSE ();
}

static void test_simple_get (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/simple_get.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	Key * parentKey = keyNew ("user/rubytest", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 0, "call to kdbGet was not successful");

	succeed_if (ksGetSize (ks) == 5, "unexpected key set size");

	Key * head = ksHead (ks);
	Key * tail = ksTail (ks);
	succeed_if_same_string (keyString (head), "myvalue0");
	succeed_if_same_string (keyString (tail), "myvalue4");

	ksDel (ks);

	PLUGIN_CLOSE ();
}

static void test_get_with_exception (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/get_with_exception.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	Key * parentKey = keyNew ("user/rubytest", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) < 0, "call to kdbGet was successful but it should not");

	const char * exp_error_msg = "Ruby Exception: RuntimeError: Throwing that expected exception";
	succeed_if (strncmp (keyString (keyGetMeta (parentKey, "error/reason")), exp_error_msg, strlen (exp_error_msg)) == 0,
		    "unexpected error message");

	succeed_if (ksGetSize (ks) == 5, "unexpected key set size");

	Key * head = ksHead (ks);
	Key * tail = ksTail (ks);
	succeed_if_same_string (keyString (head), "myvalue0");
	succeed_if_same_string (keyString (tail), "myvalue4");

	ksDel (ks);

	PLUGIN_CLOSE ();
}


static void test_simple_set (void)
{
	KeySet * conf = ksNew (1, keyNew ("user/script", KEY_VALUE, srcdir_file ("tests/simple_set.rb"), KEY_END), KS_END);
	PLUGIN_OPEN (PLUGIN_NAME);

	Key * parentKey = keyNew ("user/rubytest", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user/rubytest/key1", KEY_VALUE, "myvalue1", KEY_END),
			     keyNew ("user/rubytest/key2", KEY_VALUE, "myvalue2", KEY_END),
			     keyNew ("user/rubytest/key3", KEY_VALUE, "myvalue3", KEY_END),
			     keyNew ("user/rubytest/key4", KEY_VALUE, "myvalue4", KEY_END),
			     keyNew ("user/rubytest/key5", KEY_VALUE, "myvalue5", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 5, "call to kdbSet was not successful");

	ksDel (ks);

	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("RUBY         TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

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


	print_result ("test_ruby");

	return nbError;
}
