/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_process (void)
{
//	printf ("Test processing of plugin name\n");
//
//	Key * k = keyNew ("system/elektra/#0name", KEY_END);
//	int pluginNumber = -1;
//	char * pluginName = 0;
//	char * referenceName = 0;
//
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 1, "process plugin error");
//	succeed_if (pluginNumber == 0, "number not correct");
//	succeed_if_same_string (pluginName, "name");
//	succeed_if (referenceName == 0, "reference name not correct");
//	elektraFree (pluginName);
//	pluginName = 0;
//
//	keySetName (k, "system/e/#2dump");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 1, "process plugin error");
//	succeed_if (pluginNumber == 2, "number not correct");
//	succeed_if_same_string (pluginName, "dump");
//	succeed_if (referenceName == 0, "reference name not correct");
//	elektraFree (pluginName);
//	pluginName = 0;
//
//	keySetName (k, "system/e/#9default");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 1, "process plugin error");
//	succeed_if (pluginNumber == 9, "number not correct");
//	succeed_if_same_string (pluginName, "default");
//	succeed_if (referenceName == 0, "reference name not correct");
//	elektraFree (pluginName);
//	pluginName = 0;
//
//	keySetName (k, "system/e/1default");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == -1, "should be error");
//
//	keySetName (k, "system/e/#xdefault");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == -1, "should be error");
//
//	keySetName (k, "system/e/#1#name");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 2, "process plugin error");
//	succeed_if (pluginNumber == 1, "number not correct");
//	succeed_if (pluginName == 0, "plugin name not correct");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/name");
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keySetName (k, "system/e/#5#dump");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 2, "process plugin error");
//	succeed_if (pluginNumber == 5, "number not correct");
//	succeed_if (pluginName == 0, "plugin name not correct");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/dump");
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keySetName (k, "system/e/#0#very_long_name with space");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 2, "process plugin error");
//	succeed_if (pluginNumber == 0, "number not correct");
//	succeed_if (pluginName == 0, "plugin name not correct");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/very_long_name with space");
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keySetName (k, "system/e/#1#plugname#refname#");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 3, "process plugin error");
//	succeed_if (pluginNumber == 1, "number not correct");
//	succeed_if_same_string (pluginName, "plugname");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/refname");
//	elektraFree (pluginName);
//	pluginName = 0;
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keySetName (k, "system/e/#0#dump#dumpy#");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 3, "process plugin error");
//	succeed_if (pluginNumber == 0, "number not correct");
//	succeed_if_same_string (pluginName, "dump");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/dumpy");
//	elektraFree (pluginName);
//	pluginName = 0;
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keySetName (k, "system/e/#9#default#default#");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 3, "process plugin error");
//	succeed_if (pluginNumber == 9, "number not correct");
//	succeed_if_same_string (pluginName, "default");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/default");
//	elektraFree (pluginName);
//	pluginName = 0;
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keySetName (k, "system/e/#8#a_very long name with $ sthg#also a long name_()#");
//	succeed_if (elektraProcessPlugin (k, &pluginNumber, &pluginName, &referenceName, 0) == 3, "process plugin error");
//	succeed_if (pluginNumber == 8, "number not correct");
//	succeed_if_same_string (pluginName, "a_very long name with $ sthg");
//	succeed_if_same_string (referenceName, "system/elektra/plugins/also a long name_()");
//	elektraFree (pluginName);
//	pluginName = 0;
//	elektraFree (referenceName);
//	referenceName = 0;
//
//	keyDel (k);
}

KeySet * set_pluginconf (void)
{
	return ksNew (10, keyNew ("system/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system/more", KEY_END),
		      keyNew ("system/more/config", KEY_END), keyNew ("system/more/config/below", KEY_END), keyNew ("system/path", KEY_END),
		      keyNew ("user/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user/more", KEY_END),
		      keyNew ("user/more/config", KEY_END), keyNew ("user/more/config/below", KEY_END), keyNew ("user/path", KEY_END),
		      KS_END);
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
	Key * errorKey = keyNew (0);
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

	test_process ();
	test_simple ();
	test_name ();

	printf ("\ntest_plugin RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
