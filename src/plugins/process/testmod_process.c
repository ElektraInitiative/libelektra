/**
 * @file
 *
 * @brief Tests for process plugin
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

	const char * tmpFile = elektraFilename ();

	Key * parentKey = keyNew ("user:/tests/process", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	// We can safely assume dump exists as without it the processplugin wouldn't work
	// We don't test any other plugin here since we can't make any guarantees about their existence
	Key * pluginKey = keyNew ("/plugin", KEY_VALUE, "dump", KEY_END);
	ksAppendKey (conf, pluginKey);
	PLUGIN_OPEN ("process");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	Key * contractKey = keyNew ("system:/elektra/modules/process", KEY_END);
	KeySet * contractSet = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, contractSet, contractKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "call to kdbGet for the contract was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbError was not successful");
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	keyDel (contractKey);
	ksDel (contractSet);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
	elektraUnlink (tmpFile);
}

static void test_no_plugin_key (void)
{
	printf ("test no plugin key \n");

	Key * parentKey = keyNew ("user:/tests/process", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	// No plugin key, should fail
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * plugin = elektraPluginOpen ("process", modules, conf, errorKey);
	succeed_if (!output_warnings (errorKey), "no warnings in kdbOpen for plugin process");
	succeed_if (output_error (errorKey), "error in kdbOpen for plugin process");

	keyDel (errorKey);
	keyDel (parentKey);
	succeed_if (plugin != 0, "could not open process plugin");
	PLUGIN_CLOSE ();
}

static void test_invalid_plugin_key (void)
{
	printf ("test invalid plugin key\n");

	Key * parentKey = keyNew ("user:/tests/process", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Key * pluginKey = keyNew ("/plugin", KEY_VALUE, "non_existent_plugin", KEY_END);
	ksAppendKey (conf, pluginKey);

	// Non-existent plugin, should fail
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * plugin = elektraPluginOpen ("process", modules, conf, errorKey);
	succeed_if (!output_warnings (errorKey), "no warnings in kdbOpen for plugin process");
	succeed_if (!output_error (errorKey), "no error in kdbOpen for plugin process");

	keyDel (errorKey);
	keyDel (parentKey);
	succeed_if (plugin == 0, "could open process plugin");
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("PROCESS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_no_plugin_key ();
	test_invalid_plugin_key ();

	print_result ("testmod_process");

	return nbError;
}
