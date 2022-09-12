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

	ElektraKey * parentKey = keyNew ("user:/tests/process", ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	// We can safely assume dump exists as without it the processplugin wouldn't work
	// We don't test any other plugin here since we can't make any guarantees about their existence
	ElektraKey * pluginKey = keyNew ("/plugin", ELEKTRA_KEY_VALUE, "dump", ELEKTRA_KEY_END);
	ksAppendKey (conf, pluginKey);
	PLUGIN_OPEN ("process");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	ElektraKey * contractKey = keyNew ("system:/elektra/modules/process", ELEKTRA_KEY_END);
	ElektraKeyset * contractSet = ksNew (0, ELEKTRA_KS_END);

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

	ElektraKey * parentKey = keyNew ("user:/tests/process", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	// No plugin key, should fail
	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = keyNew ("/", ELEKTRA_KEY_END);
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

	ElektraKey * parentKey = keyNew ("user:/tests/process", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * pluginKey = keyNew ("/plugin", ELEKTRA_KEY_VALUE, "non_existent_plugin", ELEKTRA_KEY_END);
	ksAppendKey (conf, pluginKey);

	// Non-existent plugin, should fail
	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	ElektraKey * errorKey = keyNew ("/", ELEKTRA_KEY_END);
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
