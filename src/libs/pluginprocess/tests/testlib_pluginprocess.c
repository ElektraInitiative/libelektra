/**
 * @file
 *
 * @brief Tests for pluginprocess library.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <kdbpluginprocess.h>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbprivate.h>

#include <tests.h>

static int elektraDummyOpen (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
		elektraPluginSetData (handle, pp);
		// pass dummy plugin data over to the child
		int * testData = (int *) malloc (sizeof (int));
		*testData = 42;
		elektraPluginProcessSetData (handle, testData);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	keySetMeta (errorKey, "user/tests/pluginprocess/open", "");
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummyClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp))
	{
		int * testData = (int *) elektraPluginProcessGetData (handle);
		ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
		if (result.cleanedUp)
		{
			elektraPluginSetData (handle, NULL);
			free (testData);
		}
		return result.result;
	}

	keySetMeta (errorKey, "user/tests/pluginprocess/close", "");
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraDummyGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, returned, parentKey);

	int * testData = (int *) elektraPluginProcessGetData (handle);

	keySetMeta (parentKey, "user/tests/pluginprocess/get", "");
	// Just check if the child can access its actual plugin data
	if (*testData == 42) keySetMeta (parentKey, "user/tests/pluginprocess/testdata", "");
	ksAppendKey (returned, keyNew ("user/tests/pluginprocess/get", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraDummySet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);

	keySetMeta (parentKey, "user/tests/pluginprocess/set", "");
	ksAppendKey (returned, keyNew ("user/tests/pluginprocess/set", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraDummyError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_ERROR, returned, parentKey);

	keySetMeta (parentKey, "user/tests/pluginprocess/error", "");
	ksAppendKey (returned, keyNew ("user/tests/pluginprocess/error", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static Plugin * createDummyPlugin (KeySet * conf)
{
	Plugin * plugin = malloc (sizeof (struct _Plugin));
	plugin->config = conf;
	plugin->kdbOpen = &elektraDummyOpen;
	plugin->kdbClose = &elektraDummyClose;
	plugin->kdbGet = &elektraDummyGet;
	plugin->kdbSet = &elektraDummySet;
	plugin->kdbError = &elektraDummyError;
	plugin->name = "dummy";
	plugin->refcounter = 1;
	plugin->data = NULL;
	return plugin;
}

static void test_communication (void)
{
	printf ("test communication\n");

	Key * parentKey = keyNew ("user/tests/pluginprocess", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	succeed_if (elektraPluginGetData (plugin) != NULL, "didn't store the pluginprocess struct in the plugin's data");
	succeed_if (keyGetMeta (parentKey, "user/tests/pluginprocess/open") != NULL,
		    "child process didn't set the open metadata on the parent key");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if (keyGetMeta (parentKey, "user/tests/pluginprocess/set") != NULL,
		    "child process didn't set the set metadata on the parent key");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (keyGetMeta (parentKey, "user/tests/pluginprocess/get") != NULL,
		    "child process didn't set the get metadata on the parent key");
	succeed_if (keyGetMeta (parentKey, "user/tests/pluginprocess/testdata") != NULL,
		    "child process didn't receive the plugin data from the parent");
	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbError was not successful");
	succeed_if (keyGetMeta (parentKey, "user/tests/pluginprocess/error") != NULL,
		    "child process didn't set the error metadata on the parent key");
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
	succeed_if (keyGetMeta (parentKey, "user/tests/pluginprocess/close") != NULL,
		    "child process didn't set the close metadata on the parent key");
	succeed_if (elektraPluginGetData (plugin) == NULL, "didn't free the pluginprocess struct in the plugin's data");

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	free (plugin);
}

static void test_emptyKeySet (void)
{
	printf ("test emptyKeySet\n");

	Key * parentKey = keyNew ("user/tests/pluginprocess", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = NULL;

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "call to kdbSet with null keyset was successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "call to kdbGet with null keyset was successful");
	succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "call to kdbError with null keyset was successful");
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	free (plugin);
}

static void test_reservedParentKeyName (void)
{
	printf ("test reservedParentKeyName\n");

	Key * parentKey = keyNew ("/pluginprocess/parent/name", KEY_VALUE, "invalid", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, keyNew ("/pluginprocess/parent", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/parent/name", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/command", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/payload/exists", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/version", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/result", KEY_END));

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	free (plugin);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_communication ();
	test_emptyKeySet ();
	test_reservedParentKeyName ();

	print_result ("pluginprocess");

	return nbError;
}
