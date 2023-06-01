/**
 * @file
 *
 * @brief Tests for pluginprocess library.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <internal/pluginprocess.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/plugin/plugin.h>

#include <internal/plugin/struct.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>

#include <stdio.h>
#include <stdlib.h>

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
		elektraPluginProcessSetData (pp, testData);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	keySetMeta (errorKey, "user:/tests/pluginprocess/open", "");
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummyClose (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp)
	{
		int * testData = (int *) elektraPluginProcessGetData (pp);
		elektraPluginSetData (handle, NULL);
		elektraFree (testData);

		if (elektraPluginProcessIsParent (pp))
		{
			ElektraPluginProcessCloseResult result = elektraPluginProcessClose (pp, errorKey);
			return result.result;
		}
	}

	keySetMeta (errorKey, "user:/tests/pluginprocess/close", "");
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummyInit (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp))
	{
		return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_INIT, returned, parentKey);
	}

	keySetMeta (parentKey, "user:/tests/pluginprocess/init", "");
	ksAppendKey (returned, keyNew ("user:/tests/pluginprocess/init", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummyGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_GET, returned, parentKey);

	int * testData = (int *) elektraPluginProcessGetData (pp);

	keySetMeta (parentKey, "user:/tests/pluginprocess/get", "");
	// Just check if the child can access its actual plugin data
	if (*testData == 42) keySetMeta (parentKey, "user:/tests/pluginprocess/testdata", "");
	ksAppendKey (returned, keyNew ("user:/tests/pluginprocess/get", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummySet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp))
	{
		return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);
	}

	keySetMeta (parentKey, "user:/tests/pluginprocess/set", "value");
	ksAppendKey (returned, keyNew ("user:/tests/pluginprocess/set", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummyCommit (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp))
	{
		return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_COMMIT, returned, parentKey);
	}

	keySetMeta (parentKey, "user:/tests/pluginprocess/commit", "value");
	ksAppendKey (returned, keyNew ("user:/tests/pluginprocess/commit", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static int elektraDummyError (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_ERROR, returned, parentKey);

	keySetMeta (parentKey, "user:/tests/pluginprocess/error", "");
	ksAppendKey (returned, keyNew ("user:/tests/pluginprocess/error", KEY_END));
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
	plugin->kdbInit = &elektraDummyInit;
	plugin->kdbCommit = &elektraDummyCommit;
	plugin->name = "dummy";
	plugin->refcounter = 1;
	plugin->data = NULL;
	return plugin;
}

static void test_communication (void)
{
	printf ("test communication\n");

	Key * parentKey = keyNew ("user:/tests/pluginprocess", KEY_END);
	keySetMeta (parentKey, "/hello/from/parent", "value");
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	ElektraPluginProcess * pp = elektraPluginGetData (plugin);
	succeed_if (pp != NULL, "didn't store the pluginprocess struct in the plugin's data");
	if (pp)
	{
		succeed_if (keyGetMeta (parentKey, "user:/tests/pluginprocess/open") != NULL,
			    "child process didn't set the open metadata on the parent key");
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
		const Key * parentMeta = keyGetMeta (parentKey, "/hello/from/parent");
		succeed_if (parentMeta != NULL, "missing parent metadata on parent key") if (parentMeta != NULL)
		{
			succeed_if (elektraStrCmp (keyString (parentMeta), "value") == 0, "missing parent metadata value on parent key");
		}
		const Key * childMeta = keyGetMeta (parentKey, "user:/tests/pluginprocess/set");
		succeed_if (childMeta != NULL, "missing child metadata on parent key");
		if (childMeta != NULL)
		{
			succeed_if (elektraStrCmp (keyString (childMeta), "value") == 0, "missing child metadata value on parent key");
		}
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		succeed_if (keyGetMeta (parentKey, "user:/tests/pluginprocess/get") != NULL,
			    "child process didn't set the get metadata on the parent key");
		succeed_if (keyGetMeta (parentKey, "user:/tests/pluginprocess/testdata") != NULL,
			    "child process didn't receive the plugin data from the parent");
		succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
			    "call to kdbError was not successful");
		succeed_if (keyGetMeta (parentKey, "user:/tests/pluginprocess/error") != NULL,
			    "child process didn't set the error metadata on the parent key");
	}
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
	succeed_if (keyGetMeta (parentKey, "user:/tests/pluginprocess/close") != NULL,
		    "child process didn't set the close metadata on the parent key");
	succeed_if (elektraPluginGetData (plugin) == NULL, "didn't free the pluginprocess struct in the plugin's data");
	succeed_if (ksLookup (ks, parentKey, KDB_O_NONE) == NULL, "stored the parent key in the keyset");

	output_warnings (parentKey);
	output_error (parentKey);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

static void test_emptyKeySet (void)
{
	printf ("test emptyKeySet\n");

	Key * parentKey = keyNew ("user:/tests/pluginprocess", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = NULL;

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	ElektraPluginProcess * pp = elektraPluginGetData (plugin);
	if (pp)
	{
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "call to kdbSet with null keyset was successful");
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "call to kdbGet with null keyset was successful");
		succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "call to kdbError with null keyset was successful");
	}
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

static void test_reservedParentKeyName (void)
{
	printf ("test reservedParentKeyName\n");

	Key * parentKey = keyNew ("/pluginprocess/parent/name", KEY_VALUE, "invalid", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = ksNew (6, KS_END);
	ksAppendKey (ks, keyNew ("/pluginprocess/parent", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/parent/name", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/command", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/payload/exists", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/version", KEY_END));
	ksAppendKey (ks, keyNew ("/pluginprocess/result", KEY_END));

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	ElektraPluginProcess * pp = elektraPluginGetData (plugin);
	if (pp)
	{
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	}
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

static void test_keysetContainingParentKey (void)
{
	printf ("test keysetContainingParentKey\n");

	Key * parentKey = keyNew ("user:/tests/pluginprocess", KEY_END);
	keySetMeta (parentKey, "/hello/from/parent", "value");
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);

	KeySet * ks = ksNew (1, KS_END);
	ksAppendKey (ks, parentKey);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	ElektraPluginProcess * pp = elektraPluginGetData (plugin);
	if (pp)
	{
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
		const Key * parentMeta = keyGetMeta (parentKey, "/hello/from/parent");
		succeed_if (parentMeta != NULL, "missing parent metadata on parent key") if (parentMeta != NULL)
		{
			succeed_if (elektraStrCmp (keyString (parentMeta), "value") == 0, "missing parent metadata value on parent key");
		}
		const Key * childMeta = keyGetMeta (parentKey, "user:/tests/pluginprocess/set");
		succeed_if (childMeta != NULL, "missing child metadata on parent key");
		if (childMeta != NULL)
		{
			succeed_if (elektraStrCmp (keyString (childMeta), "value") == 0, "missing child metadata value on parent key");
		}
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
			    "call to kdbError was not successful");
	}
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
	succeed_if (ksLookupByName (ks, "user:/tests/pluginprocess", KDB_O_NONE) != NULL,
		    "parent key got removed from the keyset by pluginprocess");

	output_warnings (parentKey);
	output_error (parentKey);

	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

static int elektraDummySetAddingParentKey (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (elektraPluginProcessIsParent (pp))
	{
		return elektraPluginProcessSend (pp, ELEKTRA_PLUGINPROCESS_SET, returned, parentKey);
	}

	keySetMeta (parentKey, "user:/tests/pluginprocess/set", "value");
	ksAppendKey (returned, keyNew ("user:/tests/pluginprocess/set", KEY_END));
	ksAppendKey (returned, parentKey);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void test_childAddingParentKey (void)
{
	printf ("test childAddingParentKey\n");

	Key * parentKey = keyNew ("user:/tests/pluginprocess", KEY_END);
	keySetMeta (parentKey, "/hello/from/parent", "value");
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);
	plugin->kdbSet = &elektraDummySetAddingParentKey;

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	ElektraPluginProcess * pp = elektraPluginGetData (plugin);
	if (pp)
	{
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
		Key * addedParentKey = ksLookup (ks, parentKey, KDB_O_NONE);
		succeed_if (addedParentKey != NULL, "parent key was not added to keyset");
		const Key * parentMeta = keyGetMeta (addedParentKey, "/hello/from/parent");
		succeed_if (parentMeta != NULL, "missing parent metadata on parent key") if (parentMeta != NULL)
		{
			succeed_if (elektraStrCmp (keyString (parentMeta), "value") == 0, "missing parent metadata value on parent key");
		}
		const Key * childMeta = keyGetMeta (addedParentKey, "user:/tests/pluginprocess/set");
		succeed_if (childMeta != NULL, "missing child metadata on parent key");
		if (childMeta != NULL)
		{
			succeed_if (elektraStrCmp (keyString (childMeta), "value") == 0, "missing child metadata value on parent key");
		}
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		succeed_if (plugin->kdbError (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
			    "call to kdbError was not successful");
	}
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
	succeed_if (ksLookupByName (ks, "user:/tests/pluginprocess", KDB_O_NONE) != NULL,
		    "parent key got removed from the keyset by pluginprocess");

	output_warnings (parentKey);
	output_error (parentKey);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

static int elektraDummyOpenWithError (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
		elektraPluginSetData (handle, pp);
		// Assume some other initialization failed and thus close here without calling open
		// to free the resources but without sending the command
		// Note that init didn't fail thus we have forked already
		// so kill the child here and cleanup the parent
		if (elektraPluginProcessIsParent (pp))
		{
			elektraPluginProcessClose (pp, errorKey);
		}
		else
		{
			_Exit (EXIT_SUCCESS);
		}
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void test_closeWithoutOpen (void)
{
	printf ("test closeWithoutOpen\n");

	Key * parentKey = keyNew ("user:/tests/pluginprocess", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);
	plugin->kdbOpen = &elektraDummyOpenWithError;
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");

	output_warnings (parentKey);
	output_error (parentKey);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

static int elektraDummyOpenAndDie (Plugin * handle, Key * errorKey)
{
	ElektraPluginProcess * pp = elektraPluginGetData (handle);
	if (pp == NULL)
	{
		if ((pp = elektraPluginProcessInit (errorKey)) == NULL) return ELEKTRA_PLUGIN_STATUS_ERROR;
		elektraPluginSetData (handle, pp);
		if (!elektraPluginProcessIsParent (pp)) elektraPluginProcessStart (handle, pp);
	}
	if (elektraPluginProcessIsParent (pp)) return elektraPluginProcessOpen (pp, errorKey);

	// simulate a dying child process to check if pipes get notified about it
	_Exit (0);
}

static void test_childDies (void)
{
	printf ("test childDies\n");

	Key * parentKey = keyNew ("user:/tests/pluginprocess", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * plugin = createDummyPlugin (conf);
	plugin->kdbOpen = &elektraDummyOpenAndDie;
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbOpen was successful");
	// Child died, we still have to call close to cleanup the resources
	// It is expected to fail as pluginprocess cannot communicate with the child anymore, but still cleans up resources
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbClose was successful");

	output_warnings (parentKey);
	output_error (parentKey);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (conf);
	elektraFree (plugin);
}

int main (int argc, char ** argv)
{
	init (argc, argv);

	test_communication ();
	test_emptyKeySet ();
	test_reservedParentKeyName ();
	test_keysetContainingParentKey ();
	test_closeWithoutOpen ();
	test_childAddingParentKey ();
	test_childDies ();

	print_result ("pluginprocess");

	return nbError;
}
