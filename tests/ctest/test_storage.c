/**
 * @file
 *
 * @brief Tests for storage plugins
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <tests_internal.h>
#include <tests_plugin.h>

#include <kdbconfig.h>

/* -- Macros & Test Configuration ------------------------------------------------------------------------------------------------------- */

#define TEST_ROOT_KEY "user:/tests/storage"

// below are the plugins suggested for testing, but only compiled plugins are tested
#define NUM_PLUGINS_SUGGESTED 4
static const char * pluginsSuggested[] = { "mmapstorage_crc", "mmapstorage", "dump",
					   "quickdump" }; // remember to adjust NUM_PLUGINS_SUGGESTED if you add/remove a storage plugin

// below is the list of available plugins truly tested.
static size_t numPlugins = 0;
static ElektraKeyset * modules[NUM_PLUGINS_SUGGESTED];
static Plugin * plugins[NUM_PLUGINS_SUGGESTED];
static const char * pluginNames[NUM_PLUGINS_SUGGESTED];

#define open_storage_plugin(storagePlugin)                                                                                                 \
	if (openStoragePlugin (storagePlugin) == -1)                                                                                       \
	{                                                                                                                                  \
		nbError++;                                                                                                                 \
		printf ("Error opening storage plugin: %s. Skipping test.\n", pluginNames[storagePlugin]);                                 \
		return;                                                                                                                    \
	}

/* -- KeySet test data ------------------------------------------------------------------------------------------------------------------ */

static ElektraKeyset * simpleTestKeySet (void)
{
	return elektraKeysetNew (10, elektraKeyNew ("user:/tests/storage/simpleKey", ELEKTRA_KEY_VALUE, "root key", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/storage/simpleKey/a", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/storage/simpleKey/b", ELEKTRA_KEY_VALUE, "b value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static ElektraKeyset * metaTestKeySet (void)
{
	return elektraKeysetNew (10, elektraKeyNew ("user:/tests/storage", ELEKTRA_KEY_VALUE, "root key", ELEKTRA_KEY_META, "a", "some metadata for root key", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/storage/a", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_META, "ab", "other metadata for a key", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/storage/b", ELEKTRA_KEY_VALUE, "b value", ELEKTRA_KEY_META, "longer val", "metadata for key b", ELEKTRA_KEY_END),
		      ELEKTRA_KS_END);
}

/* -- Test helpers ---------------------------------------------------------------------------------------------------------------------- */

static void initPlugins (void)
{
	// check if plugin is compiled, and only test if it is
	for (size_t plugin = 0; plugin < NUM_PLUGINS_SUGGESTED; ++plugin)
	{
		if (strstr (ELEKTRA_PLUGINS, pluginsSuggested[plugin]) != NULL)
		{
			pluginNames[numPlugins] = pluginsSuggested[plugin];
			numPlugins++;
		}
		else
		{
			printf ("Warning: Plugin %s is not compiled. Excuding from storage tests.\n", pluginsSuggested[plugin]);
		}
	}
}

static int openStoragePlugin (const size_t storagePlugin)
{
	modules[storagePlugin] = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules[storagePlugin], 0);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	Plugin * plugin = elektraPluginOpen (pluginNames[storagePlugin], modules[storagePlugin], conf, errorKey);

	const ElektraKey * metaWarnings = elektraKeyGetMeta (errorKey, "warnings");
	if (metaWarnings) printf ("There are warnings for plugin: %s\n", pluginNames[storagePlugin]);
	const ElektraKey * metaError = elektraKeyGetMeta (errorKey, "error");
	if (metaError) printf ("There are errors for plugin: %s\n", pluginNames[storagePlugin]);

	if (plugin == 0)
	{
		printf ("Could not open plugin: %s\n", pluginNames[storagePlugin]);
		return -1;
	}

	plugins[storagePlugin] = plugin;
	elektraKeyDel (errorKey);

	return 0;
}

static int closeStoragePlugin (const size_t storagePlugin)
{
	elektraPluginClose (plugins[storagePlugin], 0);
	elektraModulesClose (modules[storagePlugin], 0);
	elektraKeysetDel (modules[storagePlugin]);

	return 0;
}

/* -- KeySet API tests ------------------------------------------------------------------------------------------------------------------ */

static void test_ksDupFun (const size_t storagePlugin, const char * tmpFile, ElektraKeyset * copyFunction (const ElektraKeyset * source))
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * dupKs = copyFunction (ks);
	compare_keyset (dupKs, ks);
	compare_keyset (ks, dupKs);

	elektraKeysetDel (dupKs);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksCopy (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * copyKs = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (elektraKeysetCopy (copyKs, ks) == 1)
	{
		compare_keyset (copyKs, ks);
		compare_keyset (ks, copyKs);
	}
	else
	{
		yield_error ("ksCopy failed");
	}

	elektraKeysetDel (copyKs);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksGetSize (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	ssize_t origSize = elektraKeysetGetSize (ks);
	succeed_if (origSize > 0, "ks was empty before kdbSet");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	elektraKeysetDel (ks);
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ssize_t returnedSize = elektraKeysetGetSize (ks);
	succeed_if (origSize == returnedSize, "ksGetSize before and after kdbSet, kdbGet differ");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_double_get (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ElektraKeyset * first = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, first, parentKey) == 1, "kdbGet was not successful");
	ElektraKeyset * second = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, second, parentKey) == 1, "kdbGet was not successful");
	succeed_if (first->array != second->array, "ks->array points to same thing");

	compare_keyset (first, ks);
	compare_keyset (ks, first);
	compare_keyset (second, ks);
	compare_keyset (ks, second);

	elektraKeysetDel (ks);
	ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ElektraKeyset * simple = simpleTestKeySet ();
	compare_keyset (first, simple);
	compare_keyset (second, simple);
	elektraKeysetDel (first);
	elektraKeysetDel (second);
	elektraKeysetDel (simple);
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	closeStoragePlugin (storagePlugin);
}

static void test_ksAppendKey (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	ssize_t origSize = elektraKeysetGetSize (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ssize_t appendSize = 0;
	ElektraKey * toAppend = elektraKeyNew (TEST_ROOT_KEY "/my/new/key", ELEKTRA_KEY_END);

	if ((appendSize = elektraKeysetAppendKey (ks, toAppend)) == -1)
	{
		yield_error ("ksAppendKey failed");
	}

	succeed_if (appendSize == (origSize + 1), "ksAppendKey after append should be incremented");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ssize_t returnedSize = elektraKeysetGetSize (ks);

	succeed_if (returnedSize == (origSize + 1), "ksGetSize after append should be incremented");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksAppend (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * toAppend = elektraKeysetNew (10, elektraKeyNew ("user:/tests/storage/zzzz", ELEKTRA_KEY_VALUE, "root key", ELEKTRA_KEY_END),
				   elektraKeyNew ("user:/tests/storage/simpleKey/c", ELEKTRA_KEY_VALUE, "c value", ELEKTRA_KEY_END),
				   elektraKeyNew ("user:/tests/storage/simpleKey/d", ELEKTRA_KEY_VALUE, "d value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	if (elektraKeysetAppend (ks, toAppend) == -1)
	{
		yield_error ("ksAppend failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	elektraKeysetDel (toAppend);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksCut (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	// create keyset with some folder 'other' that we will then cut
	ElektraKeyset * ks = simpleTestKeySet ();
	ElektraKeyset * other = elektraKeysetNew (10, elektraKeyNew ("user:/tests/storage/other", ELEKTRA_KEY_VALUE, "other key", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/tests/storage/other/a", ELEKTRA_KEY_VALUE, "other a value", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/tests/storage/other/b", ELEKTRA_KEY_VALUE, "other b value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	if (elektraKeysetAppend (ks, other) == -1)
	{
		yield_error ("ksAppend failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	// now cut the 'other' folder
	ElektraKey * cutKey = elektraKeyNew ("user:/tests/storage/other", ELEKTRA_KEY_END);
	ElektraKeyset * returned = elektraKeysetCut (ks, cutKey);
	succeed_if (returned, "keyset is empty (does not contain the cut keyset)");

	ElektraKeyset * simple = simpleTestKeySet ();
	compare_keyset (simple, ks);
	compare_keyset (other, returned);

	elektraKeysetDel (other);
	elektraKeysetDel (returned);
	elektraKeysetDel (simple);
	elektraKeyDel (cutKey);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksPop (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * poppedKeys = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (elektraKeysetAppendKey (poppedKeys, elektraKeysetPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (elektraKeysetAppendKey (poppedKeys, elektraKeysetPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (elektraKeysetGetSize (ks) == 1, "ksGetSize after ksPop should be decremented");
	succeed_if (elektraKeysetAppendKey (poppedKeys, elektraKeysetPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (elektraKeysetGetSize (poppedKeys) == 3, "expecting three keys to be in ks");
	succeed_if (elektraKeysetPop (ks) == 0, "ks should be empty");
	succeed_if (elektraKeysetAppendKey (poppedKeys, elektraKeysetPop (ks)) == -1, "ks should be empty, but is not");

	ElektraKeyset * test = simpleTestKeySet ();
	compare_keyset (poppedKeys, test);
	elektraKeysetDel (test);

	elektraKeysetDel (poppedKeys);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksLookup (const size_t storagePlugin, const char * tmpFile, elektraLookupFlags options)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * lookup = elektraKeyNew ("user:/tests/storage/simpleKey/a", ELEKTRA_KEY_END);
	ElektraKey * found = elektraKeysetLookup (ks, lookup, options);
	succeed_if (found, "did not find key");
	if (options == ELEKTRA_KDB_O_POP)
	{
		// make sure key is really popped
		elektraKeyDel (found);
		found = elektraKeysetLookup (ks, lookup, 0);
		succeed_if (!found, "found key that should not exist");
	}
	elektraKeyDel (lookup);

	lookup = elektraKeyNew ("user:/tests/storage/simpleKey/foo", ELEKTRA_KEY_END);
	found = elektraKeysetLookup (ks, lookup, options);
	succeed_if (!found, "found key that should not exist");
	elektraKeyDel (lookup);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksLookupByName (const size_t storagePlugin, const char * tmpFile, elektraLookupFlags options)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/storage/simpleKey/a";
	ElektraKey * found = elektraKeysetLookupByName (ks, name, options);
	succeed_if (found, "did not find key");
	if (options == ELEKTRA_KDB_O_POP)
	{
		// make sure key is really popped
		elektraKeyDel (found);
		found = elektraKeysetLookupByName (ks, name, 0);
		succeed_if (!found, "found key that should not exist");
	}

	name = "user:/tests/storage/simpleKey/foo";
	found = elektraKeysetLookupByName (ks, name, options);
	succeed_if (!found, "found key that should not exist");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

/* -- Key API tests --------------------------------------------------------------------------------------------------------------------- */

static void test_keyFlags (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("user:/tests/storage/testKey", ELEKTRA_KEY_FLAGS, ELEKTRA_KEY_BINARY, ELEKTRA_KEY_VALUE, "test key", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/testKey", 0);
	succeed_if (found, "did not find key");
	succeed_if (elektraKeyIsBinary (found) == 1, "Key is not binary.");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyDup (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	ElektraKey * duplicate = elektraKeyDup (found, ELEKTRA_KEY_CP_ALL);

	// check that keyDup has not changed KeySet
	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	// check that KeySet is intact after deleting duplicate Key
	elektraKeyDel (duplicate);
	compare_keyset (ks, expected);

	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyCopy_newKey (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	ElektraKey * copy = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyCopy (copy, found, ELEKTRA_KEY_CP_ALL) != NULL, "keyCopy failed");

	compare_key (found, copy);

	// check that keyCopy has not changed KeySet
	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	// check that KeySet is intact after deleting Key copy
	elektraKeyDel (copy);
	compare_keyset (ks, expected);

	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyCopy_clearOverwriteKey (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * toCopy = elektraKeyNew ("user:/tests/storage/newnewkey", ELEKTRA_KEY_VALUE, "new key", ELEKTRA_KEY_END);

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/b", ELEKTRA_KDB_O_POP);
	succeed_if (found, "did not find key");

	// currently, KDB_O_POP doest not clear the readonly name flag
	if (test_bit (found->flags, ELEKTRA_KEY_FLAG_RO_NAME))
	{
		clear_bit (found->flags, ELEKTRA_KEY_FLAG_RO_NAME);
	}

	// overwrite Key
	succeed_if (elektraKeyCopy (found, 0, ELEKTRA_KEY_CP_ALL) != NULL, "keyCopy: clear destination failed");
	succeed_if (elektraKeyCopy (found, toCopy, ELEKTRA_KEY_CP_ALL) != NULL, "keyCopy failed");
	compare_key (found, toCopy);
	elektraKeyDel (toCopy);

	// put key back into place
	elektraKeysetAppendKey (ks, found);

	// write KeySet back to storage
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = elektraKeysetLookupByName (ks, "user:/tests/storage/newnewkey", 0);
	succeed_if (found, "did not find key");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyDel (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	succeed_if (elektraKeyDel (found) > 0, "Key was NULL or free()'d unexpectedly");

	// check that keyDel has not changed KeySet
	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyClear (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/a", 0);
	succeed_if (found, "did not find key");

	succeed_if (elektraKeyClear (found) == 0, "Key was NULL, keyClear failed");

	elektraKeySetName (found, "user:/tests/storage/foo");
	elektraKeySetString (found, "new key value");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

/* -- Key name API tests ---------------------------------------------------------------------------------------------------------------- */

static void test_keyName (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/storage/a";
	ElektraKey * found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t nameSize = elektraKeyGetNameSize (found);
	succeed_if (elektraStrNCmp (name, elektraKeyName (found), nameSize) == 0, "wrong Key name");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keySetName (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	ElektraKey * duplicate = elektraKeyDup (found, ELEKTRA_KEY_CP_ALL);
	elektraKeySetName (duplicate, "user:/tests/storage/z");
	elektraKeySetString (duplicate, "zzz");

	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	elektraKeysetDel (expected);
	elektraKeyDel (duplicate);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyGetBaseName (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/storage/a";
	ElektraKey * found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	const char * constBaseName = "a";
	size_t constBaseNameSize = elektraStrLen (constBaseName);
	ssize_t baseNameSize = elektraKeyGetBaseNameSize (found);
	char * baseName = elektraMalloc (baseNameSize);
	ssize_t ret = elektraKeyGetBaseName (found, baseName, baseNameSize);
	if (ret < 1)
	{
		yield_error ("Key base name NULL or size error");
	}
	else
	{
		succeed_if ((size_t) ret == elektraStrLen (constBaseName), "Key base name has wrong size");
	}

	succeed_if (elektraStrNCmp (constBaseName, baseName, constBaseNameSize) == 0, "Key base name is wrong");

	elektraFree (baseName);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

/* -- Key value API tests --------------------------------------------------------------------------------------------------------------- */

static void test_keyValue (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	size_t valueSize = 42;
	void * value = elektraMalloc (valueSize);
	memset (value, 42, valueSize);

	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_END);
	elektraKeySetBinary (key, value, valueSize);
	elektraKeysetAppendKey (ks, elektraKeyDup (key, ELEKTRA_KEY_CP_ALL));
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");
	compare_key (key, found);

	elektraFree (value);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeyDel (key);
	closeStoragePlugin (storagePlugin);
}

static void test_keyString (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	const char * value = "special value";
	size_t valueSize = elektraStrLen (value);
	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END);
	elektraKeysetAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	succeed_if (elektraStrNCmp (value, elektraKeyString (found), valueSize) == 0, "Key string value is wrong");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyGetBinary (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	size_t realValueSize = 42;
	void * value = elektraMalloc (realValueSize);
	memset (value, 42, realValueSize);

	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_END);
	elektraKeySetBinary (key, value, realValueSize);
	elektraKeysetAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = elektraKeyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (elektraKeyGetBinary (found, apiValue, apiValueSize) == (ssize_t) realValueSize, "Key binary has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) == 0, "Key binary value is wrong");

	elektraFree (apiValue);
	elektraFree (value);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyGetString (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	const char * value = "special value";
	size_t realValueSize = elektraStrLen (value);
	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END);
	elektraKeysetAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = elektraKeyGetValueSize (found);
	char * apiString = elektraMalloc (apiValueSize);
	succeed_if (elektraKeyGetString (found, apiString, apiValueSize) == (ssize_t) realValueSize, "Key string has wrong size");

	succeed_if (elektraStrNCmp (value, apiString, realValueSize) == 0, "Key string value is wrong");

	elektraFree (apiString);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keySetBinary (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	size_t realValueSize = 42;
	void * value = elektraMalloc (realValueSize);
	memset (value, 42, realValueSize);

	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_END);
	elektraKeySetBinary (key, value, realValueSize);
	elektraKeysetAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, name, ELEKTRA_KDB_O_POP);
	succeed_if (found, "did not find key");

	// now set a new key value to the Key _after_ kdbGet
	size_t newValueSize = 4096;
	void * newValue = elektraMalloc (newValueSize);
	memset (newValue, 253, newValueSize);

	succeed_if (elektraKeySetBinary (found, newValue, newValueSize) == (ssize_t) newValueSize, "Key binary could not be set");

	elektraKeysetAppendKey (ks, found);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = elektraKeyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (elektraKeyGetBinary (found, apiValue, apiValueSize) == (ssize_t) newValueSize, "Key binary has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) != 0, "Key binary value is wrong");
	succeed_if (elektraStrNCmp (newValue, apiValue, newValueSize) == 0, "Key binary value is wrong");

	elektraFree (newValue);
	elektraFree (apiValue);
	elektraFree (value);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keySetString (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	const char * value = "special value";
	size_t realValueSize = elektraStrLen (value);
	ElektraKey * key = elektraKeyNew (name, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END);
	elektraKeysetAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * found = elektraKeysetLookupByName (ks, name, ELEKTRA_KDB_O_POP);
	succeed_if (found, "did not find key");

	// now set a new key string to the Key _after_ kdbGet
	const char * newValue = "some new special value";
	size_t newValueSize = elektraStrLen (newValue);

	succeed_if (elektraKeySetString (found, newValue) == (ssize_t) newValueSize, "Key string could not be set");

	elektraKeysetAppendKey (ks, found);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = elektraKeysetLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = elektraKeyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (elektraKeyGetString (found, apiValue, apiValueSize) == (ssize_t) newValueSize, "Key string has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) != 0, "Key string value is wrong");
	succeed_if (elektraStrNCmp (newValue, apiValue, newValueSize) == 0, "Key string value is wrong");

	elektraFree (apiValue);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void clearStorage (const size_t storagePlugin, const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);

	closeStoragePlugin (storagePlugin);
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("STORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	initPlugins ();

	for (size_t plugin = 0; plugin < numPlugins; ++plugin)
	{
		const char * tmpFile = elektraFilename ();

		printf ("Testing plugin %s\n", pluginNames[plugin]);
		fprintf (stdout, "Tmp-file: %s\n", tmpFile);

		// KeySet API tests
		clearStorage (plugin, tmpFile);
		test_ksDupFun (plugin, tmpFile, elektraKeysetDup);

		clearStorage (plugin, tmpFile);
		test_ksDupFun (plugin, tmpFile, elektraKeysetDeepDup);

		clearStorage (plugin, tmpFile);
		test_ksCopy (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_ksGetSize (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_double_get (plugin, tmpFile); // regression test

		clearStorage (plugin, tmpFile);
		test_ksAppendKey (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_ksAppend (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_ksCut (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_ksPop (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_ksLookup (plugin, tmpFile, 0);

		clearStorage (plugin, tmpFile);
		test_ksLookup (plugin, tmpFile, ELEKTRA_KDB_O_POP);

		clearStorage (plugin, tmpFile);
		test_ksLookupByName (plugin, tmpFile, 0);

		clearStorage (plugin, tmpFile);
		test_ksLookupByName (plugin, tmpFile, ELEKTRA_KDB_O_POP);

		// Key API tests
		clearStorage (plugin, tmpFile);
		test_keyFlags (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyDup (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyCopy_newKey (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyCopy_clearOverwriteKey (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyDel (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyClear (plugin, tmpFile);

		// Key Name API tests
		clearStorage (plugin, tmpFile);
		test_keyName (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keySetName (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyGetBaseName (plugin, tmpFile);

		// Key Value API tests
		clearStorage (plugin, tmpFile);
		test_keyValue (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyString (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyGetBinary (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keyGetString (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keySetBinary (plugin, tmpFile);

		clearStorage (plugin, tmpFile);
		test_keySetString (plugin, tmpFile);
	}

	printf ("\ntest_storage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
