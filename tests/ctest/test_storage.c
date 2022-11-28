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
static KeySet * modules[NUM_PLUGINS_SUGGESTED];
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

static KeySet * simpleTestKeySet (void)
{
	return ksNew (10, keyNew ("user:/tests/storage/simpleKey", KEY_VALUE, "root key", KEY_END),
		      keyNew ("user:/tests/storage/simpleKey/a", KEY_VALUE, "a value", KEY_END),
		      keyNew ("user:/tests/storage/simpleKey/b", KEY_VALUE, "b value", KEY_END), KS_END);
}

static KeySet * metaTestKeySet (void)
{
	return ksNew (10, keyNew ("user:/tests/storage", KEY_VALUE, "root key", KEY_META, "a", "some metadata for root key", KEY_END),
		      keyNew ("user:/tests/storage/a", KEY_VALUE, "a value", KEY_META, "ab", "other metadata for a key", KEY_END),
		      keyNew ("user:/tests/storage/b", KEY_VALUE, "b value", KEY_META, "longer val", "metadata for key b", KEY_END),
		      KS_END);
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
	modules[storagePlugin] = ksNew (0, KS_END);
	elektraModulesInit (modules[storagePlugin], 0);
	KeySet * conf = ksNew (0, KS_END);
	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * plugin = elektraPluginOpen (pluginNames[storagePlugin], modules[storagePlugin], conf, errorKey);

	const Key * metaWarnings = keyGetMeta (errorKey, "warnings");
	if (metaWarnings) printf ("There are warnings for plugin: %s\n", pluginNames[storagePlugin]);
	const Key * metaError = keyGetMeta (errorKey, "error");
	if (metaError) printf ("There are errors for plugin: %s\n", pluginNames[storagePlugin]);

	if (plugin == 0)
	{
		printf ("Could not open plugin: %s\n", pluginNames[storagePlugin]);
		return -1;
	}

	plugins[storagePlugin] = plugin;
	keyDel (errorKey);

	return 0;
}

static int closeStoragePlugin (const size_t storagePlugin)
{
	elektraPluginClose (plugins[storagePlugin], 0);
	elektraModulesClose (modules[storagePlugin], 0);
	ksDel (modules[storagePlugin]);

	return 0;
}

/* -- KeySet API tests ------------------------------------------------------------------------------------------------------------------ */

static void test_ksDupFun (const size_t storagePlugin, const char * tmpFile, KeySet * copyFunction (const KeySet * source))
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * dupKs = copyFunction (ks);
	compare_keyset (dupKs, ks);
	compare_keyset (ks, dupKs);

	ksDel (dupKs);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksCopy (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * copyKs = ksNew (0, KS_END);
	if (ksCopy (copyKs, ks) == 1)
	{
		compare_keyset (copyKs, ks);
		compare_keyset (ks, copyKs);
	}
	else
	{
		yield_error ("ksCopy failed");
	}

	ksDel (copyKs);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksGetSize (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	ssize_t origSize = ksGetSize (ks);
	succeed_if (origSize > 0, "ks was empty before kdbSet");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ssize_t returnedSize = ksGetSize (ks);
	succeed_if (origSize == returnedSize, "ksGetSize before and after kdbSet, kdbGet differ");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_double_get (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * first = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, first, parentKey) == 1, "kdbGet was not successful");
	KeySet * second = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, second, parentKey) == 1, "kdbGet was not successful");
	succeed_if (first->data->array != second->data->array, "ks->array points to same thing");

	compare_keyset (first, ks);
	compare_keyset (ks, first);
	compare_keyset (second, ks);
	compare_keyset (ks, second);

	ksDel (ks);
	ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * simple = simpleTestKeySet ();
	compare_keyset (first, simple);
	compare_keyset (second, simple);
	ksDel (first);
	ksDel (second);
	ksDel (simple);
	ksDel (ks);
	keyDel (parentKey);
	closeStoragePlugin (storagePlugin);
}

static void test_ksAppendKey (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	ssize_t origSize = ksGetSize (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ssize_t appendSize = 0;
	Key * toAppend = keyNew (TEST_ROOT_KEY "/my/new/key", KEY_END);

	if ((appendSize = ksAppendKey (ks, toAppend)) == -1)
	{
		yield_error ("ksAppendKey failed");
	}

	succeed_if (appendSize == (origSize + 1), "ksAppendKey after append should be incremented");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ssize_t returnedSize = ksGetSize (ks);

	succeed_if (returnedSize == (origSize + 1), "ksGetSize after append should be incremented");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksAppend (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * toAppend = ksNew (10, keyNew ("user:/tests/storage/zzzz", KEY_VALUE, "root key", KEY_END),
				   keyNew ("user:/tests/storage/simpleKey/c", KEY_VALUE, "c value", KEY_END),
				   keyNew ("user:/tests/storage/simpleKey/d", KEY_VALUE, "d value", KEY_END), KS_END);
	if (ksAppend (ks, toAppend) == -1)
	{
		yield_error ("ksAppend failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ksDel (toAppend);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksCut (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	// create keyset with some folder 'other' that we will then cut
	KeySet * ks = simpleTestKeySet ();
	KeySet * other = ksNew (10, keyNew ("user:/tests/storage/other", KEY_VALUE, "other key", KEY_END),
				keyNew ("user:/tests/storage/other/a", KEY_VALUE, "other a value", KEY_END),
				keyNew ("user:/tests/storage/other/b", KEY_VALUE, "other b value", KEY_END), KS_END);
	if (ksAppend (ks, other) == -1)
	{
		yield_error ("ksAppend failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	// now cut the 'other' folder
	Key * cutKey = keyNew ("user:/tests/storage/other", KEY_END);
	KeySet * returned = ksCut (ks, cutKey);
	succeed_if (returned, "keyset is empty (does not contain the cut keyset)");

	KeySet * simple = simpleTestKeySet ();
	compare_keyset (simple, ks);
	compare_keyset (other, returned);

	ksDel (other);
	ksDel (returned);
	ksDel (simple);
	keyDel (cutKey);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksPop (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * poppedKeys = ksNew (0, KS_END);
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (ksGetSize (ks) == 1, "ksGetSize after ksPop should be decremented");
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (ksGetSize (poppedKeys) == 3, "expecting three keys to be in ks");
	succeed_if (ksPop (ks) == 0, "ks should be empty");
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) == -1, "ks should be empty, but is not");

	KeySet * test = simpleTestKeySet ();
	compare_keyset (poppedKeys, test);
	ksDel (test);

	ksDel (poppedKeys);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksLookup (const size_t storagePlugin, const char * tmpFile, elektraLookupFlags options)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * lookup = keyNew ("user:/tests/storage/simpleKey/a", KEY_END);
	Key * found = ksLookup (ks, lookup, options);
	succeed_if (found, "did not find key");
	if (options == KDB_O_POP)
	{
		// make sure key is really popped
		keyDel (found);
		found = ksLookup (ks, lookup, 0);
		succeed_if (!found, "found key that should not exist");
	}
	keyDel (lookup);

	lookup = keyNew ("user:/tests/storage/simpleKey/foo", KEY_END);
	found = ksLookup (ks, lookup, options);
	succeed_if (!found, "found key that should not exist");
	keyDel (lookup);

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_ksLookupByName (const size_t storagePlugin, const char * tmpFile, elektraLookupFlags options)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/storage/simpleKey/a";
	Key * found = ksLookupByName (ks, name, options);
	succeed_if (found, "did not find key");
	if (options == KDB_O_POP)
	{
		// make sure key is really popped
		keyDel (found);
		found = ksLookupByName (ks, name, 0);
		succeed_if (!found, "found key that should not exist");
	}

	name = "user:/tests/storage/simpleKey/foo";
	found = ksLookupByName (ks, name, options);
	succeed_if (!found, "found key that should not exist");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

/* -- Key API tests --------------------------------------------------------------------------------------------------------------------- */

static void test_keyFlags (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = ksNew (10, keyNew ("user:/tests/storage/testKey", KEY_FLAGS, KEY_BINARY, KEY_VALUE, "test key", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user:/tests/storage/testKey", 0);
	succeed_if (found, "did not find key");
	succeed_if (keyIsBinary (found) == 1, "Key is not binary.");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyDup (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	Key * duplicate = keyDup (found, KEY_CP_ALL);

	// check that keyDup has not changed KeySet
	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	// check that KeySet is intact after deleting duplicate Key
	keyDel (duplicate);
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyCopy_newKey (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	Key * copy = keyNew ("/", KEY_END);
	succeed_if (keyCopy (copy, found, KEY_CP_ALL) != NULL, "keyCopy failed");

	compare_key (found, copy);

	// check that keyCopy has not changed KeySet
	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	// check that KeySet is intact after deleting Key copy
	keyDel (copy);
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyCopy_clearOverwriteKey (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * toCopy = keyNew ("user:/tests/storage/newnewkey", KEY_VALUE, "new key", KEY_END);

	Key * found = ksLookupByName (ks, "user:/tests/storage/b", KDB_O_POP);
	succeed_if (found, "did not find key");

	// currently, KDB_O_POP doest not clear the readonly name flag
	found->hasReadOnlyName = false;

	// overwrite Key
	succeed_if (keyCopy (found, 0, KEY_CP_ALL) != NULL, "keyCopy: clear destination failed");
	succeed_if (keyCopy (found, toCopy, KEY_CP_ALL) != NULL, "keyCopy failed");
	compare_key (found, toCopy);
	keyDel (toCopy);

	// put key back into place
	ksAppendKey (ks, found);

	// write KeySet back to storage
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = ksLookupByName (ks, "user:/tests/storage/newnewkey", 0);
	succeed_if (found, "did not find key");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyDel (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	succeed_if (keyDel (found) > 0, "Key was NULL or free()'d unexpectedly");

	// check that keyDel has not changed KeySet
	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyClear (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user:/tests/storage/a", 0);
	succeed_if (found, "did not find key");

	succeed_if (keyClear (found) == 0, "Key was NULL, keyClear failed");

	keySetName (found, "user:/tests/storage/foo");
	keySetString (found, "new key value");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

/* -- Key name API tests ---------------------------------------------------------------------------------------------------------------- */

static void test_keyName (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/storage/a";
	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t nameSize = keyGetNameSize (found);
	succeed_if (elektraStrNCmp (name, keyName (found), nameSize) == 0, "wrong Key name");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keySetName (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user:/tests/storage/b", 0);
	succeed_if (found, "did not find key");

	Key * duplicate = keyDup (found, KEY_CP_ALL);
	keySetName (duplicate, "user:/tests/storage/z");
	keySetString (duplicate, "zzz");

	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (duplicate);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyGetBaseName (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/storage/a";
	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	const char * constBaseName = "a";
	size_t constBaseNameSize = elektraStrLen (constBaseName);
	ssize_t baseNameSize = keyGetBaseNameSize (found);
	char * baseName = elektraMalloc (baseNameSize);
	ssize_t ret = keyGetBaseName (found, baseName, baseNameSize);
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
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

/* -- Key value API tests --------------------------------------------------------------------------------------------------------------- */

static void test_keyValue (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	size_t valueSize = 42;
	void * value = elektraMalloc (valueSize);
	memset (value, 42, valueSize);

	Key * key = keyNew (name, KEY_END);
	keySetBinary (key, value, valueSize);
	ksAppendKey (ks, keyDup (key, KEY_CP_ALL));
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");
	compare_key (key, found);

	elektraFree (value);
	keyDel (parentKey);
	ksDel (ks);
	keyDel (key);
	closeStoragePlugin (storagePlugin);
}

static void test_keyString (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	const char * value = "special value";
	size_t valueSize = elektraStrLen (value);
	Key * key = keyNew (name, KEY_VALUE, value, KEY_END);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	succeed_if (elektraStrNCmp (value, keyString (found), valueSize) == 0, "Key string value is wrong");

	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyGetBinary (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	size_t realValueSize = 42;
	void * value = elektraMalloc (realValueSize);
	memset (value, 42, realValueSize);

	Key * key = keyNew (name, KEY_END);
	keySetBinary (key, value, realValueSize);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (keyGetBinary (found, apiValue, apiValueSize) == (ssize_t) realValueSize, "Key binary has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) == 0, "Key binary value is wrong");

	elektraFree (apiValue);
	elektraFree (value);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keyGetString (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	const char * value = "special value";
	size_t realValueSize = elektraStrLen (value);
	Key * key = keyNew (name, KEY_VALUE, value, KEY_END);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiString = elektraMalloc (apiValueSize);
	succeed_if (keyGetString (found, apiString, apiValueSize) == (ssize_t) realValueSize, "Key string has wrong size");

	succeed_if (elektraStrNCmp (value, apiString, realValueSize) == 0, "Key string value is wrong");

	elektraFree (apiString);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keySetBinary (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	size_t realValueSize = 42;
	void * value = elektraMalloc (realValueSize);
	memset (value, 42, realValueSize);

	Key * key = keyNew (name, KEY_END);
	keySetBinary (key, value, realValueSize);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, KDB_O_POP);
	succeed_if (found, "did not find key");

	// now set a new key value to the Key _after_ kdbGet
	size_t newValueSize = 4096;
	void * newValue = elektraMalloc (newValueSize);
	memset (newValue, 253, newValueSize);

	succeed_if (keySetBinary (found, newValue, newValueSize) == (ssize_t) newValueSize, "Key binary could not be set");

	ksAppendKey (ks, found);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (keyGetBinary (found, apiValue, apiValueSize) == (ssize_t) newValueSize, "Key binary has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) != 0, "Key binary value is wrong");
	succeed_if (elektraStrNCmp (newValue, apiValue, newValueSize) == 0, "Key binary value is wrong");

	elektraFree (newValue);
	elektraFree (apiValue);
	elektraFree (value);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void test_keySetString (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = metaTestKeySet ();
	const char * name = "user:/tests/storage/specialkey";
	const char * value = "special value";
	size_t realValueSize = elektraStrLen (value);
	Key * key = keyNew (name, KEY_VALUE, value, KEY_END);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, KDB_O_POP);
	succeed_if (found, "did not find key");

	// now set a new key string to the Key _after_ kdbGet
	const char * newValue = "some new special value";
	size_t newValueSize = elektraStrLen (newValue);

	succeed_if (keySetString (found, newValue) == (ssize_t) newValueSize, "Key string could not be set");

	ksAppendKey (ks, found);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (keyGetString (found, apiValue, apiValueSize) == (ssize_t) newValueSize, "Key string has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) != 0, "Key string value is wrong");
	succeed_if (elektraStrNCmp (newValue, apiValue, newValueSize) == 0, "Key string value is wrong");

	elektraFree (apiValue);
	keyDel (parentKey);
	ksDel (ks);
	closeStoragePlugin (storagePlugin);
}

static void clearStorage (const size_t storagePlugin, const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	open_storage_plugin (storagePlugin);
	Plugin * plugin = plugins[storagePlugin];

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);

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
		test_ksDupFun (plugin, tmpFile, ksDup);

		clearStorage (plugin, tmpFile);
		test_ksDupFun (plugin, tmpFile, ksDeepDup);

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
		test_ksLookup (plugin, tmpFile, KDB_O_POP);

		clearStorage (plugin, tmpFile);
		test_ksLookupByName (plugin, tmpFile, 0);

		clearStorage (plugin, tmpFile);
		test_ksLookupByName (plugin, tmpFile, KDB_O_POP);

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
