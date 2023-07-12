/**
 * @file
 *
 * @brief Tests for the c plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <tests_plugin.h>

static void test_contract (void)
{
	printf ("test contract\n");

	KeySet * conf = ksNew (0, KS_END);
	KeySet * contract = ksNew (0, KS_END);
	Key * parentKey = keyNew ("system:/elektra/modules/c", KEY_END);

	PLUGIN_OPEN ("c");
	succeed_if (plugin->kdbGet (plugin, contract, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");
	PLUGIN_CLOSE ();

	succeed_if (ksGetSize (contract) > 0, "Empty KeySet was returned for plugin contract");
	succeed_if (ksLookup (contract, parentKey, KDB_O_NONE),
		    "The key with the name 'system:/elektra/modules/c' was not part of the plugin contract");
	keyDel (parentKey);

	Key * keyExportsSet = ksLookupByName (contract, "system:/elektra/modules/c/exports/set", KDB_O_NONE);
	succeed_if (keyExportsSet,
		    "The set-function was not part of the plugin contract (key 'system:/elektra/modules/c/exports/set' not found)");
	succeed_if (keyValue (keyExportsSet), "The set function retrieved as part of the plugin contract was NULL");
	ksDel (contract);
}

static void test_set_empty (void)
{
	printf ("test set with empty KeySet\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("c");

	Key * parentKey = keyNew ("user:/tests/c", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * ksEmpty = ksNew (0, KS_END);
	succeed_if (plugin->kdbSet (plugin, ksEmpty, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to write empty KeySet to file");
	PLUGIN_CLOSE ();

	keyDel (parentKey);
	ksDel (ksEmpty);
}

static void test_set_simple (void)
{
	printf ("test set with simple KeySet\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("c");

	Key * parentKey = keyNew ("user:/tests/c", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * ksTest = ksNew (0, KS_END);
	ksAppendKey (ksTest, keyNew ("user:/tests/c/key1", KEY_VALUE, "value1", KEY_END));
	ksAppendKey (ksTest, keyNew ("user:/tests/c/key2", KEY_VALUE, "value2", KEY_END));
	ksAppendKey (ksTest, keyNew ("user:/tests/c/key3", KEY_VALUE, "value3", KEY_END));

	succeed_if (plugin->kdbSet (plugin, ksTest, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "Unable to write non-empty KeySet to file");
	PLUGIN_CLOSE ();

	keyDel (parentKey);
	ksDel (ksTest);
}

static void test_set_meta (void)
{
	printf ("test set with simple KeySet\n");

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("c");

	Key * parentKey = keyNew ("user:/tests/c", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * ksTest = ksNew (0, KS_END);

	Key * k1 = keyNew ("user:/tests/c/key1", KEY_VALUE, "value1", KEY_END);
	Key * k2 = keyNew ("user:/tests/c/key2", KEY_VALUE, "value2", KEY_END);
	Key * k3 = keyNew ("user:/tests/c/key3", KEY_VALUE, "value3", KEY_END);

	keySetMeta (k1, "metakey 1.1", "metavalue 1.1");
	keySetMeta (k3, "metakey 3.1", "metavalue 3.1");
	keySetMeta (k3, "metakey 3.2", "metavalue 3.2");
	keySetMeta (k3, "metakey 3.3", "metavalue 3.3");

	ksAppendKey (ksTest, k1);
	ksAppendKey (ksTest, k2);
	ksAppendKey (ksTest, k3);

	succeed_if (plugin->kdbSet (plugin, ksTest, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "Unable to write KeySet with keys containing metadata to file");
	PLUGIN_CLOSE ();

	keyDel (parentKey);
	ksDel (ksTest);
}


int main (int argc, char ** argv)
{
	printf ("C PLUGIN      TESTS\n");
	printf ("==================\n\n");
	init (argc, argv);

	test_contract ();
	test_set_empty ();
	test_set_simple ();
	test_set_meta ();

	print_result ("testmod_c");
	return nbError;
}
