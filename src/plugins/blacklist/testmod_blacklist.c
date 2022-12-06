/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <elektra/kdbmodule.h>
#include <elektra/kdbplugin.h>
#include <tests_plugin.h>


static void test_blacklist (void)
{
	Key * parentKey = keyNew ("user:/tests/blacklist", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/blacklist/valid1", KEY_VALUE, "ICE", KEY_META, "check/blacklist", "#1", KEY_META,
			   "check/blacklist/#0", "FIRE", KEY_META, "check/blacklist/#1", "EARTH", KEY_END);
	Key * k2 = keyNew ("user:/tests/blacklist/valid2", KEY_VALUE, "EARTH", KEY_META, "check/blacklist", "#2", KEY_META,
			   "check/blacklist/#0", "FIRE", KEY_META, "check/blacklist/#2", "WATER", KEY_END);
	Key * k3 = keyNew ("user:/tests/blacklist/valid3", KEY_VALUE, "", KEY_META, "check/blacklist", "#1", KEY_META, "check/blacklist/#0",
			   "FIRE", KEY_META, "check/blacklist/#1", "EARTH", KEY_END);
	Key * k4 = keyNew ("user:/tests/blacklist/valid4", KEY_VALUE, "WATER", KEY_META, "check/blacklist", "#0", KEY_META,
			   "check/blacklist/#0", "COLD/WATER", KEY_END);
	Key * k5 = keyNew ("user:/tests/blacklist/invalid1", KEY_VALUE, "ICE", KEY_META, "check/blacklist", "#2", KEY_META,
			   "check/blacklist/#0", "FIRE", KEY_META, "check/blacklist/#1", "EARTH", KEY_META, "check/blacklist/#2", "ICE",
			   KEY_END);
	Key * k6 = keyNew ("user:/tests/blacklist/invalid2", KEY_VALUE, "FIRE", KEY_META, "check/blacklist", "#0", KEY_META,
			   "check/blacklist/#0", "FIRE", KEY_END);
	Key * k7 = keyNew ("user:/tests/blacklist/invalid3", KEY_VALUE, "COLD/WATER", KEY_META, "check/blacklist", "#0", KEY_META,
			   "check/blacklist/#0", "COLD/WATER", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (4, k1, k2, k3, k4, KS_END);
	PLUGIN_OPEN ("blacklist");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k5);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k6);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k7);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_blacklist_empty_values (void)
{
	Key * parentKey = keyNew ("user:/tests/blacklist", KEY_VALUE, "", KEY_END);

	// empty blacklist value
	Key * k1 = keyNew ("user:/tests/blacklist/valid1", KEY_VALUE, "ICE", KEY_META, "check/blacklist", "#0", KEY_META,
			   "check/blacklist/#0", "", KEY_END);
	Key * k2 = keyNew ("user:/tests/blacklist/invalid1", KEY_VALUE, "", KEY_META, "check/blacklist", "#0", KEY_META,
			   "check/blacklist/#0", "", KEY_END);
	// empty blacklist array - should not blacklist anything
	Key * k3 = keyNew ("user:/tests/blacklist/valid2", KEY_VALUE, "", KEY_META, "check/blacklist", "", KEY_END);
	// no blacklist meta-keys provided
	Key * k4 = keyNew ("user:/tests/blacklist/valid3", KEY_VALUE, "ICE", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (1, k1, KS_END);
	PLUGIN_OPEN ("blacklist");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k2);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k3);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k4);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_blacklist_null_values (void)
{
	Key * parentKey = keyNew ("user:/tests/blacklist", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/blacklist/valid1", KEY_VALUE, "", KEY_END);
	Key * k2 = keyNew ("user:/tests/blacklist/valid2", KEY_VALUE, NULL, KEY_END);
	Key * k3 = keyNew ("user:/tests/blacklist/valid3", KEY_VALUE, NULL, KEY_META, "check/blacklist", "", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (1, k1, KS_END);
	PLUGIN_OPEN ("blacklist");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k2);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k3);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("BLACKLIST     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_blacklist ();
	test_blacklist_empty_values ();
	test_blacklist_null_values ();


	print_result ("testmod_blacklist");

	return nbError;
}
