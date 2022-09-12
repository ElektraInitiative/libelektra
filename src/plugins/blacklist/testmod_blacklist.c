/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbmodule.h>
#include <kdbplugin.h>
#include <tests_plugin.h>


static void test_blacklist (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/blacklist", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/blacklist/valid1", ELEKTRA_KEY_VALUE, "ICE", ELEKTRA_KEY_META, "check/blacklist", "#1", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "FIRE", ELEKTRA_KEY_META, "check/blacklist/#1", "EARTH", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/blacklist/valid2", ELEKTRA_KEY_VALUE, "EARTH", ELEKTRA_KEY_META, "check/blacklist", "#2", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "FIRE", ELEKTRA_KEY_META, "check/blacklist/#2", "WATER", ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/blacklist/valid3", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "check/blacklist", "#1", ELEKTRA_KEY_META, "check/blacklist/#0",
			   "FIRE", ELEKTRA_KEY_META, "check/blacklist/#1", "EARTH", ELEKTRA_KEY_END);
	ElektraKey * k4 = elektraKeyNew ("user:/tests/blacklist/valid4", ELEKTRA_KEY_VALUE, "WATER", ELEKTRA_KEY_META, "check/blacklist", "#0", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "COLD/WATER", ELEKTRA_KEY_END);
	ElektraKey * k5 = elektraKeyNew ("user:/tests/blacklist/invalid1", ELEKTRA_KEY_VALUE, "ICE", ELEKTRA_KEY_META, "check/blacklist", "#2", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "FIRE", ELEKTRA_KEY_META, "check/blacklist/#1", "EARTH", ELEKTRA_KEY_META, "check/blacklist/#2", "ICE",
			   ELEKTRA_KEY_END);
	ElektraKey * k6 = elektraKeyNew ("user:/tests/blacklist/invalid2", ELEKTRA_KEY_VALUE, "FIRE", ELEKTRA_KEY_META, "check/blacklist", "#0", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "FIRE", ELEKTRA_KEY_END);
	ElektraKey * k7 = elektraKeyNew ("user:/tests/blacklist/invalid3", ELEKTRA_KEY_VALUE, "COLD/WATER", ELEKTRA_KEY_META, "check/blacklist", "#0", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "COLD/WATER", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (4, k1, k2, k3, k4, ELEKTRA_KS_END);
	PLUGIN_OPEN ("blacklist");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k5);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k6);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k7);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_blacklist_empty_values (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/blacklist", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);

	// empty blacklist value
	ElektraKey * k1 = elektraKeyNew ("user:/tests/blacklist/valid1", ELEKTRA_KEY_VALUE, "ICE", ELEKTRA_KEY_META, "check/blacklist", "#0", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/blacklist/invalid1", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "check/blacklist", "#0", ELEKTRA_KEY_META,
			   "check/blacklist/#0", "", ELEKTRA_KEY_END);
	// empty blacklist array - should not blacklist anything
	ElektraKey * k3 = elektraKeyNew ("user:/tests/blacklist/valid2", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_META, "check/blacklist", "", ELEKTRA_KEY_END);
	// no blacklist meta-keys provided
	ElektraKey * k4 = elektraKeyNew ("user:/tests/blacklist/valid3", ELEKTRA_KEY_VALUE, "ICE", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (1, k1, ELEKTRA_KS_END);
	PLUGIN_OPEN ("blacklist");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k2);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k3);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet should have failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k4);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_blacklist_null_values (void)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/blacklist", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = elektraKeyNew ("user:/tests/blacklist/valid1", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("user:/tests/blacklist/valid2", ELEKTRA_KEY_VALUE, NULL, ELEKTRA_KEY_END);
	ElektraKey * k3 = elektraKeyNew ("user:/tests/blacklist/valid3", ELEKTRA_KEY_VALUE, NULL, ELEKTRA_KEY_META, "check/blacklist", "", ELEKTRA_KEY_END);

	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (1, k1, ELEKTRA_KS_END);
	PLUGIN_OPEN ("blacklist");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k2);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet should have failed");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	elektraKeysetAppendKey (ks, k3);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	elektraKeysetDel (ks);

	elektraKeyDel (parentKey);
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
