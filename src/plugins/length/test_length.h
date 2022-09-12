/**
 * @file
 *
 * @brief Tests for length plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <tests_plugin.h>

#include <kdbconfig.h>


static void test_length (void)
{
	ElektraKey * parentKey = keyNew ("user:/tests/length", KEY_VALUE, "", KEY_END);
	ElektraKey * k1 = keyNew ("user:/tests/length/valid1", KEY_VALUE, "value", KEY_META, "check/length/max", "10", KEY_END);
	ElektraKey * k2 = keyNew ("user:/tests/length/invalid1", KEY_VALUE, "waytoolongvalue", KEY_META, "check/length/max", "5", KEY_END);
	ElektraKey * k3 = keyNew ("user:/tests/length/edgecase1", KEY_VALUE, "edgy", KEY_META, "check/length/max", "4", KEY_END);
	ElektraKey * k4 = keyNew ("user:/tests/length/edgecase2", KEY_VALUE, "edgyy", KEY_META, "check/length/max", "4", KEY_END);
	ElektraKey * k5 = keyNew ("user:/tests/length/edgecase3", KEY_VALUE, "edg", KEY_META, "check/length/max", "4", KEY_END);
	ElektraKey * k6 = keyNew ("user:/tests/length/edgecase3", KEY_BINARY, KEY_SIZE, 8, KEY_VALUE, "edg\0abc", KEY_META, "check/length/max",
			   "10", KEY_END);
	ElektraKey * k7 = keyNew ("user:/tests/length/edgecase3", KEY_BINARY, KEY_SIZE, 8, KEY_VALUE, "edg\0abc", KEY_META, "check/length/max",
			   "4", KEY_END);

	ElektraKeyset * conf = ksNew (0, KS_END);
	ElektraKeyset * ks = ksNew (1, k1, KS_END);
	PLUGIN_OPEN ("length");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k2);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k3);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k4);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k5);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k6);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");

	ksAppendKey (ks, k7);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
