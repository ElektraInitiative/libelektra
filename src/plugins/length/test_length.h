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
	ElektraKey * parentKey = keyNew ("user:/tests/length", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);
	ElektraKey * k1 = keyNew ("user:/tests/length/valid1", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_META, "check/length/max", "10", ELEKTRA_KEY_END);
	ElektraKey * k2 = keyNew ("user:/tests/length/invalid1", ELEKTRA_KEY_VALUE, "waytoolongvalue", ELEKTRA_KEY_META, "check/length/max", "5", ELEKTRA_KEY_END);
	ElektraKey * k3 = keyNew ("user:/tests/length/edgecase1", ELEKTRA_KEY_VALUE, "edgy", ELEKTRA_KEY_META, "check/length/max", "4", ELEKTRA_KEY_END);
	ElektraKey * k4 = keyNew ("user:/tests/length/edgecase2", ELEKTRA_KEY_VALUE, "edgyy", ELEKTRA_KEY_META, "check/length/max", "4", ELEKTRA_KEY_END);
	ElektraKey * k5 = keyNew ("user:/tests/length/edgecase3", ELEKTRA_KEY_VALUE, "edg", ELEKTRA_KEY_META, "check/length/max", "4", ELEKTRA_KEY_END);
	ElektraKey * k6 = keyNew ("user:/tests/length/edgecase3", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, 8, ELEKTRA_KEY_VALUE, "edg\0abc", ELEKTRA_KEY_META, "check/length/max",
			   "10", ELEKTRA_KEY_END);
	ElektraKey * k7 = keyNew ("user:/tests/length/edgecase3", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, 8, ELEKTRA_KEY_VALUE, "edg\0abc", ELEKTRA_KEY_META, "check/length/max",
			   "4", ELEKTRA_KEY_END);

	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	ElektraKeyset * ks = ksNew (1, k1, ELEKTRA_KS_END);
	PLUGIN_OPEN ("length");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, ELEKTRA_KS_END);
	ksAppendKey (ks, k2);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, ELEKTRA_KS_END);
	ksAppendKey (ks, k3);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	ksDel (ks);

	ks = ksNew (20, ELEKTRA_KS_END);
	ksAppendKey (ks, k4);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, ELEKTRA_KS_END);
	ksAppendKey (ks, k5);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	ksDel (ks);

	ks = ksNew (20, ELEKTRA_KS_END);
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
