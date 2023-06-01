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

#include <internal/config.h>


static void test_length (void)
{
	Key * parentKey = keyNew ("user:/tests/length", KEY_VALUE, "", KEY_END);
	Key * lessThanMaxLen = keyNew ("user:/tests/length/valid1", KEY_VALUE, "value", KEY_META, "check/length/max", "10", KEY_END);
	Key * moreThanMaxLen =
		keyNew ("user:/tests/length/invalid1", KEY_VALUE, "waytoolongvalue", KEY_META, "check/length/max", "5", KEY_END);
	Key * equalToMaxLen = keyNew ("user:/tests/length/edgecase1", KEY_VALUE, "edgy", KEY_META, "check/length/max", "4", KEY_END);
	Key * moreThanMaxLenBy1 = keyNew ("user:/tests/length/edgecase2", KEY_VALUE, "edgyy", KEY_META, "check/length/max", "4", KEY_END);
	Key * lessThanMaxLenBy1 = keyNew ("user:/tests/length/edgecase3", KEY_VALUE, "edg", KEY_META, "check/length/max", "4", KEY_END);
	Key * lessThanMaxWithStringTerminator = keyNew ("user:/tests/length/edgecase3", KEY_BINARY, KEY_SIZE, 8, KEY_VALUE, "edg\0abc",
							KEY_META, "check/length/max", "10", KEY_END);
	Key * moreThanMaxWithStringTerminator = keyNew ("user:/tests/length/edgecase3", KEY_BINARY, KEY_SIZE, 8, KEY_VALUE, "edg\0abc",
							KEY_META, "check/length/max", "4", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (1, lessThanMaxLen, KS_END);
	PLUGIN_OPEN ("length");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, moreThanMaxLen);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should not fail");
	succeed_if (output_warnings (parentKey) == 0, "kdbGet should produce warnings");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, equalToMaxLen);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, moreThanMaxLenBy1);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should not fail");
	succeed_if (output_warnings (parentKey) == 0, "kdbGet should produce warnings");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, lessThanMaxLenBy1);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, lessThanMaxWithStringTerminator);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");

	ksAppendKey (ks, moreThanMaxWithStringTerminator);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet should not fail");
	succeed_if (output_warnings (parentKey) == 0, "kdbGet should produce warnings");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
