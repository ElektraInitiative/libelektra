/**
 * @file
 *
 * @brief Tests for ipaddr plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#include <tests_plugin.h>

#include <kdbconfig.h>

#ifdef HAVE_FEATURES_H

#include <features.h>
// The function `getaddrinfo` used in the `network` plugin leaks memory, if we use the default value for `ai_flags` on systems that use
// `glibc` 2.19.
// See also: https://travis-ci.org/ElektraInitiative/libelektra/builds/428298531
#if defined(__GLIBC__) && defined(__GLIBC_PREREQ)
#if !(__GLIBC_PREREQ(2, 20))
#include <string.h>
#define PLUGIN_LEAKS_MEMORY (strcmp (PLUGIN_NAME, "network") == 0)
#endif
#endif

#endif // HAVE_FEATURES_H

#ifndef PLUGIN_LEAKS_MEMORY
#define PLUGIN_LEAKS_MEMORY 0
#endif

static void test_length (void)
{
	Key * parentKey = keyNew ("user:/tests/length", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/length/valid1", KEY_VALUE, "value", KEY_META, "check/length", "10", KEY_END);
	Key * k2 = keyNew ("user:/tests/length/invalid1", KEY_VALUE, "waytoolongvalue", KEY_META, "check/length", "5", KEY_END);
	Key * k3 = keyNew ("user:/tests/length/edgecase1", KEY_VALUE, "edgy", KEY_META, "check/length", "4", KEY_END);
	Key * k4 = keyNew ("user:/tests/length/edgecase2", KEY_VALUE, "edgyy", KEY_META, "check/length", "4", KEY_END);
	Key * k5 = keyNew ("user:/tests/length/edgecase3", KEY_VALUE, "edg", KEY_META, "check/length", "4", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (1, k1, KS_END);
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

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
