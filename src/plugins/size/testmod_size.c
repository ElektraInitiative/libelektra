/**
 * @file
 *
 * @brief Tests for size plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void simple (const char * width, const char * height)
{
	char value[strlen (width) + strlen (height) + 2];
	snprintf (value, sizeof (value), "%sx%s", width, height);
	Key * parentKey = keyNew ("user/tests/size", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/size/key", KEY_VALUE, value, KEY_META, "transform/size", "", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("size");
	ksRewind (ks);
	plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Width", KDB_O_NONE)), width) == 0,
		    "transformation failed, Width doesn't exist");
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Height", KDB_O_NONE)), height) == 0,
		    "transformation failed, Height doesn't exist");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void metaConfig (const char * width, const char * height, const char * sep)
{
	char value[strlen (width) + strlen (height) + strlen (sep) + 2];
	snprintf (value, sizeof (value), "%s%s%s", width, sep, height);
	Key * parentKey = keyNew ("user/tests/size", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/size/key", KEY_VALUE, value, KEY_META, "transform/size", sep, KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("size");
	ksRewind (ks);
	plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Width", KDB_O_NONE)), width) == 0,
		    "transformation failed, Width doesn't exist");
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Height", KDB_O_NONE)), height) == 0,
		    "transformation failed, Height doesn't exist");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void pluginConfig (const char * width, const char * height, const char * sep)
{
	char value[strlen (width) + strlen (height) + strlen (sep) + 2];
	snprintf (value, sizeof (value), "%s%s%s", width, sep, height);
	Key * parentKey = keyNew ("user/tests/size", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/size/key", KEY_VALUE, value, KEY_META, "transform/size", "", KEY_END), KS_END);
	KeySet * conf = ksNew (3, keyNew ("user/separator", KEY_VALUE, sep, KEY_END), KS_END);
	PLUGIN_OPEN ("size");
	ksRewind (ks);
	plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Width", KDB_O_NONE)), width) == 0,
		    "transformation failed, Width doesn't exist");
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Height", KDB_O_NONE)), height) == 0,
		    "transformation failed, Height doesn't exist");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testPriorityMeta (void)
{
	Key * parentKey = keyNew ("user/tests/size", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/size/key", KEY_VALUE, "20:4", KEY_META, "transform/size", ":", KEY_END), KS_END);
	KeySet * conf = ksNew (3, keyNew ("user/separator", KEY_VALUE, "-", KEY_END), KS_END);
	PLUGIN_OPEN ("size");
	ksRewind (ks);
	plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Width", KDB_O_NONE)), "20") == 0,
		    "transformation failed, Width doesn't exist");
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Height", KDB_O_NONE)), "4") == 0,
		    "transformation failed, Height doesn't exist");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testPriorityPlugin (void)
{
	Key * parentKey = keyNew ("user/tests/size", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/size/key", KEY_VALUE, "20:4", KEY_META, "transform/size", "", KEY_END), KS_END);
	KeySet * conf = ksNew (3, keyNew ("user/separator", KEY_VALUE, ":", KEY_END), KS_END);
	PLUGIN_OPEN ("size");
	ksRewind (ks);
	plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Width", KDB_O_NONE)), "20") == 0,
		    "transformation failed, Width doesn't exist");
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Height", KDB_O_NONE)), "4") == 0,
		    "transformation failed, Height doesn't exist");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testCleanup (void)
{
	Key * parentKey = keyNew ("user/tests/size", KEY_VALUE, "", KEY_END);
	KeySet * ks = ksNew (10, keyNew ("user/tests/size/key", KEY_VALUE, "20x4", KEY_META, "transform/size", "", KEY_END), KS_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("size");
	ksRewind (ks);
	plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Width", KDB_O_NONE)), "20") == 0,
		    "transformation failed, Width doesn't exist");
	succeed_if (strcmp (keyString (ksLookupByName (ks, "user/tests/size/key/Height", KDB_O_NONE)), "4") == 0,
		    "transformation failed, Height doesn't exist");
	plugin->kdbSet (plugin, ks, parentKey);
	succeed_if (!ksLookupByName (ks, "/tests/size/key/Width", KDB_O_NONE), "cleanup failed");
	succeed_if (!ksLookupByName (ks, "/tests/size/key/Height", KDB_O_NONE), "cleanup failed");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("SIZE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	simple ("20", "4");
	simple ("4", "20");

	metaConfig ("20", "4", "x");
	metaConfig ("20", "4", "/x\\/x\\");
	metaConfig ("20", "4", "-:-");

	pluginConfig ("20", "4", "x");
	pluginConfig ("20", "4", "/x\\/x\\");
	pluginConfig ("20", "4", "-:-");

	testPriorityMeta ();
	testPriorityPlugin ();

	testCleanup ();

	printf ("\ntestmod_size RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
