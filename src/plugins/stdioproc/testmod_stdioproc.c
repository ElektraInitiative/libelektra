/**
 * @file
 *
 * @brief Tests for stdioproc plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_success (void)
{
	printf ("test success\n");

	Key * parentKey = keyNew ("user:/tests/stdioproc", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/app", KEY_VALUE, bindir_file ("stdioproc-testapp.sh"), KEY_END), KS_END);
	PLUGIN_OPEN ("stdioproc");

	// replace config, after PLUGIN_OPEN so that we can test kdbOpen
	ksClear (plugin->config);
	ksAppend (plugin->config, ksNew (16, keyNew ("user:/app", KEY_VALUE, bindir_file ("stdioproc-testapp.sh"), KEY_END), KS_END));

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "open") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/stdioproc/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "get") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "get") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/stdioproc/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "set") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "set") == 0, "parent has wrong value");

	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "close") == 0, "parent has wrong value");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_error (void)
{
	printf ("test error\n");

	Key * parentKey = keyNew ("user:/tests/stdioproc", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/app", KEY_VALUE, bindir_file ("stdioproc-testapp.sh"), KEY_END), KS_END);
	PLUGIN_OPEN ("stdioproc");

	// replace config, after PLUGIN_OPEN so that we can test kdbOpen
	ksClear (plugin->config);
	ksAppend (plugin->config,
		  ksNew (16, keyNew ("user:/app", KEY_VALUE, bindir_file ("stdioproc-testapp.sh"), KEY_END),
			 keyNew ("user:/args", KEY_VALUE, "#0", KEY_END), keyNew ("user:/args/#0", KEY_VALUE, "error", KEY_END), KS_END));

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbOpen was not error");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "open") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbGet was not error");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/stdioproc/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "get") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "get") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbSet was not error");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/stdioproc/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "set") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "set") == 0, "parent has wrong value");

	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbClose was not error");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "close") == 0, "parent has wrong value");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_noupdate (void)
{
	printf ("test noupdate\n");

	Key * parentKey = keyNew ("user:/tests/stdioproc", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/app", KEY_VALUE, bindir_file ("stdioproc-testapp.sh"), KEY_END), KS_END);
	PLUGIN_OPEN ("stdioproc");

	// replace config, after PLUGIN_OPEN so that we can test kdbOpen
	ksClear (plugin->config);
	ksAppend (plugin->config, ksNew (16, keyNew ("user:/app", KEY_VALUE, bindir_file ("stdioproc-testapp.sh"), KEY_END),
					 keyNew ("user:/args", KEY_VALUE, "#0", KEY_END),
					 keyNew ("user:/args/#0", KEY_VALUE, "noupdate", KEY_END), KS_END));

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbOpen was not no_update");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "open") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not no_update");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/stdioproc/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "get") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "get") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not no_update");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/stdioproc/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "set") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "set") == 0, "parent has wrong value");

	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbClose was not no_update");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/stdioproc") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "close") == 0, "parent has wrong value");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("STDIOPROC     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_success ();
	test_error ();
	test_noupdate ();

	print_result ("testmod_stdioproc");

	return nbError;
}
