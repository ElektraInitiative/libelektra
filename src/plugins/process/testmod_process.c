/**
 * @file
 *
 * @brief Tests for process plugin
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

	Key * parentKey = keyNew ("user:/tests/process", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/executable", KEY_VALUE, bindir_file ("process-testapp.sh"), KEY_END), KS_END);
	PLUGIN_OPEN ("process");

	// replace config, after PLUGIN_OPEN so that we can test kdbOpen
	ksClear (plugin->config);
	KeySet * newConf = ksNew (16, keyNew ("user:/executable", KEY_VALUE, bindir_file ("process-testapp.sh"), KEY_END), KS_END);
	ksAppend (plugin->config, newConf);
	ksDel (newConf);

	// need to run close first, so we don't create memory leaks via double opn
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not successful");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "close") == 0, "parent has wrong value");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbOpen was not successful");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "open") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/process/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "get") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "get") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/process/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "set") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "set") == 0, "parent has wrong value");


	KeySet * expectedContract = ksNew (
		16, keyNew ("system:/elektra/modules/testapp/exports/close", KEY_FUNC, plugin->kdbClose, KEY_END),
		keyNew ("system:/elektra/modules/testapp/exports/get", KEY_FUNC, plugin->kdbGet, KEY_END),
		keyNew ("system:/elektra/modules/testapp/exports/open", KEY_FUNC, plugin->kdbOpen, KEY_END),
		keyNew ("system:/elektra/modules/testapp/exports/set", KEY_FUNC, plugin->kdbSet, KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos", KEY_VALUE, "Information about the process test plugin is in keys below",
			KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/author", KEY_VALUE, "Klemens Böswirth <k.boeswirth+git@gmail.com>", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/description", KEY_VALUE, "test plugin for process", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/licence", KEY_VALUE, "BSD", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/metadata", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/needs", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/placements", KEY_VALUE, "getstorage setstorage", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/provides", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/recommends", KEY_VALUE, "", KEY_END),
		keyNew ("system:/elektra/modules/testapp/infos/status", KEY_VALUE, "maintained tested/unit tested/shell experimental",
			KEY_END),
		KS_END);

	ksClear (ks);
	keySetName (parentKey, "system:/elektra/modules/testapp");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
		    "call to kdbGet (contract) was not successful");
	compare_keyset (ks, expectedContract);
	ksDel (expectedContract);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_error (void)
{
	printf ("test error\n");

	Key * parentKey = keyNew ("user:/tests/process", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/executable", KEY_VALUE, bindir_file ("process-testapp.sh"), KEY_END), KS_END);
	PLUGIN_OPEN ("process");

	// replace config, after PLUGIN_OPEN so that we can test kdbOpen
	ksClear (plugin->config);
	KeySet * newConf =
		ksNew (16, keyNew ("user:/executable", KEY_VALUE, bindir_file ("process-testapp.sh"), KEY_END),
		       keyNew ("user:/args", KEY_VALUE, "#0", KEY_END), keyNew ("user:/args/#0", KEY_VALUE, "error", KEY_END), KS_END);
	ksAppend (plugin->config, newConf);
	ksDel (newConf);

	// need to run close first, so we don't create memory leaks via double open
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not error");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "close") == 0, "parent has wrong value");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbOpen was not error");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "open") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbGet was not error");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/process/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "get") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "get") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "call to kdbSet was not error");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/process/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "set") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "set") == 0, "parent has wrong value");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_noupdate (void)
{
	printf ("test noupdate\n");

	Key * parentKey = keyNew ("user:/tests/process", KEY_END);
	KeySet * conf = ksNew (1, keyNew ("user:/executable", KEY_VALUE, bindir_file ("process-testapp.sh"), KEY_END), KS_END);
	PLUGIN_OPEN ("process");

	// replace config, after PLUGIN_OPEN so that we can test kdbOpen
	ksClear (plugin->config);
	KeySet * newConf =
		ksNew (16, keyNew ("user:/executable", KEY_VALUE, bindir_file ("process-testapp.sh"), KEY_END),
		       keyNew ("user:/args", KEY_VALUE, "#0", KEY_END), keyNew ("user:/args/#0", KEY_VALUE, "noupdate", KEY_END), KS_END);
	ksAppend (plugin->config, newConf);
	ksDel (newConf);

	// need to run close first, so we don't create memory leaks via double open
	succeed_if (plugin->kdbClose (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbClose was not no_update");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "close") == 0, "parent has wrong value");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbOpen was not no_update");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "open") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not no_update");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/process/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "get") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "get") == 0, "parent has wrong value");

	ksClear (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not no_update");
	succeed_if (ksGetSize (ks) == 1, "ks has wrong size");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyName (ksAtCursor (ks, 0)), "user:/tests/process/operation") == 0,
		    "key has wrong name");
	succeed_if (ksGetSize (ks) >= 1 && strcmp (keyString (ksAtCursor (ks, 0)), "set") == 0, "key has wrong value");
	succeed_if (strcmp (keyName (parentKey), "user:/tests/process") == 0, "parent has wrong name");
	succeed_if (strcmp (keyString (parentKey), "set") == 0, "parent has wrong value");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("PROCESS     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_success ();
	test_error ();
	test_noupdate ();

	print_result ("testmod_process");

	return nbError;
}
