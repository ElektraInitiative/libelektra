/**
 * @file
 *
 * @brief Tests for backend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "backendprivate_odbc.h"
#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

// FIXME [new_backend]: tests disabled
/*
KeySet * set_simple (void)
{
	return ksNew (50, keyNew ("system:/elektra/mountpoints/simple", KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/more", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/more/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/mountpoint", KEY_VALUE, "user:/tests/backend/simple", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/path", KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/error", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/error/prerollback", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/error/prerollback/#1", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/error/prerollback/#1/name", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/get", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/anything", KEY_VALUE, "plugin", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/more", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/more/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/more/config/below", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/path", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/name", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/set", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/set/presetstorage", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/set/presetstorage/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/set/presetstorage/#0/name", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),
		      KS_END);
}

KeySet * set_default (void)
{
	return ksNew (27, keyNew ("system:/elektra/mountpoints/default", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/config/mountpoint", KEY_VALUE, "user:/tests/backend/default", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/config/path", KEY_VALUE, KDB_DB_FILE, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/error", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/error/rollback", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/error/rollback/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/error/rollback/#0/label", KEY_VALUE, KDB_RESOLVER, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/error/rollback/#0/name", KEY_VALUE, KDB_RESOLVER, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getresolver", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getresolver/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getresolver/#0/reference", KEY_VALUE, KDB_RESOLVER, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getstorage", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getstorage/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getstorage/#0/label", KEY_VALUE, KDB_STORAGE, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/get/getstorage/#0/name", KEY_VALUE, KDB_STORAGE, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/commit", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/commit/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/commit/#0/reference", KEY_VALUE, KDB_RESOLVER, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/setresolver", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/setresolver/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/setresolver/#0/reference", KEY_VALUE, KDB_RESOLVER, KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/setstorage", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/setstorage/#0", KEY_END),
		      keyNew ("system:/elektra/mountpoints/default/set/setstorage/#0/reference", KEY_VALUE, KDB_STORAGE, KEY_END), KS_END);
}

Plugin * open_backend (KeySet * config, KeySet * modules, KeySet * global, Key * errorKey)
{
	Plugin * backend = elektraPluginOpen ("backend", modules, ksDup (config), errorKey);
	output_error (errorKey);
	output_warnings (errorKey);

	if (backend != 0)
	{
		backend->global = global;
	}

	ksDel (config);

	return backend;
}


KeySet * set_simpleconf (void)
{
	return ksNew (10, keyNew ("system:/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system:/more", KEY_END),
		      keyNew ("system:/more/config", KEY_END), keyNew ("system:/more/config/below", KEY_END),
		      keyNew ("system:/mountpoint", KEY_VALUE, "user:/tests/backend/simple", KEY_END), keyNew ("system:/path", KEY_END),
		      keyNew ("user:/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user:/more", KEY_END),
		      keyNew ("user:/more/config", KEY_END), keyNew ("user:/more/config/below", KEY_END), keyNew ("user:/path", KEY_END),
		      KS_END);
}

KeySet * set_backrefconf (void)
{
	return ksNew (10, keyNew ("system:/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system:/more", KEY_END),
		      keyNew ("system:/more/config", KEY_END), keyNew ("system:/more/config/below", KEY_END),
		      keyNew ("system:/mountpoint", KEY_VALUE, "user:/tests/backend/backref", KEY_END), keyNew ("system:/path", KEY_END),
		      keyNew ("user:/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user:/more", KEY_END),
		      keyNew ("user:/more/config", KEY_END), keyNew ("user:/more/config/below", KEY_END), keyNew ("user:/path", KEY_END),
		      KS_END);
}

KeySet * set_defaultconf (void)
{
	return ksNew (10, keyNew ("system:/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system:/more", KEY_END),
		      keyNew ("system:/more/config", KEY_END), keyNew ("system:/more/config/below", KEY_END),
		      keyNew ("system:/mountpoint", KEY_VALUE, "user:/tests/backend/default", KEY_END), keyNew ("system:/path", KEY_END),
		      keyNew ("user:/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user:/more", KEY_END),
		      keyNew ("user:/more/config", KEY_END), keyNew ("user:/more/config/below", KEY_END), keyNew ("user:/path", KEY_END),
		      KS_END);
}

int check_null_in_slot (Slot * slot, int index)
{
	if (!slot) return 1;

	Slot * curSlot = slot;

	for (int a = 0; a <= index; a++)
	{
		if (!curSlot)
		{
			return 1;
		}
		if (a == index)
		{
			if (curSlot->value)
			{
				return 0;
			}
			return 1;
		}

		curSlot = curSlot->next;
	}

	return 1;
}

static void test_simple (void)
{
	printf ("Test simple building of backend\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * global = ksNew (0, KS_END);
	Key * errorKey = keyNew ("/", KEY_END);
	Plugin * backend = open_backend (set_simple (), modules, global, errorKey);

	exit_if_fail (backend != 0, "no backend found");

	BackendHandle * bh = elektraPluginGetData (backend);

	exit_if_fail (bh != 0, "no backend handle found");
	succeed_if (check_null_in_slot (bh->errorplugins[0], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[0], 2), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[1], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[2], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[2], 1), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[2], 0), "there should be no plugin");
	exit_if_fail (check_null_in_slot (bh->errorplugins[0], 1) == 0, "there should be a plugin");

	succeed_if (check_null_in_slot (bh->getplugins[0], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->getplugins[1], 1), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->getplugins[1], 2), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->getplugins[2], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->getplugins[3], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->getplugins[3], 1), "there should be no plugin");
	exit_if_fail (check_null_in_slot (bh->getplugins[1], 0) == 0, "there should be a plugin");

	succeed_if (check_null_in_slot (bh->setplugins[0], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[1], 1), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[1], 2), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[2], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[2], 1), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[3], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[3], 1), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[4], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[4], 1), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[5], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[5], 1), "there should be no plugin");
	exit_if_fail (check_null_in_slot (bh->setplugins[1], 0) == 0, "there should be a plugin");

	Key * mp;
	succeed_if ((mp = bh->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "user:/tests/backend/simple");
	succeed_if_same_string (keyString (mp), "simple");

	Plugin * plugin = bh->getplugins[1]->value;

	KeySet * test_config = set_simpleconf ();
	KeySet * config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");

	elektraPluginClose (backend, errorKey);

	elektraModulesClose (modules, 0);
	ksDel (modules);

	ksDel (global);

	keyDel (errorKey);
}

static void test_default (void)
{
	printf ("Test default " KDB_DEFAULT_STORAGE "\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Plugin * plugin = elektraPluginOpen (KDB_DEFAULT_STORAGE, modules, set_defaultconf (), 0);
	exit_if_fail (plugin, "KDB_DEFAULT_STORAGE: " KDB_DEFAULT_STORAGE " plugin could not be loaded");

	KeySet * test_config = set_defaultconf ();
	KeySet * config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");

	elektraPluginClose (plugin, 0);

	KeySet * global = ksNew (0, KS_END);
	KeySet * defaultKeyset = set_default ();
	Plugin * backend = open_backend (defaultKeyset, modules, global, 0);

	exit_if_fail (backend != 0, "no backend found");

	BackendHandle * bh = elektraPluginGetData (backend);

	succeed_if (bh != 0, "no backend handle found");

	Key * mp;
	succeed_if ((mp = bh->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "user:/tests/backend/default");
	succeed_if_same_string (keyString (mp), "default");

	elektraPluginClose (backend, 0);
	elektraModulesClose (modules, 0);

	ksDel (modules);
	ksDel (global);
}


KeySet * set_backref (void)
{
	return ksNew (
		50, keyNew ("system:/elektra/mountpoints/backref", KEY_END),

		keyNew ("system:/elektra/mountpoints/backref/config", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/config/more", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/config/more/config", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/config/more/config/below", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/config/mountpoint", KEY_VALUE, "user:/tests/backend/backref", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/config/path", KEY_END),

		keyNew ("system:/elektra/mountpoints/backref/error", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/config", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/config/anything", KEY_VALUE, "plugin", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/config/more", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/config/more/config", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/config/more/config/below", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/config/path", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/label", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/error/prerollback/#0/name", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),

		keyNew ("system:/elektra/mountpoints/backref/get", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/get/pregetstorage", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/get/pregetstorage/#0", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/get/pregetstorage/#0/reference", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),

		keyNew ("system:/elektra/mountpoints/backref/set", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/set/presetstorage", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/set/presetstorage/#0", KEY_END),
		keyNew ("system:/elektra/mountpoints/backref/set/presetstorage/#0/reference", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),
		KS_END);
}

static void test_backref (void)
{
	printf ("Test back references\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * global = ksNew (0, KS_END);
	Plugin * backend = open_backend (set_backref (), modules, global, 0);

	exit_if_fail (backend != 0, "there should be a backend");

	BackendHandle * bh = elektraPluginGetData (backend);

	exit_if_fail (bh != 0, "no backend handle found");
	succeed_if (check_null_in_slot (bh->getplugins[0], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->getplugins[0], 1), "there should be no plugin");
	exit_if_fail (check_null_in_slot (bh->getplugins[1], 0) == 0, "there should be a plugin");
	succeed_if (check_null_in_slot (bh->getplugins[1], 1), "there should be no plugin");

	succeed_if (check_null_in_slot (bh->setplugins[0], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->setplugins[0], 1), "there should be no plugin");
	exit_if_fail (check_null_in_slot (bh->setplugins[1], 0) == 0, "there should be a plugin");
	succeed_if (check_null_in_slot (bh->setplugins[1], 1), "there should be no plugin");

	succeed_if (check_null_in_slot (bh->errorplugins[0], 1), "there should be no plugin");
	exit_if_fail (check_null_in_slot (bh->errorplugins[0], 0) == 0, "there should be a plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[1], 0), "there should be no plugin");
	succeed_if (check_null_in_slot (bh->errorplugins[1], 1), "there should be no plugin");

	Key * mp;
	succeed_if ((mp = bh->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "user:/tests/backend/backref");
	succeed_if_same_string (keyString (mp), "backref");

	Plugin * plugin1 = bh->getplugins[1]->value;
	Plugin * plugin2 = bh->setplugins[1]->value;
	Plugin * plugin3 = bh->errorplugins[0]->value;

	succeed_if (plugin1 != 0, "there should be a plugin");
	succeed_if (plugin2 != 0, "there should be a plugin");
	succeed_if (plugin3 != 0, "there should be a plugin");

	succeed_if (plugin1 == plugin2, "it should be the same plugin");
	succeed_if (plugin2 == plugin3, "it should be the same plugin");
	succeed_if (plugin1 == plugin3, "it should be the same plugin");

	succeed_if (plugin1->refcounter == 3, "ref counter should be 3");

	KeySet * test_config = set_backrefconf ();
	KeySet * config = elektraPluginGetConfig (plugin1);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin1->kdbGet != 0, "no get pointer");
	succeed_if (plugin1->kdbSet != 0, "no set pointer");
	succeed_if (plugin2->kdbGet != 0, "no get pointer");
	succeed_if (plugin2->kdbSet != 0, "no set pointer");

	elektraPluginClose (backend, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
	ksDel (global);
}
*/

int main (int argc, char ** argv)
{
	printf ("BACKEND     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	// FIXME [new_backend]: tests disabled
	/*
		test_simple ();
		test_default ();
		test_backref ();
	*/
	print_result ("testmod_backend");

	return nbError;
}
