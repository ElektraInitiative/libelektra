/**
 * @file
 *
 * @brief Test cases for how to build a backend out of system/elektra/mountpoints/<name>
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <../../src/libs/elektra/backend.c>
#include <tests_internal.h>


KeySet * set_simple (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints/simple", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/path", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/errorplugins", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/errorplugins/#1" KDB_DEFAULT_STORAGE, KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/getplugins", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE, KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/anything", KEY_VALUE, "plugin",
			      KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/path", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/setplugins", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/setplugins/#1" KDB_DEFAULT_STORAGE, KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/errorplugins", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/errorplugins/#1" KDB_DEFAULT_STORAGE, KEY_END), KS_END);
}

KeySet * set_pluginconf (void)
{
	return ksNew (10, keyNew ("system/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system/more", KEY_END),
		      keyNew ("system/more/config", KEY_END), keyNew ("system/more/config/below", KEY_END), keyNew ("system/path", KEY_END),
		      keyNew ("user/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user/more", KEY_END),
		      keyNew ("user/more/config", KEY_END), keyNew ("user/more/config/below", KEY_END), keyNew ("user/path", KEY_END),
		      KS_END);
}

static void test_simple (void)
{
	printf ("Test simple building of backend\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * global = ksNew (0, KS_END);
	Key * errorKey = 0;
	Backend * backend = backendOpen (set_simple (), modules, global, errorKey);
	succeed_if (backend->errorplugins[0] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[2] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[3] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[4] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[5] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[6] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[7] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[8] == 0, "there should be no plugin");
	succeed_if (backend->errorplugins[9] == 0, "there should be no plugin");
	exit_if_fail (backend->errorplugins[1] != 0, "there should be a plugin");

	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[3] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[4] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[5] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[6] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[7] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[8] == 0, "there should be no plugin");
	succeed_if (backend->getplugins[9] == 0, "there should be no plugin");
	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");

	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[3] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[4] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[5] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[6] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[7] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[8] == 0, "there should be no plugin");
	succeed_if (backend->setplugins[9] == 0, "there should be no plugin");
	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");

	Key * mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "user/tests/backend/simple");
	succeed_if_same_string (keyString (mp), "simple");

	Plugin * plugin = backend->getplugins[1];

	KeySet * test_config = set_pluginconf ();
	KeySet * config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");

	backendClose (backend, errorKey);
	elektraModulesClose (modules, 0);
	ksDel (modules);
	ksDel (global);
}

static void test_default (void)
{
	printf ("Test default " KDB_DEFAULT_STORAGE "\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);


	Plugin * plugin = elektraPluginOpen (KDB_DEFAULT_STORAGE, modules, set_pluginconf (), 0);
	exit_if_fail (plugin, "KDB_DEFAULT_STORAGE: " KDB_DEFAULT_STORAGE " plugin could not be loaded");

	KeySet * test_config = set_pluginconf ();
	KeySet * config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");

	elektraPluginClose (plugin, 0);

	KeySet * global = ksNew (0, KS_END);
	Backend * backend = backendOpenDefault (modules, global, KDB_DB_FILE, 0);

	Key * mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "");
	succeed_if_same_string (keyString (mp), "default");

	backendClose (backend, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
	ksDel (global);
}


KeySet * set_backref (void)
{
	return ksNew (
		50, keyNew ("system/elektra/mountpoints/backref", KEY_END),

		keyNew ("system/elektra/mountpoints/backref/config", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/config/anything", KEY_VALUE, "backend", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/config/more", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/config/more/config", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/config/more/config/below", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/config/path", KEY_END),

		keyNew ("system/elektra/mountpoints/backref/errorplugins", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE "#", KEY_VALUE,
			"introduce reference", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE "#/config",
			KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE
			"#/config/anything",
			KEY_VALUE, "plugin", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE "#/config/more",
			KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE
			"#/config/more/config",
			KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE
			"#/config/more/config/below",
			KEY_END),
		keyNew ("system/elektra/mountpoints/backref/errorplugins/#1#" KDB_DEFAULT_STORAGE "#" KDB_DEFAULT_STORAGE "#/config/path",
			KEY_END),

		keyNew ("system/elektra/mountpoints/backref/getplugins", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/getplugins/#1#" KDB_DEFAULT_STORAGE, KEY_VALUE, "backend", KEY_END),

		keyNew ("system/elektra/mountpoints/backref/mountpoint", KEY_VALUE, "user/tests/backend/backref", KEY_END),

		keyNew ("system/elektra/mountpoints/backref/setplugins", KEY_END),
		keyNew ("system/elektra/mountpoints/backref/setplugins/#1#" KDB_DEFAULT_STORAGE, KEY_VALUE, "reference to other default",
			KEY_END),
		KS_END);
}

static void test_backref (void)
{
	printf ("Test back references\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	KeySet * global = ksNew (0, KS_END);
	Backend * backend = backendOpen (set_backref (), modules, global, 0);
	succeed_if (backend != 0, "there should be a backend");
	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");

	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");

	Key * mp;
	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "user/tests/backend/backref");
	succeed_if_same_string (keyString (mp), "backref");

	Plugin * plugin1 = backend->getplugins[1];
	Plugin * plugin2 = backend->setplugins[1];
	Plugin * plugin3 = backend->errorplugins[1];

	succeed_if (plugin1 != 0, "there should be a plugin");
	succeed_if (plugin2 != 0, "there should be a plugin");
	succeed_if (plugin3 != 0, "there should be a plugin");

	succeed_if (plugin1 == plugin2, "it should be the same plugin");
	succeed_if (plugin2 == plugin3, "it should be the same plugin");
	succeed_if (plugin1 == plugin3, "it should be the same plugin");

	succeed_if (plugin1->refcounter == 3, "ref counter should be 3");

	KeySet * test_config = set_pluginconf ();
	KeySet * config = elektraPluginGetConfig (plugin1);
	succeed_if (config != 0, "there should be a config");
	compare_keyset (config, test_config);
	ksDel (test_config);

	succeed_if (plugin1->kdbGet != 0, "no get pointer");
	succeed_if (plugin1->kdbSet != 0, "no set pointer");
	succeed_if (plugin2->kdbGet != 0, "no get pointer");
	succeed_if (plugin2->kdbSet != 0, "no set pointer");

	backendClose (backend, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
	ksDel (global);
}

int main (int argc, char ** argv)
{
	printf ("  BACKEND   TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_simple ();
	test_default ();
	test_backref ();

	printf ("\ntest_backend RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
