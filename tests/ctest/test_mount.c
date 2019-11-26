/**
 * @file
 *
 * @brief Test suite for mounting related issues.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <../../src/libs/elektra/backend.c>
#include <../../src/libs/elektra/mount.c>
#include <../../src/libs/elektra/split.c>
#include <../../src/libs/elektra/trie.c>
#include <tests_internal.h>

KDB * kdb_new (void)
{
	KDB * kdb = elektraCalloc (sizeof (KDB));
	kdb->split = splitNew ();
	return kdb;
}

static void kdb_del (KDB * kdb)
{
	elektraPluginClose (kdb->defaultBackend, 0);
	if (kdb->initBackend)
	{
		elektraPluginClose (kdb->initBackend, 0);
	}
	trieClose (kdb->trie, 0);
	splitDel (kdb->split);

	elektraFree (kdb);
}

KeySet * modules_config (void)
{
	return ksNew (5, keyNew ("system/elektra/modules", KEY_END), KS_END);
}

KeySet * set_simple (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints/simple", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/path", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/error", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/error/prerollback", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/error/prerollback/#1", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/error/prerollback/#1/name", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/get", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/config/anything", KEY_VALUE, "plugin", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/config/path", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/get/pregetstorage/#0/name", KEY_VALUE, KDB_DEFAULT_STORAGE, KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/set", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/set/presetstorage", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/set/presetstorage/#0", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/set/presetstorage/#0/name", KDB_DEFAULT_STORAGE, KEY_END),
		      KS_END);
}

static void test_mount (void)
{
	printf ("test mount backend\n");

	KDB * kdb = kdb_new ();
	Plugin * backend = elektraPluginOpen ("simple", modules_config (), set_simple (), 0);
	mountBackend (kdb, backend, 0);
	succeed_if (kdb->trie, "there should be a trie");

	Key * mp = keyNew ("system/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END);
	Key * sk = keyNew ("user", KEY_VALUE, "user", KEY_END);

	Plugin * tempBackend = mountGetBackend (kdb, sk);
	compare_key (backendGetMountpoint (tempBackend), mp);
	compare_key (mountGetMountpoint (kdb, sk), mp);

	keySetName (sk, "user/below");
	tempBackend = mountGetBackend (kdb, sk);
	compare_key (backendGetMountpoint (tempBackend), mp);
	compare_key (mountGetMountpoint (kdb, sk), mp);

	keySetName (sk, "system");
//	kdb->defaultBackend = b_new ("", "default");
	succeed_if (mountGetBackend (kdb, sk) == kdb->defaultBackend, "did not return default backend");

	keySetName (mp, "");
	keySetString (mp, "default");
	compare_key (backendGetMountpoint (mountGetBackend (kdb, sk)), mp);
	compare_key (mountGetMountpoint (kdb, sk), mp);

	keyDel (sk);
	keyDel (mp);

	kdb_del (kdb);
}

KeySet * minimal_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), KS_END);
}


static void test_minimaltrie (void)
{
	printf ("Test minimal mount\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew (0);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, minimal_config (), modules, errorKey) == 0, "could not open minimal config")

		succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	succeed_if (!kdb->trie, "minimal trie is null");

	keyDel (errorKey);
	ksDel (modules);
	kdb_del (kdb);
}

KeySet * simple_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END), KS_END);
}

static void test_simple (void)
{
	printf ("Test simple mount\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew (0);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, simple_config (), modules, errorKey) == 0, "could not open trie");

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	exit_if_fail (kdb->trie, "kdb->trie was not build up successfully");

	Key * searchKey = keyNew ("user", KEY_END);
	Plugin * backend = trieLookup (kdb->trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	keySetName (searchKey, "user/tests/simple");
	backend = trieLookup (kdb->trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	keySetName (searchKey, "user/tests/simple/below");
	Plugin * b2 = trieLookup (kdb->trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	keySetName (searchKey, "user/tests/simple/deep/below");
	b2 = trieLookup (kdb->trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	keyDel (errorKey);
	ksDel (modules);
	keyDel (mp);
	keyDel (searchKey);
	kdb_del (kdb);
}

KeySet * set_pluginconf (void)
{
	return ksNew (10, keyNew ("system/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system/more", KEY_END),
		      keyNew ("system/more/config", KEY_END), keyNew ("system/more/config/below", KEY_END),
		      keyNew ("system/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END), keyNew ("system/path", KEY_END),
		      keyNew ("user/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user/more", KEY_END),
		      keyNew ("user/more/config", KEY_END), keyNew ("user/more/config/below", KEY_END), keyNew ("user/path", KEY_END),
		      KS_END);
}

static void test_simpletrie (void)
{
//	printf ("Test simple mount with plugins\n");
//
//	KDB * kdb = kdb_new ();
//	KeySet * modules = ksNew (0, KS_END);
//	elektraModulesInit (modules, 0);
//
//	KeySet * config = set_simple ();
//	ksAppendKey (config, keyNew ("system/elektra/mountpoints", KEY_END));
//	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
//
//	Key * key = keyNew ("user/tests/backend/simple", KEY_END);
//	Backend * backend = trieLookup (kdb->trie, key);
//
//	keyAddBaseName (key, "somewhere");
//	keyAddBaseName (key, "deep");
//	keyAddBaseName (key, "below");
//	Backend * backend2 = trieLookup (kdb->trie, key);
//	succeed_if (backend == backend2, "should be same backend");
//
//	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
//	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
//	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");
//
//	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
//	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
//	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");
//
//	Key * mp = backend->mountpoint;
//	succeed_if (mp, "no mountpoint found");
//	if (mp)
//	{
//		succeed_if_same_string (keyName (mp), "user/tests/backend/simple");
//		succeed_if_same_string (keyString (mp), "simple");
//	}
//
//	Plugin * plugin = backend->getplugins[1];
//
//	KeySet * test_config = set_pluginconf ();
//	KeySet * cconfig = elektraPluginGetConfig (plugin);
//	succeed_if (cconfig != 0, "there should be a config");
//	compare_keyset (cconfig, test_config);
//	ksDel (test_config);
//
//	succeed_if (plugin->kdbGet != 0, "no get pointer");
//	succeed_if (plugin->kdbSet != 0, "no set pointer");
//
//	keyDel (key);
//	kdb_del (kdb);
//	ksDel (modules);
}


KeySet * set_two (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/simple", KEY_END),

		      keyNew ("system/elektra/mountpoints/simple/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/config/path", KEY_END),

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


		      keyNew ("system/elektra/mountpoints/two", KEY_END),

		      keyNew ("system/elektra/mountpoints/two/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/config/anything", KEY_VALUE, "backend", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/config/path", KEY_END),

		      keyNew ("system/elektra/mountpoints/two/getplugins", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE, KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/anything", KEY_VALUE, "plugin",
			      KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/path", KEY_END),

		      keyNew ("system/elektra/mountpoints/two/mountpoint", KEY_VALUE, "user/tests/backend/two", KEY_END),

		      keyNew ("system/elektra/mountpoints/two/setplugins", KEY_END),
		      keyNew ("system/elektra/mountpoints/two/setplugins/#1" KDB_DEFAULT_STORAGE, KEY_END),
		      keyNew ("system/elektra/mountpoints/two/setplugins/#2" KDB_DEFAULT_STORAGE, KEY_END), KS_END);
}

static void test_two (void)
{
//	printf ("Test two mounts\n");
//
//	KDB * kdb = kdb_new ();
//	KeySet * modules = ksNew (0, KS_END);
//	elektraModulesInit (modules, 0);
//
//	KeySet * config = set_two ();
//	ksAppendKey (config, keyNew ("system/elektra/mountpoints", KEY_END));
//	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
//
//	Key * key = keyNew ("user/tests/backend/simple", KEY_END);
//	Backend * backend = trieLookup (kdb->trie, key);
//
//	keyAddBaseName (key, "somewhere");
//	keyAddBaseName (key, "deep");
//	keyAddBaseName (key, "below");
//	Backend * backend2 = trieLookup (kdb->trie, key);
//	succeed_if (backend == backend2, "should be same backend");
//
//	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
//	exit_if_fail (backend->getplugins[1] != 0, "there should be a plugin");
//	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");
//
//	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
//	exit_if_fail (backend->setplugins[1] != 0, "there should be a plugin");
//	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");
//
//	Key * mp;
//	succeed_if ((mp = backend->mountpoint) != 0, "no mountpoint found");
//	succeed_if_same_string (keyName (mp), "user/tests/backend/simple");
//	succeed_if_same_string (keyString (mp), "simple");
//
//	Plugin * plugin = backend->getplugins[1];
//
//	KeySet * test_config = set_pluginconf ();
//	KeySet * cconfig = elektraPluginGetConfig (plugin);
//	succeed_if (cconfig != 0, "there should be a config");
//	compare_keyset (cconfig, test_config);
//	ksDel (test_config);
//
//	succeed_if (plugin->kdbGet != 0, "no get pointer");
//	succeed_if (plugin->kdbSet != 0, "no set pointer");
//
//	keySetName (key, "user/tests/backend/two");
//	Backend * two = trieLookup (kdb->trie, key);
//	succeed_if (two != backend, "should be differnt backend");
//
//	succeed_if ((mp = two->mountpoint) != 0, "no mountpoint found");
//	succeed_if_same_string (keyName (mp), "user/tests/backend/two");
//	succeed_if_same_string (keyString (mp), "two");
//
//	keyDel (key);
//	elektraModulesClose (modules, 0);
//	ksDel (modules);
//	kdb_del (kdb);
}


KeySet * set_us (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/user", KEY_END),
		      keyNew ("system/elektra/mountpoints/user/mountpoint", KEY_VALUE, "user", KEY_END),
		      keyNew ("system/elektra/mountpoints/system", KEY_END),
		      keyNew ("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END), KS_END);
}

static void test_us (void)
{
//	printf ("Test mounting of user and system backends\n");
//
//	KDB * kdb = kdb_new ();
//	KeySet * modules = ksNew (0, KS_END);
//	elektraModulesInit (modules, 0);
//
//	KeySet * config = set_us ();
//	ksAppendKey (config, keyNew ("system/elektra/mountpoints", KEY_END));
//	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
//
//	Key * key = keyNew ("user/anywhere/backend/simple", KEY_END);
//	Backend * backend = trieLookup (kdb->trie, key);
//
//	keyAddBaseName (key, "somewhere");
//	keyAddBaseName (key, "deep");
//	keyAddBaseName (key, "below");
//	Backend * backend2 = trieLookup (kdb->trie, key);
//	succeed_if (backend == backend2, "should be same backend");
//
//	succeed_if (backend->getplugins[0] == 0, "there should be no plugin");
//	exit_if_fail (backend->getplugins[1] == 0, "there should be no plugin");
//	succeed_if (backend->getplugins[2] == 0, "there should be no plugin");
//
//	succeed_if (backend->setplugins[0] == 0, "there should be no plugin");
//	exit_if_fail (backend->setplugins[1] == 0, "there should be no plugin");
//	succeed_if (backend->setplugins[2] == 0, "there should be no plugin");
//
//	Key * mp = backend->mountpoint;
//	succeed_if (mp, "no mountpoint found");
//	if (mp)
//	{
//		succeed_if_same_string (keyName (mp), "user");
//		succeed_if_same_string (keyString (mp), "user");
//	}
//
//	keySetName (key, "system/anywhere/tests/backend/two");
//	Backend * two = trieLookup (kdb->trie, key);
//	succeed_if (two != backend, "should be differnt backend");
//
//	succeed_if ((mp = two->mountpoint) != 0, "no mountpoint found");
//	succeed_if_same_string (keyName (mp), "system");
//	succeed_if_same_string (keyString (mp), "system");
//
//	keyDel (key);
//	elektraModulesClose (modules, 0);
//	ksDel (modules);
//	kdb_del (kdb);
}

KeySet * endings_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/slash", KEY_END),
		      keyNew ("system/elektra/mountpoints/slash/mountpoint", KEY_VALUE, "user/endings", KEY_END),
		      keyNew ("system/elektra/mountpoints/hash", KEY_END),
		      keyNew ("system/elektra/mountpoints/hash/mountpoint", KEY_VALUE, "user/endings#", KEY_END),
		      keyNew ("system/elektra/mountpoints/space", KEY_END),
		      keyNew ("system/elektra/mountpoints/space/mountpoint", KEY_VALUE, "user/endings ", KEY_END),
		      keyNew ("system/elektra/mountpoints/endings", KEY_END),
		      keyNew ("system/elektra/mountpoints/endings/mountpoint", KEY_VALUE, "user/endings\200", KEY_END), KS_END);
}

static void test_endings (void)
{
//	printf ("Test mounting with different endings\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, endings_config (), modules, errorKey) == 0, "could not open mount");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "kdb->trie was not build up successfully");
//
//	Key * searchKey = keyNew ("user", KEY_END);
//	Backend * backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (!backend, "there should be no backend");
//
//
//	Key * mp = keyNew ("user/endings", KEY_VALUE, "slash", KEY_END);
//	keySetName (searchKey, "user/endings");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/endings#");
//	keySetName (mp, "user/endings#");
//	keySetString (mp, "hash");
//	Backend * b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend != b2, "should be other backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/endings/_");
//	keySetName (mp, "user/endings");
//	keySetString (mp, "slash");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be the same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/endings/X");
//	keySetName (mp, "user/endings");
//	keySetString (mp, "slash");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be the same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/endings_");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//
//	keySetName (searchKey, "user/endingsX");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//
//	keySetName (searchKey, "user/endings!");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//
//	keySetName (searchKey, "user/endings ");
//	keySetName (mp, "user/endings ");
//	keySetString (mp, "space");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend != b2, "should be other backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	keySetName (searchKey, "user/endings\200");
//	keySetName (mp, "user/endings\200");
//	keySetString (mp, "endings");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend != b2, "should be other backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	// output_trie(trie);
//
//	keyDel (errorKey);
//	ksDel (modules);
//	keyDel (mp);
//	keyDel (searchKey);
//	kdb_del (kdb);
}

KeySet * oldroot_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END), KS_END);
}

static void test_oldroot (void)
{
//	printf ("Test mounting with old root\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, oldroot_config (), modules, errorKey) == 0, "root should be mounted as default");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "trie was not build up successfully");
//
//	Key * searchKey = keyNew ("user", KEY_END);
//	Key * rmp = keyNew ("", KEY_VALUE, "root", KEY_END);
//	Backend * backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (!backend, "there should be no root backend");
//
//
//	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
//	keySetName (searchKey, "user/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/below");
//	Backend * b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	keyDel (mp);
//	keyDel (rmp);
//
//	keyDel (searchKey);
//
//	kdb_del (kdb);
//	keyDel (errorKey);
//	ksDel (modules);
}

KeySet * cascading_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "/tests/simple", KEY_END), KS_END);
}

static void test_cascading (void)
{
//	printf ("Test simple mount with cascading\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, cascading_config (), modules, errorKey) == 0, "could not open trie");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "kdb->trie was not build up successfully");
//
//	// output_trie (kdb->trie);
//
//	Key * searchKey = keyNew ("user", KEY_END);
//	Backend * backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (!backend, "there should be no backend");
//
//	keySetName (searchKey, "system");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (!backend, "there should be no backend");
//
//
//	Key * mp = keyNew ("/tests/simple", KEY_VALUE, "simple", KEY_END);
//
//	keySetName (searchKey, "user/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/below");
//	Backend * b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "system/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//	keySetName (searchKey, "system/tests/simple/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "system/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keyDel (errorKey);
//	ksDel (modules);
//	keyDel (mp);
//	keyDel (searchKey);
//	kdb_del (kdb);
}


KeySet * root_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END), KS_END);
}

static void test_root (void)
{
//	printf ("Test mounting with root\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "trie was not build up successfully");
//
//	Key * searchKey = keyNew ("", KEY_END);
//	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
//	Backend * b2 = 0;
//
//	keySetName (searchKey, "user");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	if (b2) compare_key (b2->mountpoint, rmp);
//
//
//	Backend * backend = 0;
//	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
//	keySetName (searchKey, "user/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	keyDel (mp);
//	keyDel (rmp);
//
//	keyDel (searchKey);
//
//	kdb_del (kdb);
//	keyDel (errorKey);
//	ksDel (modules);
}

static void test_default (void)
{
//	printf ("Test mounting with default\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");
//	succeed_if (mountDefault (kdb, modules, 1, errorKey) == 0, "could not mount default backend");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "trie was not build up successfully");
//
//	// output_trie (kdb->trie);
//
//	Key * searchKey = keyNew ("", KEY_END);
//	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
//	Backend * b2 = 0;
//
//	keySetName (searchKey, "user");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	if (b2) compare_key (b2->mountpoint, rmp);
//
//
//	Backend * backend = 0;
//	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
//	keySetName (searchKey, "user/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
//	keySetName (searchKey, "system/elektra");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keySetName (searchKey, "system/elektra/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keyDel (dmp);
//	keyDel (mp);
//	keyDel (rmp);
//
//	keyDel (searchKey);
//
//	kdb_del (kdb);
//	keyDel (errorKey);
//	ksDel (modules);
}

static void test_init (void)
{
//	printf ("Test mounting with init (no config)\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, ksNew (5, KS_END), modules, errorKey) == 0, "could not buildup mount");
//	succeed_if (mountDefault (kdb, modules, 0, errorKey) == 0, "could not mount default backend");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "trie was not build up successfully");
//
//	// output_trie (kdb->trie);
//	// output_split (kdb->split);
//
//	Key * searchKey = keyNew ("", KEY_END);
//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
//	Backend * b2 = 0;
//
//	keySetName (searchKey, "user");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//	keySetName (searchKey, "user/tests/simple");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//	keySetName (searchKey, "user/tests/simple/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//	keySetName (searchKey, "system/elektra");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keySetName (searchKey, "system/elektra/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keySetName (searchKey, "system");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//	keySetName (searchKey, "system/something");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (!b2, "there should be no backend");
//
//	keyDel (dmp);
//
//	keyDel (searchKey);
//
//	kdb_del (kdb);
//	keyDel (errorKey);
//	ksDel (modules);
}

static void test_rootInit (void)
{
//	printf ("Test mounting with root and init\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");
//	succeed_if (mountDefault (kdb, modules, 0, errorKey) == 0, "could not mount default backend");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "trie was not build up successfully");
//
//	// output_trie (kdb->trie);
//
//	Key * searchKey = keyNew ("", KEY_END);
//	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
//	Backend * b2 = 0;
//
//	keySetName (searchKey, "user");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	if (b2) compare_key (b2->mountpoint, rmp);
//
//
//	Backend * backend = 0;
//	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
//	keySetName (searchKey, "user/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
//	keySetName (searchKey, "system/elektra");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keySetName (searchKey, "system/elektra/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keySetName (searchKey, "system");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 != kdb->initBackend, "should not be the init backend");
//	if (b2) compare_key (b2->mountpoint, rmp);
//
//	keySetName (searchKey, "system/something");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 != kdb->initBackend, "should not be the init backend");
//	if (b2) compare_key (b2->mountpoint, rmp);
//
//	keyDel (dmp);
//	keyDel (mp);
//	keyDel (rmp);
//
//	keyDel (searchKey);
//
//	kdb_del (kdb);
//	keyDel (errorKey);
//	ksDel (modules);
}

static void test_modules (void)
{
//	printf ("Test mounting with modules\n");
//
//	KDB * kdb = kdb_new ();
//	Key * errorKey = keyNew (0);
//	KeySet * modules = modules_config ();
//	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");
//	succeed_if (mountDefault (kdb, modules, 1, errorKey) == 0, "could not mount default backend");
//	succeed_if (mountModules (kdb, modules, errorKey) == 0, "could not mount modules");
//
//	succeed_if (output_warnings (errorKey), "warnings found");
//	succeed_if (output_error (errorKey), "error found");
//
//	exit_if_fail (kdb->trie, "trie was not build up successfully");
//
//	// output_trie (kdb->trie);
//
//	Key * searchKey = keyNew ("", KEY_END);
//	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
//	Backend * b2 = 0;
//
//	keySetName (searchKey, "user");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	if (b2) compare_key (b2->mountpoint, rmp);
//
//
//	Backend * backend = 0;
//	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
//	keySetName (searchKey, "user/tests/simple");
//	backend = trieLookup (kdb->trie, searchKey);
//	succeed_if (backend, "there should be a backend");
//	if (backend) compare_key (backend->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//
//	keySetName (searchKey, "user/tests/simple/deep/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (backend == b2, "should be same backend");
//	if (b2) compare_key (b2->mountpoint, mp);
//
//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
//	keySetName (searchKey, "system/elektra");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	keySetName (searchKey, "system/elektra/below");
//	b2 = trieLookup (kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
//	if (b2) compare_key (b2->mountpoint, dmp);
//
//	Key * mmp = keyNew ("system/elektra/modules", KEY_VALUE, "modules", KEY_END);
//	keyAddBaseName (mmp, "default");
//
//	/*
//	keySetName(searchKey, "system/elektra/modules/default");
//	b2 = trieLookup(kdb->trie, searchKey);
//	succeed_if (b2, "there should be a backend");
//	succeed_if (b2 != kdb->defaultBackend, "should not be the default backend");
//	compare_key(b2->mountpoint, mmp);
//	*/
//
//	keyDel (mmp);
//	keyDel (dmp);
//	keyDel (mp);
//	keyDel (rmp);
//
//	keyDel (searchKey);
//
//	kdb_del (kdb);
//	keyDel (errorKey);
//	ksDel (modules);
}

int main (int argc, char ** argv)
{
	printf ("MOUNT      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_mount ();
	test_minimaltrie ();
	test_simple ();
	test_simpletrie ();
	test_two ();
	test_us ();
	test_endings ();
	test_oldroot ();
	test_cascading ();
	test_root ();
	test_default ();
	test_init ();
	test_rootInit ();
	test_modules ();

	printf ("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
