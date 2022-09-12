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

// FIXME: lots of commented out tests

#if 1 == 0
ElektraKdb * kdb_new (void)
{
	ElektraKdb * kdb = elektraCalloc (sizeof (KDB));
	kdb->split = splitNew ();
	return kdb;
}

static void kdb_del (ElektraKdb * kdb)
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

ElektraKeyset * modules_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/modules", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

ElektraKeyset * set_simple (void)
{
	return elektraKeysetNew (50, elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/anything", ELEKTRA_KEY_VALUE, "backend", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/more/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/backend/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/path", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/error", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/error/prerollback", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/error/prerollback/#1", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/error/prerollback/#1/name", ELEKTRA_KEY_VALUE, KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/get", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/anything", ELEKTRA_KEY_VALUE, "plugin", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/more/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/config/path", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/get/pregetstorage/#0/name", ELEKTRA_KEY_VALUE, KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/set", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/set/presetstorage", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/set/presetstorage/#0", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/set/presetstorage/#0/name", ELEKTRA_KEY_VALUE, KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),
		      ELEKTRA_KS_END);
}

static void test_mount (void)
{
	printf ("test mount backend\n");

	ElektraKeyset * modules = modules_config ();
	Plugin * backend = elektraPluginOpen ("backend", modules, set_simple (), 0);

	if (backend == NULL)
	{
		elektraKeysetDel (modules);
		exit_if_fail (0, "couldn't open backend");
	}

	ElektraKdb * kdb = kdb_new ();

	mountBackend (kdb, backend, 0);
	succeed_if (kdb->trie, "there should be a trie");

	ElektraKey * mp = elektraKeyNew ("system:/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/backend/simple", ELEKTRA_KEY_END);
	ElektraKey * sk = elektraKeyNew ("user", ELEKTRA_KEY_VALUE, "user", ELEKTRA_KEY_END);

	Plugin * tempBackend = mountGetBackend (kdb, sk);
	compare_key (backendGetMountpoint (tempBackend), mp);
	compare_key (mountGetMountpoint (kdb, sk), mp);

	elektraKeySetName (sk, "user:/below");
	tempBackend = mountGetBackend (kdb, sk);
	compare_key (backendGetMountpoint (tempBackend), mp);
	compare_key (mountGetMountpoint (kdb, sk), mp);

	elektraKeySetName (sk, "system");
	//	kdb->defaultBackend = b_new ("", "default");
	succeed_if (mountGetBackend (kdb, sk) == kdb->defaultBackend, "did not return default backend");

	elektraKeySetName (mp, "");
	elektraKeySetString (mp, "default");
	compare_key (backendGetMountpoint (mountGetBackend (kdb, sk)), mp);
	compare_key (mountGetMountpoint (kdb, sk), mp);

	elektraKeyDel (mp);
	elektraKeysetDel (modules);

	kdb_del (kdb);
}

ElektraKeyset * minimal_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}


static void test_minimaltrie (void)
{
	printf ("Test minimal mount\n");

	ElektraKdb * kdb = kdb_new ();
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = modules_config ();
	succeed_if (mountOpen (kdb, minimal_config (), modules, errorKey) == 0, "could not open minimal config")

		succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	succeed_if (!kdb->trie, "minimal trie is null");

	elektraKeyDel (errorKey);
	elektraKeysetDel (modules);
	kdb_del (kdb);
}

ElektraKeyset * simple_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static void test_simple (void)
{
	printf ("Test simple mount\n");

	ElektraKdb * kdb = kdb_new ();
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = modules_config ();
	succeed_if (mountOpen (kdb, simple_config (), modules, errorKey) == 0, "could not open trie");

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	exit_if_fail (kdb->trie, "kdb->trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("user", ELEKTRA_KEY_END);
	Plugin * backend = trieLookup (kdb->trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	ElektraKey * mp = elektraKeyNew ("user:/tests/simple", ELEKTRA_KEY_VALUE, "simple", ELEKTRA_KEY_END);
	backend = trieLookup (kdb->trie, elektraKeyNew ("user:/tests/simple", ELEKTRA_KEY_END));
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	elektraKeySetName (searchKey, "user:/tests/simple/below");
	Plugin * b2 = trieLookup (kdb->trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	b2 = trieLookup (kdb->trie, elektraKeyNew ("user:/tests/simple/deep/below", ELEKTRA_KEY_END));
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	elektraKeyDel (errorKey);
	elektraKeysetDel (modules);
	elektraKeyDel (mp);
	kdb_del (kdb);
}

ElektraKeyset * set_pluginconf (void)
{
	return elektraKeysetNew (10, elektraKeyNew ("system:/anything", ELEKTRA_KEY_VALUE, "backend", ELEKTRA_KEY_END), elektraKeyNew ("system:/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/more/config", ELEKTRA_KEY_END), elektraKeyNew ("system:/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/backend/simple", ELEKTRA_KEY_END), elektraKeyNew ("system:/path", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/anything", ELEKTRA_KEY_VALUE, "plugin", ELEKTRA_KEY_END), elektraKeyNew ("user:/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/more/config", ELEKTRA_KEY_END), elektraKeyNew ("user:/more/config/below", ELEKTRA_KEY_END), elektraKeyNew ("user:/path", ELEKTRA_KEY_END),
		      ELEKTRA_KS_END);
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
	//	ksAppendKey (config, keyNew ("system:/elektra/mountpoints", KEY_END));
	//	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
	//
	//	Key * key = keyNew ("user:/tests/backend/simple", KEY_END);
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
	//		succeed_if_same_string (keyName (mp), "user:/tests/backend/simple");
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


ElektraKeyset * set_two (void)
{
	return elektraKeysetNew (50, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/anything", ELEKTRA_KEY_VALUE, "backend", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/more/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/config/path", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/anything", ELEKTRA_KEY_VALUE,
			      "plugin", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/getplugins/#1" KDB_DEFAULT_STORAGE "/config/path", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/backend/simple", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/simple/setplugins", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/setplugins/#1" KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),


		      elektraKeyNew ("system:/elektra/mountpoints/two", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/two/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/config/anything", ELEKTRA_KEY_VALUE, "backend", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/config/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/config/more/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/config/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/config/path", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/anything", ELEKTRA_KEY_VALUE, "plugin",
			      ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/more/config/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/getplugins/#1" KDB_DEFAULT_STORAGE "/config/path", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/two/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/backend/two", ELEKTRA_KEY_END),

		      elektraKeyNew ("system:/elektra/mountpoints/two/setplugins", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/setplugins/#1" KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/two/setplugins/#2" KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END), ELEKTRA_KS_END);
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
	//	ksAppendKey (config, keyNew ("system:/elektra/mountpoints", KEY_END));
	//	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
	//
	//	Key * key = keyNew ("user:/tests/backend/simple", KEY_END);
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
	//	succeed_if_same_string (keyName (mp), "user:/tests/backend/simple");
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
	//	keySetName (key, "user:/tests/backend/two");
	//	Backend * two = trieLookup (kdb->trie, key);
	//	succeed_if (two != backend, "should be differnt backend");
	//
	//	succeed_if ((mp = two->mountpoint) != 0, "no mountpoint found");
	//	succeed_if_same_string (keyName (mp), "user:/tests/backend/two");
	//	succeed_if_same_string (keyString (mp), "two");
	//
	//	keyDel (key);
	//	elektraModulesClose (modules, 0);
	//	ksDel (modules);
	//	kdb_del (kdb);
}


ElektraKeyset * set_us (void)
{
	return elektraKeysetNew (50, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/user:\\/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/user:\\//mountpoint", ELEKTRA_KEY_VALUE, "user:/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/system:\\/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/system:\\//mountpoint", ELEKTRA_KEY_VALUE, "system:/", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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
	//	ksAppendKey (config, keyNew ("system:/elektra/mountpoints", KEY_END));
	//	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
	//
	//	Key * key = keyNew ("user:/anywhere/backend/simple", KEY_END);
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
	//	keySetName (key, "system:/anywhere/tests/backend/two");
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

ElektraKeyset * endings_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/slash", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/slash/mountpoint", ELEKTRA_KEY_VALUE, "user:/endings", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/hash", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/hash/mountpoint", ELEKTRA_KEY_VALUE, "user:/endings#", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/space", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/space/mountpoint", ELEKTRA_KEY_VALUE, "user:/endings ", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/endings", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/endings/mountpoint", ELEKTRA_KEY_VALUE, "user:/endings\200", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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
	//	Key * mp = keyNew ("user:/endings", KEY_VALUE, "slash", KEY_END);
	//	keySetName (searchKey, "user:/endings");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/endings#");
	//	keySetName (mp, "user:/endings#");
	//	keySetString (mp, "hash");
	//	Backend * b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend != b2, "should be other backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/endings/_");
	//	keySetName (mp, "user:/endings");
	//	keySetString (mp, "slash");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be the same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/endings/X");
	//	keySetName (mp, "user:/endings");
	//	keySetString (mp, "slash");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be the same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/endings_");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//
	//	keySetName (searchKey, "user:/endingsX");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//
	//	keySetName (searchKey, "user:/endings!");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//
	//	keySetName (searchKey, "user:/endings ");
	//	keySetName (mp, "user:/endings ");
	//	keySetString (mp, "space");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend != b2, "should be other backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//	keySetName (searchKey, "user:/endings\200");
	//	keySetName (mp, "user:/endings\200");
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

ElektraKeyset * cascading_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "/tests/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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
	//	keySetName (searchKey, "user:/tests/simple");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/below");
	//	Backend * b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/deep/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "system:/tests/simple");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//	keySetName (searchKey, "system:/tests/simple/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "system:/tests/simple/deep/below");
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


ElektraKeyset * root_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/root", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/root/mountpoint", ELEKTRA_KEY_VALUE, "/", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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
	//	Key * mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//	keySetName (searchKey, "user:/tests/simple");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/deep/below");
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
	//	Key * mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//	keySetName (searchKey, "user:/tests/simple");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/deep/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
	//	keySetName (searchKey, "system:/elektra");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	//	if (b2) compare_key (b2->mountpoint, dmp);
	//
	//	keySetName (searchKey, "system:/elektra/below");
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
	//	keySetName (searchKey, "user:/tests/simple");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//	keySetName (searchKey, "user:/tests/simple/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/deep/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//	keySetName (searchKey, "system:/elektra");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
	//	if (b2) compare_key (b2->mountpoint, dmp);
	//
	//	keySetName (searchKey, "system:/elektra/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
	//	if (b2) compare_key (b2->mountpoint, dmp);
	//
	//	keySetName (searchKey, "system");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (!b2, "there should be no backend");
	//
	//	keySetName (searchKey, "system:/something");
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
	//	Key * mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//	keySetName (searchKey, "user:/tests/simple");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/deep/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
	//	keySetName (searchKey, "system:/elektra");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (b2 == kdb->initBackend, "should be the init backend");
	//	if (b2) compare_key (b2->mountpoint, dmp);
	//
	//	keySetName (searchKey, "system:/elektra/below");
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
	//	keySetName (searchKey, "system:/something");
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
	//	Key * mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);
	//	keySetName (searchKey, "user:/tests/simple");
	//	backend = trieLookup (kdb->trie, searchKey);
	//	succeed_if (backend, "there should be a backend");
	//	if (backend) compare_key (backend->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//
	//	keySetName (searchKey, "user:/tests/simple/deep/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (backend == b2, "should be same backend");
	//	if (b2) compare_key (b2->mountpoint, mp);
	//
	//	Key * dmp = keyNew ("", KEY_VALUE, "default", KEY_END);
	//	keySetName (searchKey, "system:/elektra");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	//	if (b2) compare_key (b2->mountpoint, dmp);
	//
	//	keySetName (searchKey, "system:/elektra/below");
	//	b2 = trieLookup (kdb->trie, searchKey);
	//	succeed_if (b2, "there should be a backend");
	//	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	//	if (b2) compare_key (b2->mountpoint, dmp);
	//
	//	Key * mmp = keyNew ("system:/elektra/modules", KEY_VALUE, "modules", KEY_END);
	//	keyAddBaseName (mmp, "default");
	//
	//	/*
	//	keySetName(searchKey, "system:/elektra/modules/default");
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
#endif

int main (int argc, char ** argv)
{
	printf ("MOUNT      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

#if 1 == 0
	test_mount ();
	test_minimaltrie ();
	test_simple ();
	test_simpletrie ();
	test_two ();
	test_us ();
	test_endings ();
	test_cascading ();
	test_root ();
	test_default ();
	test_init ();
	test_rootInit ();
	test_modules ();
#endif

	printf ("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
