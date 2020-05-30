/**
 * @file
 *
 * @brief Test suite for split buildup during mount.
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

Backend * b_new (const char * name, const char * value)
{
	Backend * backend = elektraCalloc (sizeof (Backend));
	backend->refcounter = 1;

	backend->mountpoint = keyNew (name, KEY_VALUE, value, KEY_END);
	keyIncRef (backend->mountpoint);

	return backend;
}

Backend * b_default (void)
{
	Backend * backend = elektraCalloc (sizeof (Backend));
	backend->refcounter = 1;
	backend->mountpoint = NULL;

	return backend;
}

static void kdb_del (KDB * kdb)
{
	backendClose (kdb->defaultBackend, 0);
	trieClose (kdb->trie, 0);
	splitDel (kdb->split);

	elektraFree (kdb);
}

static void test_mount (void)
{
	printf ("test mount backend\n");

	KDB * kdb = kdb_new ();
	mountBackend (kdb, b_new ("user:/", "user"), 0);
	succeed_if (kdb->trie, "there should be a trie");

	Key * mp = keyNew ("user:/", KEY_VALUE, "user", KEY_END);

	succeed_if (kdb->split->size == 1, "size of split not correct");
	compare_key (mp, kdb->split->parents[0]);

	compare_key (mountGetBackend (kdb, "user:/")->mountpoint, mp);
	compare_key (mountGetMountpoint (kdb, "user:/"), mp);

	compare_key (mountGetBackend (kdb, "user:/below")->mountpoint, mp);
	compare_key (mountGetMountpoint (kdb, "user:/below"), mp);

	kdb->defaultBackend = b_default ();
	succeed_if (mountGetBackend (kdb, "system:/") == kdb->defaultBackend, "did not return default backend");

	succeed_if (mountGetBackend (kdb, "system:/")->mountpoint == NULL, "mountpoint of default backend should be NULL");
	succeed_if (mountGetMountpoint (kdb, "system:/") == NULL, "mountpoint of default backend should be NULL");

	keyDel (mp);

	kdb_del (kdb);
}

KeySet * modules_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/modules", KEY_END), KS_END);
}

KeySet * minimal_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/mountpoints", KEY_END), KS_END);
}


static void test_minimaltrie (void)
{
	printf ("Test minimal mount\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, minimal_config (), modules, errorKey) == 0, "could not open minimal config");

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	succeed_if (!kdb->trie, "minimal trie is null");
	succeed_if (kdb->split->size == 0, "minimal trie has size 0");

	keyDel (errorKey);
	ksDel (modules);
	kdb_del (kdb);
}

KeySet * simple_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user:/tests/simple", KEY_END), KS_END);
}

static void test_simple (void)
{
	printf ("Test simple mount\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	Key * mp = keyNew ("user:/tests/simple", KEY_VALUE, "simple", KEY_END);

	succeed_if (mountOpen (kdb, simple_config (), modules, errorKey) == 0, "could not open trie");

	succeed_if (kdb->split->size == 1, "size of split not correct");
	compare_key (mp, kdb->split->parents[0]);

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	exit_if_fail (kdb->trie, "kdb->trie was not build up successfully");

	Backend * backend = trieLookup (kdb->trie, "user:/");
	succeed_if (!backend, "there should be no backend");


	backend = trieLookup (kdb->trie, "user:/tests/simple");
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backend->mountpoint, mp);


	Backend * b2 = trieLookup (kdb->trie, "user:/tests/simple/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/deep/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);

	keyDel (errorKey);
	ksDel (modules);
	keyDel (mp);
	kdb_del (kdb);
}

KeySet * set_us (void)
{
	return ksNew (50, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/user:\\/", KEY_END),
		      keyNew ("system:/elektra/mountpoints/user:\\//mountpoint", KEY_VALUE, "user:/", KEY_END),
		      keyNew ("system:/elektra/mountpoints/system:\\/", KEY_END),
		      keyNew ("system:/elektra/mountpoints/system:\\//mountpoint", KEY_VALUE, "system:/", KEY_END), KS_END);
}

static void test_us (void)
{
	printf ("Test mounting of user and system backends\n");

	KDB * kdb = kdb_new ();
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Key * mp;

	KeySet * config = set_us ();
	ksAppendKey (config, keyNew ("system:/elektra/mountpoints", KEY_END));
	succeed_if (mountOpen (kdb, config, modules, 0) == 0, "could not open mount");
	succeed_if (mountDefault (kdb, modules, 1, 0) == 0, "could not mount default backend");

	succeed_if (kdb->split->size == 5, "size of split not correct");
	mp = keyNew ("system:/", KEY_VALUE, "system:/", KEY_END);
	compare_key (mp, kdb->split->parents[0]);
	keySetName (mp, "user:/");
	keySetString (mp, "user:/");
	compare_key (mp, kdb->split->parents[1]);
	keySetName (mp, "system:/elektra");
	keySetString (mp, "default");
	compare_key (mp, kdb->split->parents[4]);
	keyDel (mp);

	Backend * backend = trieLookup (kdb->trie, "user:/anywhere/backend/simple");
	Backend * backend2 = trieLookup (kdb->trie, "user:/anywhere/backend/simple/somewhere/deep/below");
	succeed_if (backend == backend2, "should be same backend");
	mp = backend->mountpoint;
	succeed_if (mp, "no mountpoint found");
	if (mp)
	{
		succeed_if_same_string (keyName (mp), "user:/");
		succeed_if_same_string (keyString (mp), "user:/");
	}

	Backend * two = trieLookup (kdb->trie, "system:/anywhere/tests/backend/two");
	succeed_if (two != backend, "should be differnt backend");

	succeed_if ((mp = two->mountpoint) != 0, "no mountpoint found");
	succeed_if_same_string (keyName (mp), "system:/");
	succeed_if_same_string (keyString (mp), "system:/");

	kdb_del (kdb);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}


KeySet * cascading_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "/tests/simple", KEY_END), KS_END);
}

static void test_cascading (void)
{
	printf ("Test simple mount with cascading\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, cascading_config (), modules, errorKey) == 0, "could not open trie");
	succeed_if (mountDefault (kdb, modules, 1, errorKey) == 0, "could not mount default backend");

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	exit_if_fail (kdb->trie, "kdb->trie was not build up successfully");

	succeed_if (kdb->split->size == 7, "size of split not correct");
	Key * mp = keyNew ("dir:/tests/simple", KEY_VALUE, "simple", KEY_END);
	compare_key (mp, kdb->split->parents[0]);
	keySetName (mp, "user:/tests/simple");
	keySetString (mp, "simple");
	compare_key (mp, kdb->split->parents[1]);
	keySetName (mp, "system:/tests/simple");
	keySetString (mp, "simple");
	compare_key (mp, kdb->split->parents[2]);
	keyDel (mp);

	// output_split (kdb->split);
	// output_trie (kdb->trie);

	Backend * backend = trieLookup (kdb->trie, "user:/");
	succeed_if (!backend, "there should be no backend");

	backend = trieLookup (kdb->trie, "system:/");
	succeed_if (!backend, "there should be no backend");


	mp = keyNew ("/tests/simple", KEY_VALUE, "simple", KEY_END);
	backend = trieLookup (kdb->trie, "user:/tests/simple");
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backend->mountpoint, mp);


	Backend * b2 = trieLookup (kdb->trie, "user:/tests/simple/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/deep/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	backend = trieLookup (kdb->trie, "system:/tests/simple");
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backend->mountpoint, mp);

	b2 = trieLookup (kdb->trie, "system:/tests/simple/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "system:/tests/simple/deep/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	keyDel (errorKey);
	ksDel (modules);
	keyDel (mp);
	kdb_del (kdb);
}


KeySet * root_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/root", KEY_END),
		      keyNew ("system:/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user:/tests/simple", KEY_END), KS_END);
}

static void test_root (void)
{
	printf ("Test mounting with root\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	exit_if_fail (kdb->trie, "trie was not build up successfully");

	succeed_if (kdb->split->size == 5, "size of split not correct");
	Key * mp = keyNew ("spec:/", KEY_VALUE, "root", KEY_END);
	compare_key (mp, kdb->split->parents[0]);
	keySetName (mp, "dir:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[1]);
	keySetName (mp, "user:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[2]);
	keySetName (mp, "system:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[3]);
	keySetName (mp, "user:/tests/simple");
	keySetString (mp, "simple");
	compare_key (mp, kdb->split->parents[4]);

	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
	Backend * b2 = 0;

	b2 = trieLookup (kdb->trie, "user:/");
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (b2->mountpoint, rmp);


	Backend * backend = 0;
	backend = trieLookup (kdb->trie, "user:/tests/simple");
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backend->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/deep/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);

	keyDel (mp);
	keyDel (rmp);

	kdb_del (kdb);
	keyDel (errorKey);
	ksDel (modules);
}

static void test_default (void)
{
	printf ("Test mounting with default\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");
	succeed_if (mountDefault (kdb, modules, 1, errorKey) == 0, "could not mount default backend");

	succeed_if (kdb->split->size == 6, "size of split not correct");
	Key * mp = keyNew ("spec:/", KEY_VALUE, "root", KEY_END);
	compare_key (mp, kdb->split->parents[0]);
	keySetName (mp, "dir:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[1]);
	keySetName (mp, "user:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[2]);
	keySetName (mp, "system:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[3]);
	keySetName (mp, "system:/elektra");
	keySetString (mp, "default");
	compare_key (mp, kdb->split->parents[5]);

	// must be last, needed later
	keySetName (mp, "user:/tests/simple");
	keySetString (mp, "simple");
	compare_key (mp, kdb->split->parents[4]);

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	exit_if_fail (kdb->trie, "trie was not build up successfully");

	// output_trie (kdb->trie);

	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
	Backend * b2 = 0;

	b2 = trieLookup (kdb->trie, "user:/");
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (b2->mountpoint, rmp);


	Backend * backend = 0;
	backend = trieLookup (kdb->trie, "user:/tests/simple");
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backend->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/deep/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);

	b2 = trieLookup (kdb->trie, "system:/elektra");
	succeed_if (b2, "there should be a backend");
	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	if (b2) compare_key (b2->mountpoint, NULL);

	b2 = trieLookup (kdb->trie, "system:/elektra/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	if (b2) compare_key (b2->mountpoint, NULL);

	keyDel (mp);
	keyDel (rmp);

	kdb_del (kdb);
	keyDel (errorKey);
	ksDel (modules);
}

static void test_modules (void)
{
	printf ("Test mounting with modules\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, root_config (), modules, errorKey) == 0, "could not buildup mount");
	succeed_if (mountDefault (kdb, modules, 1, errorKey) == 0, "could not mount default backend");
	succeed_if (mountModules (kdb, modules, errorKey) == 0, "could not mount modules");

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	// output_split (kdb->split);

	succeed_if (kdb->split->size == 8, "size of split not correct");
	Key * mp = keyNew ("spec:/", KEY_VALUE, "root", KEY_END);
	compare_key (mp, kdb->split->parents[0]);
	keySetName (mp, "dir:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[1]);
	keySetName (mp, "user:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[2]);
	keySetName (mp, "system:/");
	keySetString (mp, "root");
	compare_key (mp, kdb->split->parents[3]);
	/* we cannot exactly know where resolver+dump is located
	 *(depending on alphabet)
	keySetName(mp, "system:/elektra/modules/"KDB_DEFAULT_RESOLVER); keySetString (mp, "modules");
	compare_key(mp, kdb->split->parents[4]);
	*/
	keySetName (mp, "system:/elektra");
	keySetString (mp, "default");
	compare_key (mp, kdb->split->parents[5]);

	keySetName (mp, "user:/tests/simple");
	keySetString (mp, "simple");
	compare_key (mp, kdb->split->parents[4]);

	exit_if_fail (kdb->trie, "trie was not build up successfully");

	// output_trie (kdb->trie);

	Key * rmp = keyNew ("/", KEY_VALUE, "root", KEY_END);
	elektraKeySetName (rmp, "/", KEY_CASCADING_NAME);
	Backend * b2 = 0;

	b2 = trieLookup (kdb->trie, "user:/");
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (b2->mountpoint, rmp);


	Backend * backend = 0;
	backend = trieLookup (kdb->trie, "user:/tests/simple");
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backend->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);


	b2 = trieLookup (kdb->trie, "user:/tests/simple/deep/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (b2->mountpoint, mp);

	b2 = trieLookup (kdb->trie, "system:/elektra");
	succeed_if (b2, "there should be a backend");
	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	if (b2) compare_key (b2->mountpoint, NULL);

	b2 = trieLookup (kdb->trie, "system:/elektra/below");
	succeed_if (b2, "there should be a backend");
	succeed_if (b2 == kdb->defaultBackend, "should be the default backend");
	if (b2) compare_key (b2->mountpoint, NULL);

	Key * mmp = keyNew ("system:/elektra/modules", KEY_VALUE, "modules", KEY_END);
	keyAddBaseName (mmp, "default");

	/*
	keySetName(searchKey, "system:/elektra/modules/default");
	b2 = trieLookup(kdb->trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (b2 != kdb->defaultBackend, "should not be the default backend");
	compare_key(b2->mountpoint, mmp);
	*/

	keyDel (mmp);
	keyDel (mp);
	keyDel (rmp);

	kdb_del (kdb);
	keyDel (errorKey);
	ksDel (modules);
}

static void test_defaultonly (void)
{
	printf ("Test mounting with default only\n");

	KDB * kdb = kdb_new ();
	Key * errorKey = keyNew ("/", KEY_END);
	KeySet * modules = modules_config ();
	succeed_if (mountOpen (kdb, minimal_config (), modules, errorKey) == 0, "could not buildup mount");
	succeed_if (mountDefault (kdb, modules, 1, errorKey) == 0, "could not mount default backend");


	// output_split (kdb->split);

	succeed_if (kdb->split->size == 4, "size of split not correct");
	Key * mp = keyNew ("spec:/", KEY_VALUE, "default", KEY_END);
	compare_key (mp, kdb->split->parents[0]);
	keySetName (mp, "dir:/");
	keySetString (mp, "default");
	compare_key (mp, kdb->split->parents[1]);
	keySetName (mp, "user:/");
	keySetString (mp, "default");
	compare_key (mp, kdb->split->parents[2]);
	keySetName (mp, "system:/");
	keySetString (mp, "default");
	compare_key (mp, kdb->split->parents[3]);

	succeed_if (output_warnings (errorKey), "warnings found");
	succeed_if (output_error (errorKey), "error found");

	succeed_if (!kdb->trie, "trie should be empty");

	Backend * b2 = 0;

	b2 = trieLookup (kdb->trie, "user:/");
	succeed_if (b2 == 0, "should be default backend");


	b2 = trieLookup (kdb->trie, "user:/tests/simple");
	succeed_if (b2 == 0, "should be default backend");


	b2 = trieLookup (kdb->trie, "user:/tests/simple/below");
	succeed_if (b2 == 0, "should be default backend");


	b2 = trieLookup (kdb->trie, "user:/tests/simple/deep/below");
	succeed_if (b2 == 0, "should be default backend");

	keyDel (mp);


	kdb_del (kdb);
	keyDel (errorKey);
	ksDel (modules);
}

int main (int argc, char ** argv)
{
	printf ("MOUNTSPLIT    TESTS\n");
	printf ("===================\n\n");

	init (argc, argv);

	test_mount ();
	test_minimaltrie ();
	test_simple ();
	test_us ();
	test_cascading ();
	test_root ();
	test_default ();
	test_modules ();
	test_defaultonly ();

	printf ("\ntest_mountsplit RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
