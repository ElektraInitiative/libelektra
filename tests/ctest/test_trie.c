/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <../../src/libs/elektra/backend.c>
#include <../../src/libs/elektra/trie.c>
#include <tests_internal.h>

#if 1 == 0
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
		      elektraKeyNew ("system:/elektra/mountpoints/simple/set/presetstorage/#0/name", KDB_DEFAULT_STORAGE, ELEKTRA_KEY_END), ELEKTRA_KS_END);
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

Trie * test_insert (Trie * trie, char * name, char * value ELEKTRA_UNUSED)
{
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);
	Plugin * backend = elektraPluginOpen ("backend", modules, set_simple (), 0);
	return trieInsert (trie, name, backend);
}


static void test_minimaltrie (void)
{
	printf ("Test minimal trie\n");

	Trie * trie = test_insert (0, "", "");
	ElektraKey * s = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * mp = elektraKeyNew ("/", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END);

	succeed_if (trieLookup (trie, s), "trie should not be null");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	elektraKeySetName (s, "user:/");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	elektraKeySetName (s, "system:/");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	elektraKeySetName (s, "user:/below");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	elektraKeySetName (s, "system:/below");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	trieClose (trie, 0);
}

ElektraKeyset * simple_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static void test_simple (void)
{
	printf ("Test simple trie\n");

	Trie * trie = test_insert (0, "user:/tests/simple", "simple");

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	ElektraKey * mp = elektraKeyNew ("user:/tests/simple", ELEKTRA_KEY_VALUE, "simple", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/tests/simple/below");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);

	elektraKeySetName (searchKey, "user:/tests/simple/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	elektraKeySetName (searchKey, "user:/tests/simple/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	trieClose (trie, 0);
	elektraKeyDel (mp);
}

static void collect_mountpoints (Trie * trie, ElektraKeyset * mountpoints)
{
	int i;
	for (i = 0; i < KDB_MAX_UCHAR; ++i)
	{
		if (trie->value[i]) elektraKeysetAppendKey (mountpoints, backendGetMountpoint (((Plugin *) trie->value[i])));
		if (trie->children[i]) collect_mountpoints (trie->children[i], mountpoints);
	}
	if (trie->empty_value)
	{
		elektraKeysetAppendKey (mountpoints, backendGetMountpoint (((Plugin *) trie->empty_value)));
	}
}

static void test_iterate (void)
{
	printf ("Test iterate trie\n");

	Trie * trie = test_insert (0, "user:/tests/hosts", "hosts");
	trie = test_insert (trie, "user:/tests/hosts/below", "below");

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	ElektraKey * mp = elektraKeyNew ("user:/tests/hosts", ELEKTRA_KEY_VALUE, "hosts", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/tests/hosts");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);
	// printf ("backend: %p\n", (void*)backend);


	elektraKeySetName (searchKey, "user:/tests/hosts/other/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);
	// printf ("b2: %p\n", (void*)b2);

	elektraKeySetName (searchKey, "user:/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	ElektraKey * mp2 = elektraKeyNew ("user:/tests/hosts/below", ELEKTRA_KEY_VALUE, "below", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/tests/hosts/below");
	Plugin * b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend != b3, "should be different backend");
	if (b3) compare_key (backendGetMountpoint (b3), mp2);
	backend = b3;
	// printf ("b3: %p\n", (void*)b3);


	elektraKeySetName (searchKey, "user:/tests/hosts/below/other/deep/below");
	b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend == b3, "should be same backend");
	if (b3) compare_key (backendGetMountpoint (b3), mp2);

	ElektraKeyset * mps = elektraKeysetNew (0, ELEKTRA_KS_END);
	collect_mountpoints (trie, mps);
	// output_keyset(mps);
	// output_trie(trie);
	succeed_if (elektraKeysetGetSize (mps) == 2, "not both mountpoints collected");
	compare_key (elektraKeysetHead (mps), mp);
	compare_key (elektraKeysetTail (mps), mp2);
	elektraKeysetDel (mps);

	trieClose (trie, 0);

	elektraKeyDel (mp);
	elektraKeyDel (mp2);
}

static void test_reviterate (void)
{
	printf ("Test reviterate trie\n");

	Trie * trie = test_insert (0, "user:/tests/hosts/below", "below");
	trie = test_insert (trie, "user:/tests/hosts", "hosts");

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	ElektraKey * mp = elektraKeyNew ("user:/tests/hosts", ELEKTRA_KEY_VALUE, "hosts", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/tests/hosts");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);
	// printf ("backend: %p\n", (void*)backend);


	elektraKeySetName (searchKey, "user:/tests/hosts/other/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);
	// printf ("b2: %p\n", (void*)b2);


	elektraKeySetName (searchKey, "user:/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	ElektraKey * mp2 = elektraKeyNew ("user:/tests/hosts/below", ELEKTRA_KEY_VALUE, "below", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/tests/hosts/below");
	Plugin * b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend != b3, "should be different backend");
	if (b3) compare_key (backendGetMountpoint (b3), mp2);
	backend = b3;
	// printf ("b3: %p\n", (void*)b3);


	elektraKeySetName (searchKey, "user:/tests/hosts/below/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp2);

	ElektraKeyset * mps = elektraKeysetNew (0, ELEKTRA_KS_END);
	collect_mountpoints (trie, mps);
	succeed_if (elektraKeysetGetSize (mps) == 2, "not both mountpoints collected");
	compare_key (elektraKeysetHead (mps), mp);
	compare_key (elektraKeysetTail (mps), mp2);
	elektraKeysetDel (mps);

	trieClose (trie, 0);

	elektraKeyDel (mp);
	elektraKeyDel (mp2);
}

ElektraKeyset * moreiterate_config (void)
{
	return elektraKeysetNew (50, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/user", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/user/mountpoint", ELEKTRA_KEY_VALUE, "user", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/tests", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/tests/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/hosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/hosts/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/hosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/below/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/hosts/below", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/system", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/system/mountpoint", ELEKTRA_KEY_VALUE, "system", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/systests", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/systests/mountpoint", ELEKTRA_KEY_VALUE, "system:/tests", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/syshosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/syshosts/mountpoint", ELEKTRA_KEY_VALUE, "system:/tests/hosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/sysbelow", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/sysbelow/mountpoint", ELEKTRA_KEY_VALUE, "system:/tests/hosts/below", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

ElektraKeyset * set_mountpoints (void)
{
	return elektraKeysetNew (10, elektraKeyNew ("user:/", ELEKTRA_KEY_VALUE, "user", ELEKTRA_KEY_END), elektraKeyNew ("user:/tests", ELEKTRA_KEY_VALUE, "tests", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/hosts", ELEKTRA_KEY_VALUE, "hosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/hosts/below", ELEKTRA_KEY_VALUE, "below", ELEKTRA_KEY_END), elektraKeyNew ("system:/", ELEKTRA_KEY_VALUE, "system", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/tests", ELEKTRA_KEY_VALUE, "systests", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/tests/hosts", ELEKTRA_KEY_VALUE, "syshosts", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/tests/hosts/below", ELEKTRA_KEY_VALUE, "sysbelow", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static void test_moreiterate (void)
{
	printf ("Test moreiterate trie\n");

	Trie * trie = test_insert (0, "user:/", "user");
	trie = test_insert (trie, "user:/tests", "tests");
	trie = test_insert (trie, "user:/tests/hosts", "hosts");
	trie = test_insert (trie, "user:/tests/hosts/below", "below");
	trie = test_insert (trie, "system:/", "system");
	trie = test_insert (trie, "system:/tests", "systests");
	trie = test_insert (trie, "system:/tests/hosts", "syshosts");
	trie = test_insert (trie, "system:/tests/hosts/below", "sysbelow");

	ElektraKeyset * mps = set_mountpoints ();

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);

	elektraKeySetName (searchKey, "user:/");
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	compare_key (backendGetMountpoint (backend), elektraKeysetLookupByName (mps, "user:/", 0));
	// printf ("backend: %p\n", (void*)backend);


	elektraKeySetName (searchKey, "user:/tests/hosts/other/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "user:/tests/hosts", 0));
	// printf ("b2: %p\n", (void*)b2);


	elektraKeySetName (searchKey, "user:/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "user:/tests/hosts", 0));


	elektraKeySetName (searchKey, "user:/tests/hosts/below");
	Plugin * b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	if (b3) compare_key (backendGetMountpoint (b3), elektraKeysetLookupByName (mps, "user:/tests/hosts/below", 0));
	// printf ("b3: %p\n", (void*)b3);


	elektraKeySetName (searchKey, "user:/tests/hosts/below/other/deep/below");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), elektraKeysetLookupByName (mps, "user:/tests/hosts/below", 0));

	elektraKeySetName (searchKey, "system:/");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), elektraKeysetLookupByName (mps, "system:/", 0));
	// printf ("backend: %p\n", (void*)backend);


	elektraKeySetName (searchKey, "system:/tests/hosts/other/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "system:/tests/hosts", 0));
	// printf ("b2: %p\n", (void*)b2);


	elektraKeySetName (searchKey, "system:/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "system:/tests/hosts", 0));


	elektraKeySetName (searchKey, "system:/tests/hosts/below");
	b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	if (b3) compare_key (backendGetMountpoint (b3), elektraKeysetLookupByName (mps, "system:/tests/hosts/below", 0));
	// printf ("b3: %p\n", (void*)b3);


	elektraKeySetName (searchKey, "system:/tests/hosts/below/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "system:/tests/hosts/below", 0));

	ElektraKeyset * mps_cmp = elektraKeysetNew (0, ELEKTRA_KS_END);
	collect_mountpoints (trie, mps_cmp);
	succeed_if (elektraKeysetGetSize (mps_cmp) == 8, "size should be 8");
	compare_keyset (mps, mps_cmp);

	elektraKeysetDel (mps_cmp);
	elektraKeysetDel (mps);

	trieClose (trie, 0);
}

static void test_revmoreiterate (void)
{
	printf ("Test revmoreiterate trie\n");

	for (int i = 0; i < 5; ++i)
	{

		Trie * trie = 0;
		switch (i)
		{
		case 0:
			trie = test_insert (trie, "user:/tests", "tests");
			trie = test_insert (trie, "user:/tests/hosts", "hosts");
			trie = test_insert (trie, "user:/tests/hosts/below", "below");
			trie = test_insert (trie, "system:/tests", "systests");
			trie = test_insert (trie, "system:/tests/hosts", "syshosts");
			trie = test_insert (trie, "system:/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system:/", "system");
			trie = test_insert (trie, "user:/", "user");
			break;
		case 1:
			trie = test_insert (trie, "system:/tests/hosts", "syshosts");
			trie = test_insert (trie, "system:/", "system");
			trie = test_insert (trie, "user:/tests", "tests");
			trie = test_insert (trie, "user:/tests/hosts", "hosts");
			trie = test_insert (trie, "user:/tests/hosts/below", "below");
			trie = test_insert (trie, "system:/tests", "systests");
			trie = test_insert (trie, "user:/", "user");
			trie = test_insert (trie, "system:/tests/hosts/below", "sysbelow");
			break;
		case 2:
			trie = test_insert (trie, "system:/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system:/tests/hosts", "syshosts");
			trie = test_insert (trie, "user:/tests/hosts/below", "below");
			trie = test_insert (trie, "user:/tests/hosts", "hosts");
			trie = test_insert (trie, "user:/tests", "tests");
			trie = test_insert (trie, "user:/", "user");
			trie = test_insert (trie, "system:/tests", "systests");
			trie = test_insert (trie, "system:/", "system");
			break;
		case 3:
			trie = test_insert (trie, "user:/tests/hosts/below", "below");
			trie = test_insert (trie, "user:/tests/hosts", "hosts");
			trie = test_insert (trie, "user:/tests", "tests");
			trie = test_insert (trie, "user:/", "user");
			trie = test_insert (trie, "system:/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system:/tests/hosts", "syshosts");
			trie = test_insert (trie, "system:/tests", "systests");
			trie = test_insert (trie, "system:/", "system");
			break;
		case 4:
			trie = test_insert (trie, "system:/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system:/tests/hosts", "syshosts");
			trie = test_insert (trie, "system:/tests", "systests");
			trie = test_insert (trie, "system:/", "system");
			trie = test_insert (trie, "user:/tests/hosts/below", "below");
			trie = test_insert (trie, "user:/tests/hosts", "hosts");
			trie = test_insert (trie, "user:/tests", "tests");
			trie = test_insert (trie, "user:/", "user");
			break;
		}

		ElektraKeyset * mps = set_mountpoints ();

		exit_if_fail (trie, "trie was not build up successfully");

		ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);

		elektraKeySetName (searchKey, "user:/");
		Plugin * backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), elektraKeysetLookupByName (mps, "user:/", 0));
		// printf ("backend: %p\n", (void*)backend);


		elektraKeySetName (searchKey, "user:/tests/hosts/other/below");
		Plugin * b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "user:/tests/hosts", 0));
		// printf ("b2: %p\n", (void*)b2);


		elektraKeySetName (searchKey, "user:/tests/hosts/other/deep/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "user:/tests/hosts", 0));


		elektraKeySetName (searchKey, "user:/tests/hosts/below");
		Plugin * b3 = trieLookup (trie, searchKey);
		succeed_if (b3, "there should be a backend");
		if (b3) compare_key (backendGetMountpoint (b3), elektraKeysetLookupByName (mps, "user:/tests/hosts/below", 0));
		// printf ("b3: %p\n", (void*)b3);


		elektraKeySetName (searchKey, "user:/tests/hosts/below/other/deep/below");
		backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), elektraKeysetLookupByName (mps, "user:/tests/hosts/below", 0));

		elektraKeySetName (searchKey, "system:/");
		backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), elektraKeysetLookupByName (mps, "system:/", 0));
		// printf ("backend: %p\n", (void*)backend);


		elektraKeySetName (searchKey, "system:/tests/hosts/other/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "system:/tests/hosts", 0));
		// printf ("b2: %p\n", (void*)b2);


		elektraKeySetName (searchKey, "system:/tests/hosts/other/deep/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "system:/tests/hosts", 0));


		elektraKeySetName (searchKey, "system:/tests/hosts/below");
		b3 = trieLookup (trie, searchKey);
		succeed_if (b3, "there should be a backend");
		if (b3) compare_key (backendGetMountpoint (b3), elektraKeysetLookupByName (mps, "system:/tests/hosts/below", 0));
		// printf ("b3: %p\n", (void*)b3);


		elektraKeySetName (searchKey, "system:/tests/hosts/below/other/deep/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), elektraKeysetLookupByName (mps, "system:/tests/hosts/below", 0));

		/*
		printf ("---------\n");
		output_trie(trie);
		*/

		ElektraKeyset * mps_cmp = elektraKeysetNew (0, ELEKTRA_KS_END);
		collect_mountpoints (trie, mps_cmp);
		succeed_if (elektraKeysetGetSize (mps_cmp) == 8, "size should be 8");
		compare_keyset (mps, mps_cmp);

		elektraKeysetDel (mps_cmp);
		elektraKeysetDel (mps);

		trieClose (trie, 0);

	} // end for
}


static void test_umlauts (void)
{
	printf ("Test umlauts trie\n");

	Trie * trie = test_insert (0, "user:/umlauts/test", "slash");
	trie = test_insert (trie, "user:/umlauts#test", "hash");
	trie = test_insert (trie, "user:/umlauts test", "space");
	trie = test_insert (trie, "user:/umlauts\200test", "umlauts");

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	ElektraKey * mp = elektraKeyNew ("user:/umlauts/test", ELEKTRA_KEY_VALUE, "slash", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/umlauts/test");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	elektraKeySetName (mp, "user:/umlauts#test");
	elektraKeySetString (mp, "hash");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend != b2, "should be other backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	elektraKeySetName (mp, "user:/umlauts test");
	elektraKeySetString (mp, "space");
	elektraKeySetName (searchKey, "user:/umlauts test");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend != b2, "should be other backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	elektraKeySetName (mp, "user:/umlauts\200test");
	elektraKeySetString (mp, "umlauts");
	elektraKeySetName (searchKey, "user:/umlauts\200test");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend != b2, "should be other backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	// output_trie(trie);

	trieClose (trie, 0);
	elektraKeyDel (mp);
}

static void test_endings (void)
{
	printf ("Test endings trie\n");

	for (int i = 0; i < 4; ++i)
	{

		Trie * trie = 0;
		switch (i)
		{
		case 0:
			trie = test_insert (trie, "user:/endings/", "slash");
			trie = test_insert (trie, "user:/endings#", "hash");
			trie = test_insert (trie, "user:/endings ", "space");
			trie = test_insert (trie, "user:/endings\200", "endings");
			break;
		case 1:
			trie = test_insert (trie, "user:/endings#", "hash");
			trie = test_insert (trie, "user:/endings ", "space");
			trie = test_insert (trie, "user:/endings\200", "endings");
			trie = test_insert (trie, "user:/endings/", "slash");
			break;
		case 2:
			trie = test_insert (trie, "user:/endings ", "space");
			trie = test_insert (trie, "user:/endings\200", "endings");
			trie = test_insert (trie, "user:/endings/", "slash");
			trie = test_insert (trie, "user:/endings#", "hash");
			break;
		case 3:
			trie = test_insert (trie, "user:/endings\200", "endings");
			trie = test_insert (trie, "user:/endings ", "space");
			trie = test_insert (trie, "user:/endings#", "hash");
			trie = test_insert (trie, "user:/endings/", "slash");
			break;
		}

		exit_if_fail (trie, "trie was not build up successfully");

		ElektraKey * searchKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
		Plugin * backend = trieLookup (trie, searchKey);
		succeed_if (!backend, "there should be no backend");


		ElektraKey * mp = elektraKeyNew ("user:/endings", ELEKTRA_KEY_VALUE, "slash", ELEKTRA_KEY_END);
		elektraKeySetName (searchKey, "user:/endings");
		backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), mp);


		elektraKeySetName (mp, "user:/endings#");
		elektraKeySetString (mp, "hash");
		Plugin * b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend != b2, "should be other backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);


		elektraKeySetName (mp, "user:/endings");
		elektraKeySetString (mp, "slash");
		elektraKeySetName (searchKey, "user:/endings/_");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend == b2, "should be the same backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);


		elektraKeySetName (mp, "user:/endings");
		elektraKeySetString (mp, "slash");
		elektraKeySetName (searchKey, "user:/endings/X");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend == b2, "should be the same backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);


		elektraKeySetName (searchKey, "user:/endings_");
		b2 = trieLookup (trie, searchKey);
		succeed_if (!b2, "there should be no backend");


		elektraKeySetName (searchKey, "user:/endingsX");
		b2 = trieLookup (trie, searchKey);
		succeed_if (!b2, "there should be no backend");


		elektraKeySetName (searchKey, "user:/endings!");
		b2 = trieLookup (trie, searchKey);
		succeed_if (!b2, "there should be no backend");


		elektraKeySetName (mp, "user:/endings ");
		elektraKeySetString (mp, "space");
		elektraKeySetName (searchKey, "user:/endings ");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend != b2, "should be other backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);

		elektraKeySetName (mp, "user:/endings\200");
		elektraKeySetString (mp, "endings");
		elektraKeySetName (searchKey, "user:/endings\200");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend != b2, "should be other backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);

		// output_trie(trie);

		trieClose (trie, 0);
		elektraKeyDel (mp);
	}
}

static void test_root (void)
{
	printf ("Test trie with root\n");

	Trie * trie = 0;
	trie = test_insert (trie, "", "root");
	trie = test_insert (trie, "user:/tests/simple", "simple");

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	ElektraKey * rmp = elektraKeyNew ("/", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be the root backend");
	if (backend) compare_key (backendGetMountpoint (backend), rmp);


	ElektraKey * mp = elektraKeyNew ("user:/tests/simple", ELEKTRA_KEY_VALUE, "simple", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/tests/simple");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	elektraKeySetName (searchKey, "user:/tests/simple/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	elektraKeySetName (searchKey, "user:/tests/simple/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	// output_trie(trie);

	trieClose (trie, 0);
	elektraKeyDel (mp);
	elektraKeyDel (rmp);
}

static void test_double (void)
{
	printf ("Test double insertion\n");

	Trie * trie = test_insert (0, "/", "root");
	succeed_if (trie, "could not insert into trie");

	Trie * t1 = test_insert (trie, "user:/tests/simple", "t1");
	succeed_if (t1, "could not insert into trie");
	succeed_if (t1 == trie, "should be the same");

	// output_trie (trie);

	Trie * t2 = test_insert (trie, "user:/tests/simple", "t2");
	succeed_if (t2, "could not insert into trie");
	succeed_if (t2 == trie, "should be not the same");

	// output_trie (trie);

	/* ... gets lost

	Trie *t3 = test_insert (trie, "user:/tests/simple", "t3");
	succeed_if (t3, "could not insert into trie");
	succeed_if (t3 == trie, "should be not the same");

	// output_trie (trie);

	*/

	trieClose (trie, 0);
}

static void test_emptyvalues (void)
{
	printf ("Test empty values in trie\n");

	Trie * trie = 0;
	trie = test_insert (trie, "user:/umlauts/b/", "b");
	trie = test_insert (trie, "user:/umlauts/a/", "a");
	trie = test_insert (trie, "user:/umlauts/", "/");
	trie = test_insert (trie, "user:/umlauts/c/", "c");
	trie = test_insert (trie, "user:/", "u");

	exit_if_fail (trie, "trie was not build up successfully");

	// output_trie(trie);

	trieClose (trie, 0);
}

static void test_userroot (void)
{
	printf ("Test trie with user:/\n");

	Trie * trie = 0;
	trie = test_insert (trie, "user:/", "root");

	exit_if_fail (trie, "trie was not build up successfully");

	ElektraKey * searchKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * mp = elektraKeyNew ("user:/", ELEKTRA_KEY_VALUE, "root", ELEKTRA_KEY_END);
	elektraKeySetName (searchKey, "user:/");
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);

	elektraKeySetName (searchKey, "user:/tests");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);

	elektraKeySetName (searchKey, "user:/tests/simple");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	elektraKeySetName (searchKey, "user:/tests/simple/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	elektraKeySetName (searchKey, "user:/tests/simple/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	// output_trie(trie);

	trieClose (trie, 0);
	elektraKeyDel (mp);
	elektraKeyDel (searchKey);
}
#endif

int main (int argc, char ** argv)
{
	printf ("TRIE       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

#if 1 == 0
	test_minimaltrie ();
	test_simple ();
	test_iterate ();
	test_reviterate ();
	test_moreiterate ();
	test_revmoreiterate ();
	test_umlauts ();
	test_endings ();
	test_root ();
	test_double ();
	test_emptyvalues ();
	test_userroot ();
#endif

	printf ("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
