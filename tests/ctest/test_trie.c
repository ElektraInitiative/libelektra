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

KeySet * set_pluginconf (void)
{
	return ksNew (10, keyNew ("system/anything", KEY_VALUE, "backend", KEY_END), keyNew ("system/more", KEY_END),
		      keyNew ("system/more/config", KEY_END), keyNew ("system/more/config/below", KEY_END),
		      keyNew ("system/mountpoint", KEY_VALUE, "user/tests/backend/simple", KEY_END), keyNew ("system/path", KEY_END),
		      keyNew ("user/anything", KEY_VALUE, "plugin", KEY_END), keyNew ("user/more", KEY_END),
		      keyNew ("user/more/config", KEY_END), keyNew ("user/more/config/below", KEY_END), keyNew ("user/path", KEY_END),
		      KS_END);
}

Trie * test_insert (Trie * trie, char * name, char * value ELEKTRA_UNUSED)
{
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	Plugin * backend = elektraPluginOpen ("backend", modules, set_simple (), 0);
	return trieInsert (trie, name, backend);
}


static void test_minimaltrie (void)
{
	printf ("Test minimal trie\n");

	Trie * trie = test_insert (0, "", "");
	Key * s = keyNew ("", KEY_END);
	Key * mp = keyNew ("", KEY_VALUE, "", KEY_END);

	succeed_if (trieLookup (trie, s), "trie should not be null");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	keySetName (s, "user");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	keySetName (s, "system");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	keySetName (s, "user/below");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	keySetName (s, "system/below");
	compare_key (backendGetMountpoint (trieLookup (trie, s)), mp);

	trieClose (trie, 0);
	keyDel (s);
	keyDel (mp);
}

KeySet * simple_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END), KS_END);
}

static void test_simple (void)
{
	printf ("Test simple trie\n");

	Trie * trie = test_insert (0, "user/tests/simple", "simple");

	exit_if_fail (trie, "trie was not build up successfully");

	Key * searchKey = keyNew ("user", KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	keySetName (searchKey, "user/tests/simple");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);

	keySetName (searchKey, "user/tests/simple/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	keySetName (searchKey, "user/tests/simple/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	trieClose (trie, 0);
	keyDel (mp);
	keyDel (searchKey);
}

static void collect_mountpoints (Trie * trie, KeySet * mountpoints)
{
	int i;
	for (i = 0; i < KDB_MAX_UCHAR; ++i)
	{
		if (trie->value[i]) ksAppendKey (mountpoints, backendGetMountpoint (((Plugin *) trie->value[i])));
		if (trie->children[i]) collect_mountpoints (trie->children[i], mountpoints);
	}
	if (trie->empty_value)
	{
		ksAppendKey (mountpoints, backendGetMountpoint (((Plugin *) trie->empty_value)));
	}
}

static void test_iterate (void)
{
	printf ("Test iterate trie\n");

	Trie * trie = test_insert (0, "user/tests/hosts", "hosts");
	trie = test_insert (trie, "user/tests/hosts/below", "below");

	exit_if_fail (trie, "trie was not build up successfully");

	Key * searchKey = keyNew ("user", KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key * mp = keyNew ("user/tests/hosts", KEY_VALUE, "hosts", KEY_END);
	keySetName (searchKey, "user/tests/hosts");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);
	// printf ("backend: %p\n", (void*)backend);


	keySetName (searchKey, "user/tests/hosts/other/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);
	// printf ("b2: %p\n", (void*)b2);


	keySetName (searchKey, "user/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	Key * mp2 = keyNew ("user/tests/hosts/below", KEY_VALUE, "below", KEY_END);
	keySetName (searchKey, "user/tests/hosts/below");
	Plugin * b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend != b3, "should be different backend");
	if (b3) compare_key (backendGetMountpoint (b3), mp2);
	backend = b3;
	// printf ("b3: %p\n", (void*)b3);


	keySetName (searchKey, "user/tests/hosts/below/other/deep/below");
	b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend == b3, "should be same backend");
	if (b3) compare_key (backendGetMountpoint (b3), mp2);

	KeySet * mps = ksNew (0, KS_END);
	collect_mountpoints (trie, mps);
	// output_keyset(mps);
	// output_trie(trie);
	succeed_if (ksGetSize (mps) == 2, "not both mountpoints collected");
	compare_key (ksHead (mps), mp);
	compare_key (ksTail (mps), mp2);
	ksDel (mps);

	trieClose (trie, 0);

	keyDel (mp);
	keyDel (mp2);
	keyDel (searchKey);
}

static void test_reviterate (void)
{
	printf ("Test reviterate trie\n");

	Trie * trie = test_insert (0, "user/tests/hosts/below", "below");
	trie = test_insert (trie, "user/tests/hosts", "hosts");

	exit_if_fail (trie, "trie was not build up successfully");

	Key * searchKey = keyNew ("user", KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key * mp = keyNew ("user/tests/hosts", KEY_VALUE, "hosts", KEY_END);
	keySetName (searchKey, "user/tests/hosts");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);
	// printf ("backend: %p\n", (void*)backend);


	keySetName (searchKey, "user/tests/hosts/other/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);
	// printf ("b2: %p\n", (void*)b2);


	keySetName (searchKey, "user/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	Key * mp2 = keyNew ("user/tests/hosts/below", KEY_VALUE, "below", KEY_END);
	keySetName (searchKey, "user/tests/hosts/below");
	Plugin * b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend != b3, "should be different backend");
	if (b3) compare_key (backendGetMountpoint (b3), mp2);
	backend = b3;
	// printf ("b3: %p\n", (void*)b3);


	keySetName (searchKey, "user/tests/hosts/below/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp2);

	KeySet * mps = ksNew (0, KS_END);
	collect_mountpoints (trie, mps);
	succeed_if (ksGetSize (mps) == 2, "not both mountpoints collected");
	compare_key (ksHead (mps), mp);
	compare_key (ksTail (mps), mp2);
	ksDel (mps);

	trieClose (trie, 0);

	keyDel (mp);
	keyDel (mp2);
	keyDel (searchKey);
}

KeySet * moreiterate_config (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/user", KEY_END),
		      keyNew ("system/elektra/mountpoints/user/mountpoint", KEY_VALUE, "user", KEY_END),
		      keyNew ("system/elektra/mountpoints/tests", KEY_END),
		      keyNew ("system/elektra/mountpoints/tests/mountpoint", KEY_VALUE, "user/tests", KEY_END),
		      keyNew ("system/elektra/mountpoints/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/hosts/mountpoint", KEY_VALUE, "user/tests/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/below/mountpoint", KEY_VALUE, "user/tests/hosts/below", KEY_END),
		      keyNew ("system/elektra/mountpoints/system", KEY_END),
		      keyNew ("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END),
		      keyNew ("system/elektra/mountpoints/systests", KEY_END),
		      keyNew ("system/elektra/mountpoints/systests/mountpoint", KEY_VALUE, "system/tests", KEY_END),
		      keyNew ("system/elektra/mountpoints/syshosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/syshosts/mountpoint", KEY_VALUE, "system/tests/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/sysbelow", KEY_END),
		      keyNew ("system/elektra/mountpoints/sysbelow/mountpoint", KEY_VALUE, "system/tests/hosts/below", KEY_END), KS_END);
}

KeySet * set_mountpoints (void)
{
	return ksNew (10, keyNew ("user", KEY_VALUE, "user", KEY_END), keyNew ("user/tests", KEY_VALUE, "tests", KEY_END),
		      keyNew ("user/tests/hosts", KEY_VALUE, "hosts", KEY_END),
		      keyNew ("user/tests/hosts/below", KEY_VALUE, "below", KEY_END), keyNew ("system", KEY_VALUE, "system", KEY_END),
		      keyNew ("system/tests", KEY_VALUE, "systests", KEY_END),
		      keyNew ("system/tests/hosts", KEY_VALUE, "syshosts", KEY_END),
		      keyNew ("system/tests/hosts/below", KEY_VALUE, "sysbelow", KEY_END), KS_END);
}

static void test_moreiterate (void)
{
	printf ("Test moreiterate trie\n");

	Trie * trie = test_insert (0, "user", "user");
	trie = test_insert (trie, "user/tests", "tests");
	trie = test_insert (trie, "user/tests/hosts", "hosts");
	trie = test_insert (trie, "user/tests/hosts/below", "below");
	trie = test_insert (trie, "system", "system");
	trie = test_insert (trie, "system/tests", "systests");
	trie = test_insert (trie, "system/tests/hosts", "syshosts");
	trie = test_insert (trie, "system/tests/hosts/below", "sysbelow");

	KeySet * mps = set_mountpoints ();

	exit_if_fail (trie, "trie was not build up successfully");

	Key * searchKey = keyNew (0);

	keySetName (searchKey, "user");
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	compare_key (backendGetMountpoint (backend), ksLookupByName (mps, "user", 0));
	// printf ("backend: %p\n", (void*)backend);


	keySetName (searchKey, "user/tests/hosts/other/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "user/tests/hosts", 0));
	// printf ("b2: %p\n", (void*)b2);


	keySetName (searchKey, "user/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "user/tests/hosts", 0));


	keySetName (searchKey, "user/tests/hosts/below");
	Plugin * b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	if (b3) compare_key (backendGetMountpoint (b3), ksLookupByName (mps, "user/tests/hosts/below", 0));
	// printf ("b3: %p\n", (void*)b3);


	keySetName (searchKey, "user/tests/hosts/below/other/deep/below");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), ksLookupByName (mps, "user/tests/hosts/below", 0));

	keySetName (searchKey, "system");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), ksLookupByName (mps, "system", 0));
	// printf ("backend: %p\n", (void*)backend);


	keySetName (searchKey, "system/tests/hosts/other/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "system/tests/hosts", 0));
	// printf ("b2: %p\n", (void*)b2);


	keySetName (searchKey, "system/tests/hosts/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "system/tests/hosts", 0));


	keySetName (searchKey, "system/tests/hosts/below");
	b3 = trieLookup (trie, searchKey);
	succeed_if (b3, "there should be a backend");
	if (b3) compare_key (backendGetMountpoint (b3), ksLookupByName (mps, "system/tests/hosts/below", 0));
	// printf ("b3: %p\n", (void*)b3);


	keySetName (searchKey, "system/tests/hosts/below/other/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "system/tests/hosts/below", 0));

	KeySet * mps_cmp = ksNew (0, KS_END);
	collect_mountpoints (trie, mps_cmp);
	succeed_if (ksGetSize (mps_cmp) == 8, "size should be 8");
	compare_keyset (mps, mps_cmp);

	ksDel (mps_cmp);
	ksDel (mps);

	trieClose (trie, 0);
	keyDel (searchKey);
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
			trie = test_insert (trie, "user/tests", "tests");
			trie = test_insert (trie, "user/tests/hosts", "hosts");
			trie = test_insert (trie, "user/tests/hosts/below", "below");
			trie = test_insert (trie, "system/tests", "systests");
			trie = test_insert (trie, "system/tests/hosts", "syshosts");
			trie = test_insert (trie, "system/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system", "system");
			trie = test_insert (trie, "user", "user");
			break;
		case 1:
			trie = test_insert (trie, "system/tests/hosts", "syshosts");
			trie = test_insert (trie, "system", "system");
			trie = test_insert (trie, "user/tests", "tests");
			trie = test_insert (trie, "user/tests/hosts", "hosts");
			trie = test_insert (trie, "user/tests/hosts/below", "below");
			trie = test_insert (trie, "system/tests", "systests");
			trie = test_insert (trie, "user", "user");
			trie = test_insert (trie, "system/tests/hosts/below", "sysbelow");
			break;
		case 2:
			trie = test_insert (trie, "system/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system/tests/hosts", "syshosts");
			trie = test_insert (trie, "user/tests/hosts/below", "below");
			trie = test_insert (trie, "user/tests/hosts", "hosts");
			trie = test_insert (trie, "user/tests", "tests");
			trie = test_insert (trie, "user", "user");
			trie = test_insert (trie, "system/tests", "systests");
			trie = test_insert (trie, "system", "system");
			break;
		case 3:
			trie = test_insert (trie, "user/tests/hosts/below", "below");
			trie = test_insert (trie, "user/tests/hosts", "hosts");
			trie = test_insert (trie, "user/tests", "tests");
			trie = test_insert (trie, "user", "user");
			trie = test_insert (trie, "system/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system/tests/hosts", "syshosts");
			trie = test_insert (trie, "system/tests", "systests");
			trie = test_insert (trie, "system", "system");
			break;
		case 4:
			trie = test_insert (trie, "system/tests/hosts/below", "sysbelow");
			trie = test_insert (trie, "system/tests/hosts", "syshosts");
			trie = test_insert (trie, "system/tests", "systests");
			trie = test_insert (trie, "system", "system");
			trie = test_insert (trie, "user/tests/hosts/below", "below");
			trie = test_insert (trie, "user/tests/hosts", "hosts");
			trie = test_insert (trie, "user/tests", "tests");
			trie = test_insert (trie, "user", "user");
			break;
		}

		KeySet * mps = set_mountpoints ();

		exit_if_fail (trie, "trie was not build up successfully");

		Key * searchKey = keyNew (0);

		keySetName (searchKey, "user");
		Plugin * backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), ksLookupByName (mps, "user", 0));
		// printf ("backend: %p\n", (void*)backend);


		keySetName (searchKey, "user/tests/hosts/other/below");
		Plugin * b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "user/tests/hosts", 0));
		// printf ("b2: %p\n", (void*)b2);


		keySetName (searchKey, "user/tests/hosts/other/deep/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "user/tests/hosts", 0));


		keySetName (searchKey, "user/tests/hosts/below");
		Plugin * b3 = trieLookup (trie, searchKey);
		succeed_if (b3, "there should be a backend");
		if (b3) compare_key (backendGetMountpoint (b3), ksLookupByName (mps, "user/tests/hosts/below", 0));
		// printf ("b3: %p\n", (void*)b3);


		keySetName (searchKey, "user/tests/hosts/below/other/deep/below");
		backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), ksLookupByName (mps, "user/tests/hosts/below", 0));

		keySetName (searchKey, "system");
		backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), ksLookupByName (mps, "system", 0));
		// printf ("backend: %p\n", (void*)backend);


		keySetName (searchKey, "system/tests/hosts/other/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "system/tests/hosts", 0));
		// printf ("b2: %p\n", (void*)b2);


		keySetName (searchKey, "system/tests/hosts/other/deep/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "system/tests/hosts", 0));


		keySetName (searchKey, "system/tests/hosts/below");
		b3 = trieLookup (trie, searchKey);
		succeed_if (b3, "there should be a backend");
		if (b3) compare_key (backendGetMountpoint (b3), ksLookupByName (mps, "system/tests/hosts/below", 0));
		// printf ("b3: %p\n", (void*)b3);


		keySetName (searchKey, "system/tests/hosts/below/other/deep/below");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		if (b2) compare_key (backendGetMountpoint (b2), ksLookupByName (mps, "system/tests/hosts/below", 0));

		/*
		printf ("---------\n");
		output_trie(trie);
		*/

		KeySet * mps_cmp = ksNew (0, KS_END);
		collect_mountpoints (trie, mps_cmp);
		succeed_if (ksGetSize (mps_cmp) == 8, "size should be 8");
		compare_keyset (mps, mps_cmp);

		ksDel (mps_cmp);
		ksDel (mps);

		trieClose (trie, 0);
		keyDel (searchKey);

	} // end for
}


static void test_umlauts (void)
{
	printf ("Test umlauts trie\n");

	Trie * trie = test_insert (0, "user/umlauts/test", "slash");
	trie = test_insert (trie, "user/umlauts#test", "hash");
	trie = test_insert (trie, "user/umlauts test", "space");
	trie = test_insert (trie, "user/umlauts\200test", "umlauts");

	exit_if_fail (trie, "trie was not build up successfully");

	Key * searchKey = keyNew ("user", KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key * mp = keyNew ("user/umlauts/test", KEY_VALUE, "slash", KEY_END);
	keySetName (searchKey, "user/umlauts/test");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	keySetName (searchKey, "user/umlauts#test");
	keySetName (mp, "user/umlauts#test");
	keySetString (mp, "hash");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend != b2, "should be other backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	keySetName (searchKey, "user/umlauts test");
	keySetName (mp, "user/umlauts test");
	keySetString (mp, "space");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend != b2, "should be other backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	keySetName (searchKey, "user/umlauts\200test");
	keySetName (mp, "user/umlauts\200test");
	keySetString (mp, "umlauts");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend != b2, "should be other backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	// output_trie(trie);

	trieClose (trie, 0);
	keyDel (mp);
	keyDel (searchKey);
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
			trie = test_insert (trie, "user/endings/", "slash");
			trie = test_insert (trie, "user/endings#", "hash");
			trie = test_insert (trie, "user/endings ", "space");
			trie = test_insert (trie, "user/endings\200", "endings");
			break;
		case 1:
			trie = test_insert (trie, "user/endings#", "hash");
			trie = test_insert (trie, "user/endings ", "space");
			trie = test_insert (trie, "user/endings\200", "endings");
			trie = test_insert (trie, "user/endings/", "slash");
			break;
		case 2:
			trie = test_insert (trie, "user/endings ", "space");
			trie = test_insert (trie, "user/endings\200", "endings");
			trie = test_insert (trie, "user/endings/", "slash");
			trie = test_insert (trie, "user/endings#", "hash");
			break;
		case 3:
			trie = test_insert (trie, "user/endings\200", "endings");
			trie = test_insert (trie, "user/endings ", "space");
			trie = test_insert (trie, "user/endings#", "hash");
			trie = test_insert (trie, "user/endings/", "slash");
			break;
		}

		exit_if_fail (trie, "trie was not build up successfully");

		Key * searchKey = keyNew ("user", KEY_END);
		Plugin * backend = trieLookup (trie, searchKey);
		succeed_if (!backend, "there should be no backend");


		Key * mp = keyNew ("user/endings", KEY_VALUE, "slash", KEY_END);
		keySetName (searchKey, "user/endings");
		backend = trieLookup (trie, searchKey);
		succeed_if (backend, "there should be a backend");
		if (backend) compare_key (backendGetMountpoint (backend), mp);


		keySetName (searchKey, "user/endings#");
		keySetName (mp, "user/endings#");
		keySetString (mp, "hash");
		Plugin * b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend != b2, "should be other backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);


		keySetName (searchKey, "user/endings/_");
		keySetName (mp, "user/endings");
		keySetString (mp, "slash");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend == b2, "should be the same backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);


		keySetName (searchKey, "user/endings/X");
		keySetName (mp, "user/endings");
		keySetString (mp, "slash");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend == b2, "should be the same backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);


		keySetName (searchKey, "user/endings_");
		b2 = trieLookup (trie, searchKey);
		succeed_if (!b2, "there should be no backend");


		keySetName (searchKey, "user/endingsX");
		b2 = trieLookup (trie, searchKey);
		succeed_if (!b2, "there should be no backend");


		keySetName (searchKey, "user/endings!");
		b2 = trieLookup (trie, searchKey);
		succeed_if (!b2, "there should be no backend");


		keySetName (searchKey, "user/endings ");
		keySetName (mp, "user/endings ");
		keySetString (mp, "space");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend != b2, "should be other backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);

		keySetName (searchKey, "user/endings\200");
		keySetName (mp, "user/endings\200");
		keySetString (mp, "endings");
		b2 = trieLookup (trie, searchKey);
		succeed_if (b2, "there should be a backend");
		succeed_if (backend != b2, "should be other backend");
		if (b2) compare_key (backendGetMountpoint (b2), mp);

		// output_trie(trie);

		trieClose (trie, 0);
		keyDel (mp);
		keyDel (searchKey);
	}
}

static void test_root (void)
{
	printf ("Test trie with root\n");

	Trie * trie = 0;
	trie = test_insert (trie, "", "root");
	trie = test_insert (trie, "user/tests/simple", "simple");

	exit_if_fail (trie, "trie was not build up successfully");

	Key * searchKey = keyNew ("user", KEY_END);
	Key * rmp = keyNew ("", KEY_VALUE, "root", KEY_END);
	Plugin * backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be the root backend");
	if (backend) compare_key (backendGetMountpoint (backend), rmp);


	Key * mp = keyNew ("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	keySetName (searchKey, "user/tests/simple");
	backend = trieLookup (trie, searchKey);
	succeed_if (backend, "there should be a backend");
	if (backend) compare_key (backendGetMountpoint (backend), mp);


	keySetName (searchKey, "user/tests/simple/below");
	Plugin * b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);


	keySetName (searchKey, "user/tests/simple/deep/below");
	b2 = trieLookup (trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	if (b2) compare_key (backendGetMountpoint (b2), mp);

	// output_trie(trie);

	trieClose (trie, 0);
	keyDel (mp);
	keyDel (rmp);
	keyDel (searchKey);
}

static void test_double (void)
{
	printf ("Test double insertion\n");

	Trie * trie = test_insert (0, "", "root");
	succeed_if (trie, "could not insert into trie");

	Trie * t1 = test_insert (trie, "user/tests/simple", "t1");
	succeed_if (t1, "could not insert into trie");
	succeed_if (t1 == trie, "should be the same");

	// output_trie (trie);

	Trie * t2 = test_insert (trie, "user/tests/simple", "t2");
	succeed_if (t2, "could not insert into trie");
	succeed_if (t2 == trie, "should be not the same");

	// output_trie (trie);

	/* ... gets lost

	Trie *t3 = test_insert (trie, "user/tests/simple", "t3");
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
	trie = test_insert (trie, "user/umlauts/b/", "b");
	trie = test_insert (trie, "user/umlauts/a/", "a");
	trie = test_insert (trie, "user/umlauts/", "/");
	trie = test_insert (trie, "user/umlauts/c/", "c");
	trie = test_insert (trie, "user/", "u");

	exit_if_fail (trie, "trie was not build up successfully");

	// output_trie(trie);

	trieClose (trie, 0);
}


int main (int argc, char ** argv)
{
	printf ("TRIE       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

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

	printf ("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
