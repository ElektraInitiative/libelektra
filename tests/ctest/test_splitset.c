/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <../../src/libs/elektra/backend.c>
#include <../../src/libs/elektra/mount.c>
#include <../../src/libs/elektra/split.c>
#include <../../src/libs/elektra/trie.c>
#include <tests_internal.h>

KeySet * set_us (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/user", KEY_END),
		      keyNew ("system/elektra/mountpoints/user/mountpoint", KEY_VALUE, "user", KEY_END),
		      keyNew ("system/elektra/mountpoints/system", KEY_END),
		      keyNew ("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END), KS_END);
}


KeySet * set_three (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/system", KEY_END),
		      keyNew ("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END),
		      keyNew ("system/elektra/mountpoints/userin", KEY_END),
		      keyNew ("system/elektra/mountpoints/userin/mountpoint", KEY_VALUE, "user/invalid", KEY_END),
		      keyNew ("system/elektra/mountpoints/userva", KEY_END),
		      keyNew ("system/elektra/mountpoints/userva/mountpoint", KEY_VALUE, "user/valid", KEY_END), KS_END);
}


KeySet * set_realworld (void)
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		      keyNew ("system/elektra/mountpoints/default", KEY_END),
		      keyNew ("system/elektra/mountpoints/default/mountpoint", KEY_VALUE, "system/elektra", KEY_END),
		      keyNew ("system/elektra/mountpoints/users", KEY_END),
		      keyNew ("system/elektra/mountpoints/users/mountpoint", KEY_VALUE, "system/users", KEY_END),
		      keyNew ("system/elektra/mountpoints/groups", KEY_END),
		      keyNew ("system/elektra/mountpoints/groups/mountpoint", KEY_VALUE, "system/groups", KEY_END),
		      keyNew ("system/elektra/mountpoints/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/hosts/mountpoint", KEY_VALUE, "system/hosts", KEY_END),
		      keyNew ("system/elektra/mountpoints/kde", KEY_END),
		      keyNew ("system/elektra/mountpoints/kde/mountpoint", KEY_VALUE, "user/sw/kde/default", KEY_END),
		      keyNew ("system/elektra/mountpoints/app1", KEY_END),
		      keyNew ("system/elektra/mountpoints/app1/mountpoint", KEY_VALUE, "user/sw/apps/app1/default", KEY_END),
		      keyNew ("system/elektra/mountpoints/app2", KEY_END),
		      keyNew ("system/elektra/mountpoints/app2/mountpoint", KEY_VALUE, "user/sw/apps/app2", KEY_END), KS_END);
}

KDB * kdb_open (void)
{
	KDB * handle = elektraCalloc (sizeof (struct _KDB));
	handle->split = splitNew ();
	handle->modules = ksNew (0, KS_END);
	elektraModulesInit (handle->modules, 0);
	return handle;
}

static void kdb_close (KDB * kdb)
{
	kdbClose (kdb, 0);
}

void simulateGet (Split * split)
{
	for (size_t i = 0; i < split->size; ++i)
	{
		split->specsizes[i] = 0;
		split->dirsizes[i] = 0;
		split->usersizes[i] = 0;
		split->systemsizes[i] = 0;
	}
}


static void test_needsync (void)
{
	printf ("Test needs sync\n");

	KDB * handle = kdb_open ();
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not mount default backends");

	KeySet * ks = ksNew (5, keyNew ("user/abc", KEY_END), KS_END);
	Split * split = splitNew ();

	Key * parent = keyNew ("user", KEY_VALUE, "parent", KEY_END);

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	succeed_if (splitBuildup (split, handle, parent) == 1, "buildup failure");
	succeed_if (splitDivide (split, handle, ks) == 1, "there should be a need sync");

	succeed_if (split->handles[0] == handle->defaultBackend, "handle not correct");
	compare_keyset (split->keysets[0], ks);
	succeed_if (split->syncbits[0] & 1, "sync bit should be set");

	succeed_if (split->size == 1, "size should be one");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");

	splitDel (split);


	split = splitNew ();

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	clear_sync (ks);
	succeed_if (splitBuildup (split, handle, parent) == 1, "buildup failure");
	succeed_if (splitDivide (split, handle, ks) == 0, "there should not be a need sync");
	succeed_if (split->handles[0] == handle->defaultBackend, "handle not correct");
	compare_keyset (split->keysets[0], ks);
	succeed_if ((split->syncbits[0] & 1) == 0, "sync bit should be set");

	succeed_if (split->size == 1, "size should be one");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");

	splitDel (split);


	split = splitNew ();

	ksAppendKey (ks, keyNew ("user/key1", KEY_END));
	ksAppendKey (ks, keyNew ("user/key2", KEY_END));
	ksAppendKey (ks, keyNew ("user/key3", KEY_END));
	ksAppendKey (ks, keyNew ("user/key4", KEY_END));
	ksAppendKey (ks, keyNew ("user/key5", KEY_END));

	succeed_if (splitBuildup (split, handle, parent) == 1, "buildup failure");
	succeed_if (splitDivide (split, handle, ks) == 1, "there should be a need sync");
	succeed_if (split->handles[0] == handle->defaultBackend, "handle not correct");
	compare_keyset (split->keysets[0], ks);
	succeed_if (split->syncbits[0] & 1, "sync bit should be set");
	splitDel (split);


	keyDel (parent);
	ksDel (ks);
	kdb_close (handle);
}


static void test_mount (void)
{
	printf ("Test mount split\n");

	KDB * handle = kdb_open ();

	succeed_if (mountOpen (handle, set_us (), handle->modules, 0) == 0, "could not open mountpoints");
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	KeySet * ks = ksNew (5, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END),
			     keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END), KS_END);
	KeySet * split1 = ksNew (3, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END), KS_END);
	KeySet * split2 = ksNew (3, keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END), KS_END);


	Split * split = splitNew ();
	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
	succeed_if (split->size == 5, "not split according user, system");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 1, "mounted system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "mounted user part need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part need to by synced");
	succeed_if (split->syncbits[4] == 2, "system/elektra part need to by synced");
	if (split->keysets)
	{
		succeed_if (ksGetSize (split->keysets[0]) == 2, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[1]) == 2, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[2]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[3]) == 0, "size of keyset not correct");
		compare_keyset (split->keysets[1], split1);
		compare_keyset (split->keysets[0], split2);
	}
	splitDel (split);


	split = splitNew ();
	clear_sync (ks);
	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 0, "system part does not need to by synced");
	succeed_if (split->syncbits[1] == 0, "user part does not need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part does not need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part does not need to by synced");
	succeed_if (split->syncbits[4] == 2, "user part does not need to by synced");
	if (split->keysets)
	{
		succeed_if (ksGetSize (split->keysets[0]) == 2, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[1]) == 2, "size of keyset not correct");
	}
	succeed_if (split->size == 5, "not split according user, system");
	splitDel (split);

	split = splitNew ();
	keySetString (ksLookupByName (ks, "user/valid/key2", 0), "value");
	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 0, "system part does not need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part does not need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part does not need to by synced");
	succeed_if (split->syncbits[4] == 2, "user part does not need to by synced");
	if (split->keysets)
	{
		succeed_if (ksGetSize (split->keysets[0]) == 2, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[1]) == 2, "size of keyset not correct");
	}
	succeed_if (split->size == 5, "not split according user, system");
	splitDel (split);

	split = splitNew ();
	keySetString (ksLookupByName (ks, "system/valid/key2", 0), "value");
	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part does not need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part does not need to by synced");
	succeed_if (split->syncbits[4] == 2, "user part does not need to by synced");
	if (split->keysets)
	{
		succeed_if (ksGetSize (split->keysets[0]) == 2, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[1]) == 2, "size of keyset not correct");
	}
	succeed_if (split->size == 5, "not split according user, system");
	splitDel (split);


	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	kdb_close (handle);
}

static void test_easyparent (void)
{
	printf ("Test parent separation of user and system (default Backend)\n");

	KDB * handle = kdb_open ();
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");
	KeySet * ks = ksNew (8, keyNew ("user/valid", KEY_END), keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END),
			     keyNew ("system/valid", KEY_END), keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END),
			     KS_END);
	KeySet * split1 = ksNew (5, keyNew ("system/valid", KEY_END), keyNew ("system/valid/key1", KEY_END),
				 keyNew ("system/valid/key2", KEY_END), KS_END);
	KeySet * split2 =
		ksNew (5, keyNew ("user/valid", KEY_END), keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END), KS_END);
	Key * parentKey;
	Split * split;


	parentKey = keyNew ("user", KEY_END);
	split = splitNew ();
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 1, "not split by parent");
	succeed_if (split->syncbits[0] == 3, "user part need not to by synced");
	succeed_if_same_string (keyName (split->parents[0]), "user");
	if (split->keysets)
	{
		compare_keyset (split->keysets[0], split2);
	}

	splitDel (split);
	keyDel (parentKey);

	parentKey = keyNew ("system", KEY_END);
	split = splitNew ();
	succeed_if (splitBuildup (split, handle, parentKey) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 1, "not split by parent");
	succeed_if (split->syncbits[0] == 3, "system part need to by synced");
	succeed_if_same_string (keyName (split->parents[0]), "system");
	if (split->keysets)
	{
		compare_keyset (split->keysets[0], split1);
	}

	splitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	kdb_close (handle);
}

static void test_optimize (void)
{
	printf ("Test optimization split (user, system in trie)\n");

	KDB * handle = kdb_open ();

	succeed_if (mountOpen (handle, set_us (), handle->modules, 0) == 0, "could not open mountpoints");
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	KeySet * ks = ksNew (5, keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END),
			     keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END), KS_END);
	KeySet * split1 = ksNew (3, keyNew ("system/valid/key1", KEY_END), keyNew ("system/valid/key2", KEY_END), KS_END);
	KeySet * split2 = ksNew (3, keyNew ("user/valid/key1", KEY_END), keyNew ("user/valid/key2", KEY_END), KS_END);
	Split * split = splitNew ();
	Key * key;


	ksRewind (ks);
	while ((key = ksNext (ks)) != 0)
	{
		if (keyIsUser (key) == 1) keyClearSync (key);
	}

	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 5, "not split according user, system");
	succeed_if (split->syncbits[0] == 1, "system part not optimized");
	succeed_if (split->syncbits[1] == 0, "user part need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part need to by synced");
	succeed_if (split->syncbits[4] == 2, "system/elektra part need to by synced");
	compare_keyset (split->keysets[0], split1);
	compare_keyset (split->keysets[1], split2);
	if (split->keysets)
	{
		succeed_if (ksGetSize (split->keysets[2]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[3]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[4]) == 0, "size of keyset not correct");
	}
	splitDel (split);


	split = splitNew ();
	clear_sync (ks);

	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 5, "not split according user, system");
	succeed_if (split->syncbits[0] == 0, "system part not optimized");
	succeed_if (split->syncbits[1] == 0, "user part need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part need to by synced");
	succeed_if (split->syncbits[4] == 2, "system/elektra part need to by synced");
	if (split->keysets)
	{
		compare_keyset (split->keysets[0], split1);
		compare_keyset (split->keysets[1], split2);
		succeed_if (ksGetSize (split->keysets[2]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[3]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[4]) == 0, "size of keyset not correct");
	}
	splitDel (split);


	ksRewind (ks);
	while ((key = ksNext (ks)) != 0)
	{
		key->flags = KEY_FLAG_SYNC;
	}


	split = splitNew ();
	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 5, "not split according user, system");
	succeed_if (split->syncbits[0] == 1, "system part not optimized");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->syncbits[2] == 2, "spec part need to by synced");
	succeed_if (split->syncbits[3] == 2, "dir part need to by synced");
	succeed_if (split->syncbits[4] == 2, "system/elektra part need to by synced");
	if (split->keysets)
	{
		compare_keyset (split->keysets[0], split1);
		compare_keyset (split->keysets[1], split2);
		succeed_if (ksGetSize (split->keysets[2]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[3]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[4]) == 0, "size of keyset not correct");
	}
	splitDel (split);


	ksDel (ks);
	ksDel (split1);
	ksDel (split2);

	kdb_close (handle);
}

static void test_three (void)
{
	printf ("Test three mountpoints\n");

	KDB * handle = kdb_open ();

	succeed_if (mountOpen (handle, set_three (), handle->modules, 0) == 0, "could not open mountpoints");
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	KeySet * ks = ksNew (18, keyNew ("system/valid", KEY_END), keyNew ("system/valid/key1", KEY_END),
			     keyNew ("system/valid/key2", KEY_END), keyNew ("system/valid/key3", KEY_END), keyNew ("user/invalid", KEY_END),
			     keyNew ("user/invalid/key1", KEY_END), keyNew ("user/invalid/key2", KEY_END), keyNew ("user/valid", KEY_END),
			     keyNew ("user/valid/key1", KEY_END), keyNew ("user/outside", KEY_END), KS_END);
	KeySet * split0 = ksNew (9, keyNew ("system/valid", KEY_END), keyNew ("system/valid/key1", KEY_END),
				 keyNew ("system/valid/key2", KEY_END), keyNew ("system/valid/key3", KEY_END), KS_END);
	KeySet * split1 = ksNew (9, keyNew ("user/invalid", KEY_END), keyNew ("user/invalid/key1", KEY_END),
				 keyNew ("user/invalid/key2", KEY_END), KS_END);
	KeySet * split2 = ksNew (9, keyNew ("user/valid", KEY_END), keyNew ("user/valid/key1", KEY_END), KS_END);
	KeySet * split5 = ksNew (9, keyNew ("user/outside", KEY_END), KS_END);


	Split * split = splitNew ();
	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 7, "not split according three");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user invalid part need to by synced");
	succeed_if (split->syncbits[2] == 1, "user valid part need to by synced");
	succeed_if (split->syncbits[3] == 2, "spec part need to by synced");
	succeed_if (split->syncbits[4] == 2, "dirpart need to by synced");
	succeed_if (split->syncbits[5] == 3, "user default part need to by synced");
	succeed_if (split->syncbits[6] == 2, "system/elektra default part need to by synced");
	if (split->keysets)
	{
		succeed_if (ksGetSize (split->keysets[0]) == 4, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[1]) == 3, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[2]) == 2, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[3]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[4]) == 0, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[5]) == 1, "size of keyset not correct");
		succeed_if (ksGetSize (split->keysets[6]) == 0, "size of keyset not correct");
		compare_keyset (split->keysets[0], split0);
		compare_keyset (split->keysets[1], split1);
		compare_keyset (split->keysets[2], split2);
		compare_keyset (split->keysets[5], split5);
	}

	splitPrepare (split);

	/* Prepare should not change anything here, except discarding
	 * not needed backends (spec, dir) */
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 4, "not split according three (with one default)");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->syncbits[2] == 1, "user part need to by synced");
	succeed_if (split->syncbits[3] == 3, "user default part need to by synced");
	succeed_if (ksGetSize (split->keysets[0]) == 4, "size of keyset not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 3, "size of keyset not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 2, "size of keyset not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 1, "size of keyset not correct");
	compare_keyset (split->keysets[0], split0);
	compare_keyset (split->keysets[1], split1);
	compare_keyset (split->keysets[2], split2);
	compare_keyset (split->keysets[3], split5); // 3 with 5 because of prepare

	splitDel (split);


	ksDel (ks);
	ksDel (split0);
	ksDel (split1);
	ksDel (split2);
	ksDel (split5);
	kdb_close (handle);
}

static void test_userremove (void)
{
//	printf ("Test user removing\n");
//	Key * parent = 0;
//	KDB * handle = kdb_open ();
//
//	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");
//	/* So we had 2 keys before in the keyset */
//
//	KeySet * ks = ksNew (3, keyNew ("user/valid/key", KEY_END), KS_END);
//
//
//	Split * split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
//	succeed_if (splitSync (split) == -2, "should be out of sync");
//	simulateGet (split);
//	handle->defaultBackend->usersize = 2;
//	succeed_if (splitSync (split) == 1, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "split size wrong");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 1, "wrong size");
//		compare_keyset (split->keysets[2], ks);
//		succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
//	}
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("user/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
//	succeed_if (splitSync (split) == 1, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in two keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 1, "wrong size");
//		compare_keyset (split->keysets[0], ks);
//	}
//	keyDel (parent);
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("system/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should need sync");
//	succeed_if (splitSync (split) == 0, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in two keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "should be dropped");
//	}
//	keyDel (parent);
//
//	splitDel (split);
//
//
//	/* But it should even need sync when we don't have any unsynced keys! */
//	clear_sync (ks);
//	split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "no key inside needs sync");
//	succeed_if (splitSync (split) == 1, "but we need sync because of the size mismatch");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "wrong size of split");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 1, "wrong size");
//		compare_keyset (split->keysets[2], ks);
//		succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
//	}
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("user/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should need sync");
//	succeed_if (splitSync (split) == 1, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in two keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 1, "wrong size");
//		compare_keyset (split->keysets[0], ks);
//	}
//	keyDel (parent);
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("system/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	succeed_if (splitSync (split) == 0, "should not need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in two keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "should be dropped");
//	}
//	keyDel (parent);
//
//	splitPrepare (split);
//	succeed_if (split->size == 0, "split not empty");
//
//	splitDel (split);
//
//
//	ksDel (ks);
//	kdb_close (handle);
}


static void test_systemremove (void)
{
//	printf ("Test system removing\n");
//	Key * parent = 0;
//	KDB * handle = kdb_open ();
//
//	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");
//
//	KeySet * ks = ksNew (3, keyNew ("system/valid/key", KEY_END), KS_END);
//
//
//	Split * split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
//	simulateGet (split);
//	handle->defaultBackend->systemsize = 2;
//	/* So we had 2 keys before in the keyset */
//	succeed_if (splitSync (split) == 1, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "everything is in two keyset");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[3]) == 1, "wrong size");
//		compare_keyset (split->keysets[3], ks);
//	}
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("system/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
//	succeed_if (splitSync (split) == 1, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in one keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 1, "wrong size");
//		compare_keyset (split->keysets[0], ks);
//	}
//	keyDel (parent);
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("user/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	simulateGet (split);
//	succeed_if (splitSync (split) == 0, "should not need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in one keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "should be dropped");
//	}
//	keyDel (parent);
//
//	splitDel (split);
//
//
//	/* But it should even need sync when we don't have any unsynced keys! */
//	clear_sync (ks);
//	split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, 0) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "no key inside needs sync");
//	succeed_if (splitSync (split) == 1, "but we need sync because of the size mismatch");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "everything is in two keyset");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[3]) == 1, "wrong size");
//		compare_keyset (split->keysets[3], ks);
//	}
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("system/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	succeed_if (splitSync (split) == 1, "should need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in two keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 1, "wrong size");
//		compare_keyset (split->keysets[0], ks);
//	}
//	keyDel (parent);
//
//	splitDel (split);
//
//
//	split = splitNew ();
//
//	parent = keyNew ("user/valid", KEY_END);
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	succeed_if (splitSync (split) == 0, "should not need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 1, "everything is in two keyset");
//	// output_split(split);
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "should be dropped");
//	}
//	keyDel (parent);
//
//	splitPrepare (split);
//	succeed_if (split->size == 0, "no remaining keyset");
//
//	splitDel (split);
//
//
//	ksDel (ks);
//	kdb_close (handle);
}


static void test_emptyremove (void)
{
//	printf ("Test empty removing\n");
//
//	KDB * handle = kdb_open ();
//
//	Key * parent = 0;
//	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");
//
//	KeySet * ks = ksNew (3, KS_END);
//
//
//	Split * split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	simulateGet (split);
//	succeed_if (splitSync (split) == 0, "should not need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "there is an empty keset");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
//	}
//
//	splitDel (split);
//
//
//	handle->defaultBackend->usersize = 2;
//	handle->defaultBackend->systemsize = 0;
//	split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	succeed_if (splitSync (split) == 1, "should not need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "there is an empty keset");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
//	}
//
//	splitDel (split);
//
//
//	handle->defaultBackend->usersize = 2;
//	handle->defaultBackend->systemsize = 0;
//	split = splitNew ();
//
//	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
//	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");
//	succeed_if (splitSync (split) == 1, "should not need sync");
//
//	succeed_if (split->keysets, "did not alloc keysets array");
//	succeed_if (split->handles, "did not alloc handles array");
//	succeed_if (split->size == 4, "there is an empty keset");
//	if (split->keysets)
//	{
//		succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
//		succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
//	}
//	splitPrepare (split);
//	succeed_if (split->size == 1, "there is an empty keset");
//	succeed_if_same_string (keyName (split->parents[0]), "user");
//	succeed_if_same_string (keyValue (split->parents[0]), "default");
//
//	splitDel (split);
//
//
//	ksDel (ks);
//	kdb_close (handle);
//	keyDel (parent);
}

static void test_realworld (void)
{
	printf ("Test real world example\n");

	Key * parent = 0;
	KDB * handle = kdb_open ();

	succeed_if (mountOpen (handle, set_realworld (), handle->modules, 0) == 0, "could not open mountpoints");
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	KeySet * split0 = ksNew (9, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/new", KEY_END),
				 keyNew ("system/elektra/mountpoints/new/mountpoint", KEY_VALUE, "something", KEY_END), KS_END);
	KeySet * split2 = ksNew (9, keyNew ("system/hosts", KEY_END), keyNew ("system/hosts/markusbyte", KEY_VALUE, "127.0.0.1", KEY_END),
				 keyNew ("system/hosts/mobilebyte", KEY_END), keyNew ("system/hosts/n900", KEY_END), KS_END);
	KeySet * split3 = ksNew (9, keyNew ("system/users", KEY_END), keyNew ("system/users/markus", KEY_END),
				 keyNew ("system/users/harald", KEY_END), keyNew ("system/users/n", KEY_END),
				 keyNew ("system/users/albert", KEY_END), KS_END);
	KeySet * split4 = ksNew (9, keyNew ("user/sw/apps/app1/default", KEY_END),
				 keyNew ("user/sw/apps/app1/default/maximize", KEY_VALUE, "1", KEY_END),
				 keyNew ("user/sw/apps/app1/default/download", KEY_VALUE, "0", KEY_END),
				 keyNew ("user/sw/apps/app1/default/keys/a", KEY_VALUE, "a", KEY_END),
				 keyNew ("user/sw/apps/app1/default/keys/b", KEY_VALUE, "b", KEY_END),
				 keyNew ("user/sw/apps/app1/default/keys/c", KEY_VALUE, "c", KEY_END), KS_END);
	KeySet * split7 = ksNew (3, keyNew ("user/outside", KEY_VALUE, "test", KEY_END), KS_END);
	KeySet * splitX = ksNew (3, keyNew ("spec/testS", KEY_VALUE, "testS", KEY_END), KS_END);
	KeySet * splitY = ksNew (3, keyNew ("dir/testD", KEY_VALUE, "testD", KEY_END), KS_END);
	KeySet * tmp = ksNew (30, KS_END);
	ksAppend (tmp, split0);
	ksAppend (tmp, split2);
	ksAppend (tmp, split3);
	ksAppend (tmp, split4);
	ksAppend (tmp, split7);
	ksAppend (tmp, splitX);
	ksAppend (tmp, splitY);
	KeySet * ks = ksDeepDup (tmp);
	ksDel (tmp);


	Split * split = splitNew ();

	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (split->size == 12, "size of split not correct");
	succeed_if (split->syncbits[0] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (split->syncbits[1] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
	succeed_if (split->syncbits[4] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[4]) == 0, "wrong size");
	succeed_if (split->syncbits[5] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[5]) == 0, "wrong size");
	succeed_if (split->syncbits[6] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[6]) == 0, "wrong size");
	succeed_if (split->syncbits[7] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[7]) == 0, "wrong size");
	succeed_if (split->syncbits[8] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[8]) == 0, "wrong size");
	succeed_if (split->syncbits[9] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[9]) == 0, "wrong size");
	succeed_if (split->syncbits[10] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[10]) == 0, "wrong size");
	succeed_if (split->syncbits[11] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[11]) == 0, "wrong size");

	succeed_if (splitDivide (split, handle, ks) == 1, "should need sync");
	succeed_if (split->size == 12, "size of split not correct");
	succeed_if (split->syncbits[0] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 6, "wrong size");
	succeed_if (split->syncbits[1] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
	succeed_if (split->syncbits[4] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[4]) == 4, "wrong size");
	succeed_if (split->syncbits[5] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[5]) == 0, "wrong size");
	succeed_if (split->syncbits[6] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[6]) == 1, "wrong size");
	succeed_if (split->syncbits[7] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[7]) == 1, "wrong size");
	succeed_if (split->syncbits[8] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[8]) == 1, "wrong size");
	succeed_if (split->syncbits[9] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[9]) == 0, "wrong size");
	succeed_if (split->syncbits[10] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[10]) == 5, "wrong size");
	succeed_if (split->syncbits[11] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[11]) == 3, "wrong size");

	simulateGet (split);
	split->usersizes[5] = 5;
	split->systemsizes[8] = 12;
	succeed_if (splitSync (split) == 1, "should need sync");
	succeed_if (split->size == 12, "size of split not correct");
	succeed_if (split->syncbits[0] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 6, "wrong size");
	succeed_if (split->syncbits[1] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
	succeed_if (split->syncbits[4] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[4]) == 4, "wrong size");
	succeed_if (split->syncbits[5] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[5]) == 0, "wrong size");
	succeed_if (split->syncbits[6] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[6]) == 1, "wrong size");
	succeed_if (split->syncbits[7] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[7]) == 1, "wrong size");
	succeed_if (split->syncbits[8] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[8]) == 1, "wrong size");
	succeed_if (split->syncbits[9] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[9]) == 0, "wrong size");
	succeed_if (split->syncbits[10] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[10]) == 5, "wrong size");
	succeed_if (split->syncbits[11] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[11]) == 3, "wrong size");


	split->usersizes[5] = 0;
	split->systemsizes[8] = 0;
	splitDel (split);


	clear_sync (ks);
	split = splitNew ();
	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (split->size == 12, "size of split not correct");
	succeed_if (split->syncbits[0] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (split->syncbits[1] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
	succeed_if (split->syncbits[4] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[4]) == 0, "wrong size");
	succeed_if (split->syncbits[5] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[5]) == 0, "wrong size");
	succeed_if (split->syncbits[6] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[6]) == 0, "wrong size");
	succeed_if (split->syncbits[7] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[7]) == 0, "wrong size");
	succeed_if (split->syncbits[8] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[8]) == 0, "wrong size");
	succeed_if (split->syncbits[9] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[9]) == 0, "wrong size");
	succeed_if (split->syncbits[10] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[10]) == 0, "wrong size");
	succeed_if (split->syncbits[11] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[11]) == 0, "wrong size");

	succeed_if (splitDivide (split, handle, ks) == 0, "does not need sync anymore");
	succeed_if (split->size == 12, "size of split not correct");
	succeed_if (split->syncbits[0] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 6, "wrong size");
	succeed_if (split->syncbits[1] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
	succeed_if (split->syncbits[4] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[4]) == 4, "wrong size");
	succeed_if (split->syncbits[5] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[5]) == 0, "wrong size");
	succeed_if (split->syncbits[6] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[6]) == 1, "wrong size");
	succeed_if (split->syncbits[7] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[7]) == 1, "wrong size");
	succeed_if (split->syncbits[8] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[8]) == 1, "wrong size");
	succeed_if (split->syncbits[9] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[9]) == 0, "wrong size");
	succeed_if (split->syncbits[10] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[10]) == 5, "wrong size");
	succeed_if (split->syncbits[11] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[11]) == 3, "wrong size");

	succeed_if (splitSync (split) == 1, "should need sync, because of removes");
	succeed_if (split->size == 12, "size of split not correct");
	succeed_if (split->syncbits[0] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 6, "wrong size");
	succeed_if (split->syncbits[1] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");
	succeed_if (split->syncbits[4] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[4]) == 4, "wrong size");
	succeed_if (split->syncbits[5] == 0, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[5]) == 0, "wrong size");
	succeed_if (split->syncbits[6] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[6]) == 1, "wrong size");
	succeed_if (split->syncbits[7] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[7]) == 1, "wrong size");
	succeed_if (split->syncbits[8] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[8]) == 1, "wrong size");
	succeed_if (split->syncbits[9] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[9]) == 0, "wrong size");
	succeed_if (split->syncbits[10] == 1, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[10]) == 5, "wrong size");
	succeed_if (split->syncbits[11] == 3, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[11]) == 3, "wrong size");

	splitDel (split);


	split = splitNew ();
	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (split->size == 12, "size not correct");
	succeed_if (splitDivide (split, handle, ks) == 0, "does not need sync anymore");
	split->usersizes[0] = 6;
	split->systemsizes[4] = 4;
	split->specsizes[6] = 1;
	split->dirsizes[7] = 1;
	split->usersizes[9] = 1;
	split->systemsizes[10] = 5;
	split->systemsizes[11] = 3;
	succeed_if (splitSync (split) == 0, "no sync needed");
	splitDel (split);


	split = splitNew ();
	succeed_if (splitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (split->size == 12, "size not correct");
	succeed_if (splitDivide (split, handle, ks) == 0, "does not need sync anymore");
	split->usersizes[0] = 6;
	split->systemsizes[4] = 2; /* Changed */
	split->specsizes[6] = 1;
	split->dirsizes[7] = 1;
	split->usersizes[9] = 1;
	split->systemsizes[10] = 5;
	split->systemsizes[11] = 3;
	succeed_if (splitSync (split) == 1, "sync needed because one size not correct");

	splitPrepare (split);
	succeed_if (split->size == 1, "should be 1, only system/hosts to sync");
	succeed_if_same_string (keyName (split->parents[0]), "system/hosts");
	succeed_if_same_string (keyValue (split->parents[0]), "hosts");

	splitDel (split);


	ksDel (ks);
	ksDel (split0);
	ksDel (split2);
	ksDel (split3);
	ksDel (split4);
	ksDel (split7);
	ksDel (splitX);
	ksDel (splitY);
	keyDel (parent);
	kdb_close (handle);
}


static void test_emptysplit (void)
{
	printf ("Test empty split\n");

	KDB * handle = kdb_open ();
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	KeySet * ks = ksNew (0, KS_END);
	Split * split = splitNew ();
	Key * parentKey;

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	succeed_if (splitBuildup (split, handle, 0) == 1, "default backend should be added");
	succeed_if (split->size == 4, "size of split not correct");
	succeed_if (split->syncbits[0] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	succeed_if (split->syncbits[1] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[1]) == 0, "wrong size");
	succeed_if (split->syncbits[2] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[2]) == 0, "wrong size");
	succeed_if (split->syncbits[3] == 2, "size of split not correct");
	succeed_if (ksGetSize (split->keysets[3]) == 0, "wrong size");

	parentKey = keyNew ("spec", KEY_VALUE, "default", KEY_END);
	compare_key (split->parents[0], parentKey);
	keyDel (parentKey);

	parentKey = keyNew ("dir", KEY_VALUE, "default", KEY_END);
	compare_key (split->parents[1], parentKey);
	keyDel (parentKey);

	parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);
	compare_key (split->parents[2], parentKey);
	keyDel (parentKey);

	parentKey = keyNew ("system", KEY_VALUE, "default", KEY_END);
	compare_key (split->parents[3], parentKey);
	keyDel (parentKey);

	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->handles[1] == handle->defaultBackend, "not correct backend");
	succeed_if (split->handles[2] == handle->defaultBackend, "not correct backend");
	succeed_if (split->handles[3] == handle->defaultBackend, "not correct backend");

	succeed_if (split->syncbits[0] == 2, "should be marked as default");
	succeed_if (split->syncbits[1] == 2, "should be marked as default");
	succeed_if (split->syncbits[2] == 2, "should be marked as default");
	succeed_if (split->syncbits[3] == 2, "should be marked as default");

	succeed_if (splitDivide (split, handle, ks) == 0, "there should be no added key");

	succeed_if (split->size == 4, "divide should never changes size");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	splitDel (split);
	ksDel (ks);
	kdb_close (handle);
}

static void test_nothingsync (void)
{
	printf ("Test buildup with nothing to sync\n");
	KDB * handle = kdb_open ();
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	KeySet * ks = ksNew (0, KS_END);

	Split * split = splitNew ();
	Key * parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);

	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");

	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong size");
	compare_key (split->parents[0], parentKey);
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	succeed_if (splitDivide (split, handle, ks) == 0, "does not need sync anymore");
	simulateGet (split);
	succeed_if (splitSync (split) == 0, "nothing to sync");
	splitPrepare (split);
	succeed_if (split->size == 0, "there should be nothing to sync");

	splitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	kdb_close (handle);
}

static void test_state (void)
{
	printf ("Test state conflicts\n");
	KDB * handle = kdb_open ();
	succeed_if (mountDefault (handle, handle->modules, 1, 0) == 0, "could not open default backend");

	Key * k;
	KeySet * ks = ksNew (2, k = keyNew ("user/abc", KEY_END), KS_END);

	Split * split = splitNew ();
	Key * parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);

	succeed_if (splitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");

	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize (split->keysets[0]) == 0, "wrong init size");
	compare_key (split->parents[0], parentKey);
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	succeed_if (splitDivide (split, handle, ks) == 1, "does not need sync anymore");
	clear_bit (k->flags, KEY_FLAG_SYNC);
	succeed_if (splitDivide (split, handle, ks) == 0, "should not need sync");

	succeed_if (ksGetSize (split->keysets[0]) == 1, "wrong size");
	succeed_if (splitSync (split) == -2, "state error: should fail");

	split->usersizes[0] = 1;
	succeed_if (ksGetSize (split->keysets[0]) == 1, "wrong size");
	// output_split (split);
	succeed_if (splitSync (split) == 0, "state nothing to do: same size");

	split->usersizes[0] = 3;
	succeed_if (splitSync (split) == 1, "state should sync: other size");
	splitPrepare (split);
	succeed_if (split->size == 1, "there should be nothing to sync");

	splitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	kdb_close (handle);
}


int main (int argc, char ** argv)
{
	printf ("SPLIT SET   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_needsync ();
	test_mount ();
	test_easyparent ();
	test_optimize ();
	test_three ();
	test_userremove ();
	test_systemremove ();
	test_emptyremove ();
	test_realworld ();
	test_emptysplit ();
	test_nothingsync ();
	test_state ();

	printf ("\ntest_splitset RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
