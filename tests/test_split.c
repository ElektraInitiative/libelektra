/*************************************************************************** 
 *           test_split.c  - Test suite for splitted keyset data structure
 *                  -------------------
 *  begin                : Fri 21 Mar 2008
 *  copyright            : (C) 2008 by Markus Raab
 *  email                : elektra@markus-raab.org
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <tests.h>

void test_create()
{
	printf ("Test create split\n");

	Split *split = elektraSplitNew();
	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "alloc not correct");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");

	for (size_t i=1; i<= APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		elektraSplitAppend(split);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	elektraSplitDel (split);
}

void test_resize()
{
	printf ("Test resize split\n");

	Split *split = elektraSplitNew();

	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	elektraSplitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS*2, "resize not correct");

	elektraSplitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS*4, "resize not correct");

	elektraSplitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS*8, "resize not correct");

	elektraSplitDel (split);
}

void test_append()
{
	printf ("Test append split\n");

	Split *split = elektraSplitNew();
	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	for (size_t i=1; i<= APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		elektraSplitAppend(split);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	for (size_t i=APPROXIMATE_NR_OF_BACKENDS+1; i<= APPROXIMATE_NR_OF_BACKENDS*2; ++i)
	{
		elektraSplitAppend(split);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS*2, "should realloc");
	}

	elektraSplitDel (split);
}


void test_emptysplit()
{
	printf ("Test empty split\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *ks = ksNew (0);
	Split *split = elektraSplitNew();

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	succeed_if (elektraSplitSync (split, handle, ks) == 0, "there should be no need sync");

	succeed_if (split->size == 0, "empty requires no appending");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	elektraSplitDel (split);
	ksDel (ks);
	elektraFree(handle);
}

void test_needsSync()
{
	printf ("Test needs sync\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *ks = ksNew (5,
			keyNew("user/abc", KEY_END),
			KS_END);
	Split *split = elektraSplitNew();

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	succeed_if (elektraSplitSync (split, handle, ks) == 1, "there should be a need sync");

	elektraSplitDel (split);

	split = elektraSplitNew();

	clear_sync (ks);
	succeed_if (elektraSplitSync (split, handle, ks) == 0, "there should not be a need sync");

	succeed_if (split->size == 1, "size should be one");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");

	elektraSplitDel (split);


	split = elektraSplitNew();
	ksAppendKey(ks, keyNew("user/key1", KEY_END));
	ksAppendKey(ks, keyNew("user/key2", KEY_END));
	ksAppendKey(ks, keyNew("user/key3", KEY_END));
	ksAppendKey(ks, keyNew("user/key4", KEY_END));
	ksAppendKey(ks, keyNew("user/key5", KEY_END));
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "there should be a need sync");
	elektraSplitDel (split);


	split = elektraSplitNew();
	clear_sync (ks);
	succeed_if (elektraSplitSync (split, handle, ks) == 0, "there should not be a need sync");

	succeed_if (split->size == 1, "size should be one");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");
	elektraSplitDel (split);



	split = elektraSplitNew();
	succeed_if (elektraSplitSync (split, handle, ks) == 0, "there should not be a need sync (again)");

	succeed_if (split->size == 1, "size should be one");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");
	elektraSplitDel (split);


	split = elektraSplitNew();
	keySetString(ksLookupByName(ks, "user/key2", 0), "value");
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "there should not be a need sync (value changed)");

	succeed_if (split->size == 1, "size should be one");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");
	elektraSplitDel (split);


	ksDel (ks);
	elektraFree(handle);
}

void test_easysplit()
{
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *ks = ksNew (
		3,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	Split *split = elektraSplitNew();

	elektraSplitSync (split, handle, ks);

	printf ("Test easy split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 1, "everything is in one keyset");
	compare_keyset (split->keysets[0], ks);

	elektraSplitDel (split);
	ksDel (ks);
	elektraFree(handle);
}

void test_singlesplit()
{
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *ks = ksNew (
		3,
		keyNew ("user/valid/key", KEY_END),
		KS_END);
	Split *split = elektraSplitNew();

	elektraSplitSync (split, handle, ks);

	printf ("Test single split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 1, "everything is in one keyset");
	compare_keyset (split->keysets[0], ks);

	elektraSplitDel (split);
	ksDel (ks);
	elektraFree(handle);
}


KeySet *set_us()
{
	return ksNew(50,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/user", KEY_END),
		keyNew("system/elektra/mountpoints/user/mountpoint", KEY_VALUE, "user", KEY_END),
		keyNew("system/elektra/mountpoints/system", KEY_END),
		keyNew("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END),
		KS_END);
}

void test_mount()
{
	printf ("Test mount split\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);
	handle->trie = elektraTrieOpen(set_us(), modules, 0);

	KeySet *ks = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		3,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		3,
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);

	Split *split = elektraSplitNew();
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 2, "size of keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[0], split2) == 0, "system keyset not correct");
	elektraSplitDel (split);


	split = elektraSplitNew();
	clear_sync (ks);
	succeed_if (elektraSplitSync (split, handle, ks) == 0, "should not need sync");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should stay same");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 0, "system part does not need to by synced");
	succeed_if (split->syncbits[1] == 0, "user part does not need to by synced");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 2, "size of keyset not correct");
	succeed_if (split->size == 2, "not splitted according user, system");
	elektraSplitDel (split);

	split = elektraSplitNew();
	keySetString(ksLookupByName(ks, "user/valid/key2", 0), "value");
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 0, "system part does not need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 2, "size of keyset not correct");
	succeed_if (split->size == 2, "not splitted according user, system");
	elektraSplitDel (split);

	split = elektraSplitNew();
	keySetString(ksLookupByName(ks, "system/valid/key2", 0), "value");
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 2, "size of keyset not correct");
	elektraSplitDel (split);


	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	elektraModulesClose(modules, 0);
	ksDel (modules);
	elektraTrieClose(handle->trie, 0);
	elektraFree(handle);
}

void test_us()
{
	printf ("Test user system splitting\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	Key *parentKey = keyNew(0);

	KeySet *ks = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		3,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		3,
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);

	Split *split = elektraSplitNew();
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "should need sync");
	succeed_if (elektraSplitDomains (split, ks, parentKey) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 2, "size of keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[0], split2) == 0, "system keyset not correct");
	elektraSplitDel (split);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	elektraFree(handle);
}

void test_optimize()
{
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);
	handle->trie = elektraTrieOpen(set_us(), modules, 0);

	KeySet *ks = ksNew ( 5,
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew ( 3,
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew ( 3,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	Split *split = elektraSplitNew();
	Key *key;


	ksRewind (ks);
	while ((key = ksNext(ks)) != 0)
	{
		if (keyIsUser(key) == 1) keyClearSync(key);
	}

	elektraSplitSync (split, handle, ks);


	printf ("Test optimization split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "system part not optimized");
	succeed_if (split->syncbits[1] == 0, "user part need to by synced");
	succeed_if (compare_keyset (split->keysets[0], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2) == 0, "system keyset not correct");

	elektraSplitDel (split);


	split = elektraSplitNew();
	clear_sync (ks);
	elektraSplitSync (split, handle, ks);

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 0, "system part not optimized");
	succeed_if (split->syncbits[1] == 0, "user part not optimized");
	succeed_if (compare_keyset (split->keysets[0], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2) == 0, "system keyset not correct");

	elektraSplitDel (split);



	split = elektraSplitNew();
	elektraSplitSync (split, handle, ks);

	ksRewind (ks);
	while ((key = ksNext(ks)) != 0)
	{
		key->flags = KEY_FLAG_SYNC;
	}

	elektraSplitDel (split);


	split = elektraSplitNew();
	elektraSplitSync (split, handle, ks);

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "optimized too much");
	succeed_if (split->syncbits[1] == 1, "optimized too much");
	succeed_if (compare_keyset (split->keysets[0], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2) == 0, "system keyset not correct");

	elektraSplitDel (split);


	ksDel (ks);
	ksDel (split1);
	ksDel (split2);

	elektraModulesClose(modules, 0);
	ksDel (modules);
	elektraTrieClose(handle->trie, 0);
	elektraFree(handle);
}

void test_easyparent()
{
	printf ("Test parent separation of user and system\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *ks = ksNew (
		8,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew ( 5,
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew ( 5,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	Key *parentKey;
	Split *split;


	parentKey = keyNew ("user", KEY_END);
	split = elektraSplitNew();
	elektraSplitSync (split, handle, ks);
	elektraSplitDomains (split, ks, parentKey);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to be synced");
	succeed_if (compare_keyset (split->keysets[0], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2) == 0, "system keyset not correct");

	elektraSplitDel (split);
	keyDel (parentKey);

	parentKey = keyNew ("system", KEY_END);
	split = elektraSplitNew();
	elektraSplitSync (split, handle, ks);
	elektraSplitDomains (split, ks, parentKey);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to be synced");
	succeed_if (compare_keyset (split->keysets[0], split1) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2) == 0, "system keyset not correct");

	elektraSplitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	elektraFree(handle);
}


KeySet *set_three()
{
	return ksNew(50,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/system", KEY_END),
		keyNew("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END),
		keyNew("system/elektra/mountpoints/userin", KEY_END),
		keyNew("system/elektra/mountpoints/userin/mountpoint", KEY_VALUE, "user/invalid", KEY_END),
		keyNew("system/elektra/mountpoints/userva", KEY_END),
		keyNew("system/elektra/mountpoints/userva/mountpoint", KEY_VALUE, "user/valid", KEY_END),
		KS_END);
}

void test_three()
{
	printf ("Test three mountpoints\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);
	handle->trie = elektraTrieOpen(set_three(), modules, 0);

	KeySet *ks = ksNew (
		18,
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		keyNew ("user/invalid", KEY_END),
		keyNew ("user/invalid/key1", KEY_END),
		keyNew ("user/invalid/key2", KEY_END),
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split0 = ksNew (
		9,
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		9,
		keyNew ("user/invalid", KEY_END),
		keyNew ("user/invalid/key1", KEY_END),
		keyNew ("user/invalid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		9,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);


	Split *split = elektraSplitNew();
	succeed_if (elektraSplitSync (split, handle, ks) == 1, "should need sync");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 1, "system part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user part need to by synced");
	succeed_if (split->syncbits[2] == 1, "user part need to by synced");
	succeed_if (split->size == 3, "not splitted according three");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "size of keyset not correct");
	succeed_if (ksGetSize(split->keysets[2]) == 3, "size of keyset not correct");
	succeed_if (compare_keyset (split->keysets[0], split0) == 0, "system keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split1) == 0, "userin keyset not correct");
	succeed_if (compare_keyset (split->keysets[2], split2) == 0, "userva keyset not correct");
	elektraSplitDel (split);




	ksDel (ks);
	ksDel (split0);
	ksDel (split1);
	ksDel (split2);
	elektraModulesClose(modules, 0);
	ksDel (modules);
	elektraTrieClose(handle->trie, 0);
	elektraFree(handle);
}

void test_remove()
{
	printf ("Test removing\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	handle->defaultBackend->size = 2;
	/* So we had 2 keys before in the keyset */

	KeySet *ks = ksNew ( 3,
		keyNew ("user/valid/key", KEY_END),
		KS_END);
	Split *split = elektraSplitNew();

	succeed_if (elektraSplitSync (split, handle, ks) == 1, "should need sync");
	succeed_if (elektraSplitRemove (split, handle, ks) == 1, "should see remove");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 1, "everything is in one keyset");
	succeed_if (ksGetSize(split->keysets[0]) == 1, "wrong size");
	compare_keyset (split->keysets[0], ks);

	elektraSplitDel (split);

	/* But it should even need sync when we dont have any unsynced keys! */
	clear_sync(ks);
	split = elektraSplitNew();
	succeed_if (elektraSplitSync (split, handle, ks) == 0, "no unsync key");
	succeed_if (elektraSplitRemove (split, handle, ks) == 1, "but we should see remove");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->size == 1, "everything is in one keyset");
	succeed_if (ksGetSize(split->keysets[0]) == 1, "wrong size");
	compare_keyset (split->keysets[0], ks);

	elektraSplitDel (split);



	ksDel (ks);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
}


int main(int argc, char** argv)
{
	printf("SPLIT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_create();
	test_resize();
	test_append();
	test_emptysplit();
	test_needsSync();
	test_easysplit();
	test_singlesplit();
	test_mount();
	test_us();
	test_optimize();
	test_easyparent();
	test_three();
	test_remove();

	printf("\ntest_split RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

