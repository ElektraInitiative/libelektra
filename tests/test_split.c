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



KeySet *modules_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/modules", KEY_END),
		KS_END);
}


KeySet *simple_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/root", KEY_END),
		keyNew("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "", KEY_END),
		keyNew("system/elektra/mountpoints/simple", KEY_END),
		keyNew("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END),
		KS_END);
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


KeySet *root_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/root", KEY_END),
		keyNew("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		KS_END);
}



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
		elektraSplitAppend(split, 0, 0, 0);
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
		elektraSplitAppend(split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	for (size_t i=APPROXIMATE_NR_OF_BACKENDS+1; i<= APPROXIMATE_NR_OF_BACKENDS*2; ++i)
	{
		elektraSplitAppend(split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS*2, "should realloc");
	}

	elektraSplitDel (split);
}

void test_searchroot()
{
	printf ("Test search root\n");

	Split * split = elektraSplitNew();
	/* This here is in the trie */
	elektraSplitAppend(split, 0, keyNew("user/bla/bla", KEY_END), 0);
	elektraSplitAppend(split, 0, keyNew("user/bla/bla/something", KEY_END), 0);
	elektraSplitAppend(split, 0, keyNew("user/bla/bla/deep/below", KEY_END), 0);

	Key *searchKey = keyNew("user/bla/bla/deep/below", KEY_END);
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName(searchKey, "user/bla/bla/something");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName(searchKey, "user/bla/bla");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName(searchKey, "user/bla/bla/somewhere");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName(searchKey, "user/bla/bla/somewhere/else");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName(searchKey, "user/bla");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");
	keySetName(searchKey, "user/somewhere/else");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");
	keySetName(searchKey, "system");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root (mmh, cant be)");
	keySetName(searchKey, "user/bla/somewhere");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");
	keySetName(searchKey, "user/bla/somewhere/else");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");

	keyDel (searchKey);
	elektraSplitDel (split);
}


void test_basic()
{
	printf ("Test basic buildup\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	/* So we had 2 keys before in the keyset */


	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");

	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "system backend should be added");

	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	elektraFree(handle->defaultBackend);
	elektraFree(handle);
}


void test_triesimple()
{
	printf ("Test simple trie\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *modules = modules_config();
	Backend *backend;

	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	elektraMountOpen(handle, simple_config(), modules, 0);

	Split *split;
	Key *parentKey;
	Key *mp;

	split = elektraSplitNew();
	parentKey = keyNew("user/tests/simple/below", KEY_END);
	mp = keyNew("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], mp) == 0, "parentKey not correct");
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be user backend");
	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	// output_trie(trie);

	elektraTrieClose(handle->trie, 0);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
	ksDel (modules);
}



void test_trie()
{
	printf ("Test basic trie\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	Key *parentKey;
	Key *mp;
	Split *split;
	Backend *backend;
	KeySet *modules = modules_config();

	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	handle->defaultBackend->usersize = 2;
	handle->defaultBackend->systemsize = 2;
	/* So we had 2 keys before in the keyset */
	elektraMountOpen(handle, set_us(), modules, 0);


	split = elektraSplitNew();
	parentKey = keyNew("user", KEY_VALUE, "user", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be user backend");
	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "system", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for system");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be system backend");
	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system/below", KEY_VALUE, "system", KEY_END);
	mp = keyNew("system", KEY_VALUE, "system", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for system");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], mp) == 0, "parentKey not correct");
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be system backend");
	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);



	split = elektraSplitNew();
	parentKey = keyNew("system/deep/below", KEY_VALUE, "system", KEY_END);
	mp = keyNew("system", KEY_VALUE, "system", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for system");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], mp) == 0, "parentKey not correct");
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be system backend");
	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);



	ksDel (modules);
	elektraTrieClose(handle->trie, 0);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
}

void test_rootbackend()
{
	printf ("Test buildup with root backend\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *modules = modules_config();
	Backend *backend;

	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	elektraMountOpen(handle, root_config(), modules, 0);

	printf ("----------\n");
	output_trie (handle->trie);

	Split *split;
	Key *parentKey;
	Key *mp;

	split = elektraSplitNew();
	parentKey = keyNew("user/tests/simple/below", KEY_END);
	mp = keyNew("user", KEY_VALUE, "root", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], mp) == 0, "parentKey not correct");
	output_key(split->parents[0]);
	output_backend (split->handles[0]);
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be root backend");
	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);


	split = elektraSplitNew();
	parentKey = keyNew("system/tests/simple/below", KEY_END);
	mp = keyNew("system", KEY_VALUE, "root", KEY_END);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for system");
	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], mp) == 0, "parentKey not correct");
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be root backend");
	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	elektraTrieClose(handle->trie, 0);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
	ksDel (modules);
}



void test_invalidname()
{
	printf ("Test buildup with invalid parent name\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	/* So we had 2 keys before in the keyset */
	KeySet *ks = ksNew (0);


	Split *split = elektraSplitNew();
	Key *parentKey = keyNew(0);
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backends for user");
	keyDel (parentKey);

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");
	parentKey = keyNew ("user", KEY_VALUE, "default", KEY_END);
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	keyDel (parentKey);
	parentKey = keyNew ("system", KEY_VALUE, "default", KEY_END);
	succeed_if (compare_key (split->parents[1], parentKey) == 0, "parentKey not correct");
	keyDel (parentKey);
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->handles[1] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as default");
	succeed_if (split->syncbits[1] == 2, "should be marked as default");

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
	test_searchroot();
	test_basic();
	test_triesimple();
	test_trie();
	// test_rootbackend();
	test_invalidname();

	printf("\ntest_split RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

