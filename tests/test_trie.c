/*************************************************************************** 
 *           test_trie.c  - Test suite for trie data structure
 *                  -------------------
 *  begin                : Thu Oct 24 2007
 *  copyright            : (C) 2007 by Patrick Sabin
 *  email                : patricksabin@gmx.at
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

/*Needs private declarations*/
#include <kdbprivate.h>



KeySet *modules_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/modules", KEY_END),
		KS_END);
}

KeySet *minimal_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/mountpoints", KEY_END),
		KS_END);
}


void test_minimaltrie()
{
	printf ("Test minimal trie\n");

	Key *errorKey = keyNew(0);
	KeySet *modules = modules_config();
	Trie *trie = elektraTrieOpen(minimal_config(), modules, errorKey);

	output_warnings (errorKey);
	output_errors (errorKey);

	succeed_if (!trie, "minimal trie is null");

	elektraTrieClose(trie, 0);
	keyDel (errorKey);
	ksDel (modules);
}
KeySet *simple_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/simple", KEY_END),
		keyNew("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END),
		KS_END);
}

void test_simple()
{
	printf ("Test simple trie\n");

	Key *errorKey = keyNew(0);
	KeySet *modules = modules_config();
	Trie *trie = elektraTrieOpen(simple_config(), modules, errorKey);

	output_warnings (errorKey);
	output_errors (errorKey);

	exit_if_fail (trie, "trie was not build up successfully");

	Key *searchKey = keyNew("user");
	Backend *backend = elektraTrieLookup(trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key *mp = keyNew("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	keySetName(searchKey, "user/tests/simple");
	backend = elektraTrieLookup(trie, searchKey);
	succeed_if (backend, "there should be a backend");
	succeed_if (compare_key(backend->mountpoint, mp) == 0, "mountpoint key not correct");


	keySetName(searchKey, "user/tests/simple/below");
	Backend *b2 = elektraTrieLookup(trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	succeed_if (compare_key(b2->mountpoint, mp) == 0, "mountpoint key not correct");


	keySetName(searchKey, "user/tests/simple/deep/below");
	b2 = elektraTrieLookup(trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	succeed_if (compare_key(b2->mountpoint, mp) == 0, "mountpoint key not correct");

	// output_trie(trie);

	elektraTrieClose(trie, 0);
	keyDel (errorKey);
	ksDel (modules);
	keyDel (mp);
	keyDel (searchKey);
}

KeySet *iterate_config(void)
{
	return ksNew(5,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/hosts", KEY_END),
		keyNew("system/elektra/mountpoints/hosts/mountpoint", KEY_VALUE, "user/tests/hosts", KEY_END),
		keyNew("system/elektra/mountpoints/below", KEY_END),
		keyNew("system/elektra/mountpoints/below/mountpoint", KEY_VALUE, "user/tests/hosts/below", KEY_END),
		KS_END);
}

void collect_mountpoints(Trie *trie, KeySet *mountpoints)
{
	int i;
	for (i=0; i <= MAX_UCHAR; ++i)
	{
		if (trie->value[i]) ksAppendKey(mountpoints, ((Backend*) trie->value[i])->mountpoint);
		if (trie->children[i]) collect_mountpoints(trie->children[i], mountpoints);
	}
}

void test_iterate()
{
	printf ("Test iterate trie\n");

	Key *errorKey = keyNew(0);
	KeySet *modules = modules_config();
	Trie *trie = elektraTrieOpen(iterate_config(), modules, errorKey);

	output_warnings (errorKey);
	output_errors (errorKey);

	exit_if_fail (trie, "trie was not build up successfully");

	Key *searchKey = keyNew("user");
	Backend *backend = elektraTrieLookup(trie, searchKey);
	succeed_if (!backend, "there should be no backend");


	Key *mp = keyNew("user/tests/hosts", KEY_VALUE, "hosts", KEY_END);
	keySetName(searchKey, "user/tests/hosts");
	backend = elektraTrieLookup(trie, searchKey);
	succeed_if (backend, "there should be a backend");
	succeed_if (compare_key(backend->mountpoint, mp) == 0, "mountpoint key not correct");
	// printf ("backend: %p\n", (void*)backend);


	keySetName(searchKey, "user/tests/hosts/other/below");
	Backend *b2 = elektraTrieLookup(trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	succeed_if (compare_key(b2->mountpoint, mp) == 0, "mountpoint key not correct");
	// printf ("b2: %p\n", (void*)b2);


	keySetName(searchKey, "user/tests/hosts/other/deep/below");
	b2 = elektraTrieLookup(trie, searchKey);
	succeed_if (b2, "there should be a backend");
	succeed_if (backend == b2, "should be same backend");
	succeed_if (compare_key(b2->mountpoint, mp) == 0, "mountpoint key not correct");


	Key *mp2 = keyNew("user/tests/hosts/below", KEY_VALUE, "below", KEY_END);
	keySetName(searchKey, "user/tests/hosts/below");
	Backend *b3 = elektraTrieLookup(trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend != b3, "should be different backend");
	succeed_if (compare_key(b3->mountpoint, mp2) == 0, "mountpoint key not correct");
	backend = b3;
	// printf ("b3: %p\n", (void*)b3);


	keySetName(searchKey, "user/tests/hosts/below/other/deep/below");
	b2 = elektraTrieLookup(trie, searchKey);
	succeed_if (b3, "there should be a backend");
	succeed_if (backend == b3, "should be same backend");
	succeed_if (compare_key(b3->mountpoint, mp2) == 0, "mountpoint key not correct");

	// output_trie(trie);

	KeySet *mps = ksNew(0);
	collect_mountpoints(trie, mps);
	succeed_if (ksGetSize (mps) == 2, "not both mountpoints collected");
	succeed_if (compare_key(ksHead(mps), mp) == 0, "not correct mountpoint found");
	succeed_if (compare_key(ksTail(mps), mp2) == 0, "not correct mountpoint found");
	ksDel (mps);

	elektraTrieClose(trie, 0);
	keyDel (errorKey);
	ksDel (modules);
	keyDel (mp);
	keyDel (mp2);
	keyDel (searchKey);
}

int main(int argc, char** argv)
{
	printf("TRIE       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_minimaltrie();
	test_simple();
	test_iterate();

	printf("\ntest_trie RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

