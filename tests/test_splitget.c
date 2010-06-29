/*************************************************************************** 
 *           test_splitset.c  - Test suite for splitted keyset data structure
 *                  -------------------
 *  begin                : Tue Jun 29 2010
 *  copyright            : (C) 2010 by Markus Raab
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


void test_basic()
{
	printf ("Test basic buildup\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	/* So we had 2 keys before in the keyset */

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			KS_END);

	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");

	succeed_if (split->size == 1, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "wrong size");
	succeed_if (compare_keyset(split->keysets[0], ks) == 0, "keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
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

	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	succeed_if (compare_keyset(split->keysets[1], ks) == 0, "keyset not correct");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");


	elektraSplitDel (split);
	keyDel (parentKey);

	ksDel (ks);
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
	handle->trie = elektraTrieOpen(simple_config(), modules, 0);

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			keyNew("user/tests/simple/testkey/b1/b2/down", KEY_END),
			keyNew("user/tests/simple/testkey/b1/b2/up", KEY_END),
			KS_END);

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

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys");
	succeed_if (split->size == 2, "not correct size after appointing");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	succeed_if (compare_key (split->parents[0], mp) == 0, "parentKey not correct");
	succeed_if (split->handles[0] == backend, "should be user backend");
	succeed_if (split->handles[1] == 0, "should be default backend");

	// output_split(split);

	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	// output_trie(trie);

	ksDel (ks);
	elektraTrieClose(handle->trie, 0);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
	ksDel (modules);
}


void test_get()
{
	printf ("Test basic get\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	/* So we had 2 keys before in the keyset */

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			KS_END);

	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	succeed_if (elektraSplitGet (split, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync(split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[2]) == 0, "key should not need sync");
	succeed_if (compare_keyset(split->keysets[0], ks) == 0, "keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");
	succeed_if (elektraSplitGet (split, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	succeed_if (compare_keyset(split->keysets[1], ks) == 0, "keyset not correct");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");


	elektraSplitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
}

void test_limit()
{
	printf ("Test limit\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	/* So we had 2 keys before in the keyset */

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			KS_END);

	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[0], keyNew("system/wrong", KEY_END));
	succeed_if (elektraSplitGet (split, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync(split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[2]) == 0, "key should not need sync");
	succeed_if (compare_keyset(split->keysets[0], ks) == 0, "keyset not correct");
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[1] = 1; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[1], keyNew("user/wrong", KEY_END));
	succeed_if (elektraSplitGet (split, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 4, "default should stay untouched");
	succeed_if (keyNeedSync(split->keysets[1]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[1]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[1]->array[2]) == 0, "key should not need sync");
	succeed_if (compare_key (split->parents[0], parentKey) == 0, "parentKey not correct");
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	ksDel (ks);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
}



int main(int argc, char** argv)
{
	printf("SPLIT GET   TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_basic();
	test_triesimple();
	test_get();
	test_limit();


	printf("\ntest_splitget RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

