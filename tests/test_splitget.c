/*************************************************************************** 
 *           test_splitset.c  - Test suite for split keyset data structure
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

#include <tests_internal.h>

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
		keyNew("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		keyNew("system/elektra/mountpoints/simple", KEY_END),
		keyNew("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END),
		KS_END);
}


KeySet *set_realworld()
{
	return ksNew(50,
		keyNew("system/elektra/mountpoints", KEY_END),
		keyNew("system/elektra/mountpoints/root", KEY_END),
		keyNew("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END),
		keyNew("system/elektra/mountpoints/users", KEY_END),
		keyNew("system/elektra/mountpoints/users/mountpoint", KEY_VALUE, "system/users", KEY_END),
		keyNew("system/elektra/mountpoints/groups", KEY_END),
		keyNew("system/elektra/mountpoints/groups/mountpoint", KEY_VALUE, "system/groups", KEY_END),
		keyNew("system/elektra/mountpoints/hosts", KEY_END),
		keyNew("system/elektra/mountpoints/hosts/mountpoint", KEY_VALUE, "system/hosts", KEY_END),
		keyNew("system/elektra/mountpoints/kde", KEY_END),
		keyNew("system/elektra/mountpoints/kde/mountpoint", KEY_VALUE, "user/sw/kde/default", KEY_END),
		keyNew("system/elektra/mountpoints/app1", KEY_END),
		keyNew("system/elektra/mountpoints/app1/mountpoint", KEY_VALUE, "user/sw/apps/app1/default", KEY_END),
		keyNew("system/elektra/mountpoints/app2", KEY_END),
		keyNew("system/elektra/mountpoints/app2/mountpoint", KEY_VALUE, "user/sw/apps/app2", KEY_END),
		KS_END);
}



void test_simple()
{
	printf ("Test simple trie\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	KeySet *modules = modules_config();
	Backend *backend;

	handle->split = elektraSplitNew();
	handle->defaultBackend = elektraCalloc(sizeof(struct _Backend));
	elektraMountOpen(handle, simple_config(), modules, 0);

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
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");

	succeed_if (split->size == 1, "user root + simple");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");

	mp = keyNew("user/tests/simple", KEY_VALUE, "simple", KEY_END);
	compare_key(split->parents[0], mp);
	succeed_if (split->handles[0] != handle->defaultBackend, "should be not the default backend");
	keyDel (mp);

	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be user backend");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys");
	succeed_if (split->size == 2, "not correct size after appointing");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	succeed_if (split->handles[0] == backend, "should be user backend");
	succeed_if (split->handles[1] == 0, "should be default backend");

	elektraSplitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	elektraSplitDel (handle->split);
	elektraTrieClose(handle->trie, 0);
	elektraFree(handle->defaultBackend);
	elektraFree(handle);
	ksDel (modules);
}


void test_get()
{
	printf ("Test basic get\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	KeySet *modules = modules_config();
	/* So we had 2 keys before in the keyset */

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			KS_END);

	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraMountDefault (handle, modules, 0) == 0, "could not mount default backends");
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync(split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset(split->keysets[0], ks);
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");
	compare_key(split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as sync");

	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");
	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "keys with other domain should go to default keyset");
	compare_keyset(split->keysets[1], ks);
	compare_key(split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");


	elektraSplitDel (split);
	keyDel (parentKey);

	ksDel (ks);
	kdbClose(handle, 0);
	ksDel (modules);
}

void test_limit()
{
	printf ("Test limit\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	/* So we had 2 keys before in the keyset */
	KeySet *modules = modules_config();

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			KS_END);

	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraMountDefault (handle, modules, 0) == 0, "could not mount default backends");
	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[0], keyNew("system/wrong", KEY_END));
	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync(split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset(split->keysets[0], ks);
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");

	// we know that system/wrong will produce a warning
	compare_key_name(split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[1] = 1; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[1], keyNew("user/wrong", KEY_END));
	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 4, "default should stay untouched");
	succeed_if (keyNeedSync(split->keysets[1]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[1]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[1]->array[2]) == 0, "key should not need sync");
	compare_key(split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	ksDel (ks);
	kdbClose(handle, 0);
	ksDel (modules);
}


void test_nobackend()
{
	printf ("Test keys without backends in split\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	KeySet *modules = modules_config();
	Backend *backend;

	elektraMountOpen(handle, simple_config(), modules, 0);

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
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys");
	split->syncbits[0] = 1; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[0], keyNew("system/wrong", KEY_END));

	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	// there will be a warning
	// succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (split->size == 2, "not correct size after appointing");
	succeed_if (ksGetSize(split->keysets[0]) == 2, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	compare_key(split->parents[0], mp);
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (split->handles[0] == backend, "should be user backend");
	succeed_if (split->handles[1] == 0, "should be default backend");


	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	// output_trie(trie);

	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}


void test_sizes()
{
	printf ("Test sizes\n");
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	KeySet *modules = modules_config();

	KeySet *ks = ksNew(15,
			keyNew("user/testkey1/below/here", KEY_END),
			keyNew("user/testkey/below1/here", KEY_END),
			keyNew("user/testkey/below2/here", KEY_END),
			KS_END);


	succeed_if (elektraMountDefault (handle, modules, 0) == 0, "could not mount default backends");
	succeed_if (handle->defaultBackend->usersize == 0, "usersize not initialized correct");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");

	Split *split = elektraSplitNew();
	Key *parentKey = keyNew("user", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[0], keyNew("system/wrong", KEY_END));
	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	// there will be a warning
	// succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (handle->defaultBackend->usersize == 3, "usersize not updated by elektraSplitGet");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");
	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 3, "wrong size");
	succeed_if (keyNeedSync(split->keysets[0]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[0]->array[2]) == 0, "key should not need sync");
	compare_keyset(split->keysets[0], ks);
	succeed_if (ksGetSize(split->keysets[1]) == 0, "wrong size");
	compare_key_name(split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 3, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	split = elektraSplitNew();
	parentKey = keyNew("system", KEY_VALUE, "default", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "system backend should be added");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint(split, handle, ks) == 1, "could not appoint keys to split");
	split->syncbits[1] = 1; /* Simulate a kdbGet() */
	ksAppendKey(split->keysets[1], keyNew("user/wrong", KEY_END));
	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (handle->defaultBackend->usersize == 3, "usersize should not be updated");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");
	succeed_if (split->size == 2, "there is an empty keset");
	succeed_if (ksGetSize(split->keysets[0]) == 0, "wrong size");
	succeed_if (ksGetSize(split->keysets[1]) == 4, "default should stay untouched");
	succeed_if (keyNeedSync(split->keysets[1]->array[0]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[1]->array[1]) == 0, "key should not need sync");
	succeed_if (keyNeedSync(split->keysets[1]->array[2]) == 0, "key should not need sync");
	compare_key(split->parents[0], parentKey);
	succeed_if (split->parents[1] == 0, "parentKey for default not correct");
	succeed_if (split->handles[0] == handle->defaultBackend, "not correct backend");
	succeed_if (split->syncbits[0] == 2, "should be marked as root");

	elektraSplitDel (split);
	keyDel (parentKey);


	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}


void test_triesizes()
{
	printf ("Test sizes in backends with trie\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	KeySet *modules = modules_config();
	Backend *backend = 0;
	Backend *rootBackend = 0;

	elektraMountOpen(handle, simple_config(), modules, 0);
	succeed_if (elektraMountDefault (handle, modules, 0) == 0, "could not mount default backends");
	succeed_if (handle->defaultBackend->usersize == 0, "usersize not initialized correct");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");

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

	parentKey = keyNew("user", KEY_END);

	rootBackend = elektraTrieLookup(handle->trie, parentKey);
	keySetName (parentKey, "user/tests/simple/below");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (keySetName (parentKey, 0) == 0, "could not delete name of parentKey");
	succeed_if (backend->usersize == 0, "usersize not initialized correct in backend");
	succeed_if (backend->systemsize == 0, "systemsize not initialized correct in backend");

	mp = keyNew("user/tests/simple", KEY_VALUE, "simple", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	split->syncbits[1] = 3; /* Simulate a kdbGet() */
	split->syncbits[2] = 1; /* Simulate a kdbGet() */

	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (backend->usersize == 2, "usersize should be updated");
	succeed_if (backend->systemsize == 0, "systemsize should not change");

	succeed_if (rootBackend->usersize == 3, "usersize of rootBackend should be updated");
	succeed_if (rootBackend->systemsize == 0, "systemsize  of rootBackend should not change");

	succeed_if (handle->defaultBackend->usersize == 0, "usersize not initialized correct");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");

	succeed_if (split->size == 5, "not correct size after appointing");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	succeed_if (ksGetSize(split->keysets[2]) == 2, "wrong size");
	compare_key(split->parents[2], mp);
	succeed_if (split->handles[0] == rootBackend, "should be user backend");
	succeed_if (split->handles[1] == rootBackend, "should be root backend");
	succeed_if (split->handles[2] == backend, "should be root backend");


	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}


void test_merge()
{
	printf ("Test sizes in backends with trie\n");

	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	KeySet *modules = modules_config();
	Backend *backend = 0;
	Backend *rootBackend = 0;

	elektraMountOpen(handle, simple_config(), modules, 0);
	succeed_if (elektraMountDefault (handle, modules, 0) == 0, "could not mount default backends");
	succeed_if (handle->defaultBackend->usersize == 0, "usersize not initialized correct");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");

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

	parentKey = keyNew("user", KEY_END);

	rootBackend = elektraTrieLookup(handle->trie, parentKey);
	keySetName (parentKey, "user/tests/simple/below");
	backend = elektraTrieLookup(handle->trie, parentKey);
	succeed_if (keySetName (parentKey, 0) == 0, "could not delete name of parentKey");
	succeed_if (backend->usersize == 0, "usersize not initialized correct in backend");
	succeed_if (backend->systemsize == 0, "systemsize not initialized correct in backend");

	mp = keyNew("user/tests/simple", KEY_VALUE, "simple", KEY_END);

	succeed_if (elektraSplitBuildup (split, handle, parentKey) == 1, "we add the default backend for user");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "could not appoint keys");
	split->syncbits[0] = 3; /* Simulate a kdbGet() */
	split->syncbits[1] = 3; /* Simulate a kdbGet() */
	split->syncbits[2] = 1; /* Simulate a kdbGet() */

	succeed_if (elektraSplitGet (split, parentKey, handle) == 1, "could not postprocess get");
	succeed_if (output_error(parentKey), "error found");
	succeed_if (output_warnings(parentKey), "warning(s) found");

	succeed_if (backend->usersize == 2, "usersize should be updated");
	succeed_if (backend->systemsize == 0, "systemsize should not change");

	succeed_if (rootBackend->usersize == 3, "usersize of rootBackend should be updated");
	succeed_if (rootBackend->systemsize == 0, "systemsize  of rootBackend should not change");

	succeed_if (handle->defaultBackend->usersize == 0, "usersize not initialized correct");
	succeed_if (handle->defaultBackend->systemsize == 0, "systemsize not initialized correct");

	succeed_if (split->size == 5, "not correct size after appointing");
	succeed_if (ksGetSize(split->keysets[1]) == 3, "wrong size");
	succeed_if (ksGetSize(split->keysets[2]) == 2, "wrong size");
	compare_key(split->parents[2], mp);
	succeed_if (split->handles[0] == rootBackend, "should be user backend");
	succeed_if (split->handles[1] == rootBackend, "should be root backend");
	succeed_if (split->handles[2] == backend, "should be root backend");

	KeySet *nks = ksNew (0);
	succeed_if (elektraSplitMerge (split, nks) == 1, "could not merge together keysets");
	compare_keyset(ks, nks);
	// output_keyset (nks);
	ksDel (nks);


	elektraSplitDel (split);
	keyDel (parentKey);
	keyDel (mp);

	ksDel (ks);
	kdbClose (handle, 0);
	ksDel (modules);
}


void test_realworld()
{
	printf ("Test real world example\n");

	Key *parent = 0;
	KDB *handle = elektraCalloc(sizeof(struct _KDB));
	handle->split = elektraSplitNew();
	KeySet *modules = ksNew(0);
	elektraModulesInit(modules, 0);

	elektraMountOpen(handle, set_realworld(), modules, 0);
	succeed_if (elektraMountDefault (handle, modules, 0) == 0, "could not mount default backends");

	KeySet *ks = ksNew ( 18,
		keyNew ("system/elektra/mountpoints", KEY_END),
		keyNew ("system/elektra/mountpoints/new", KEY_END),
		keyNew ("system/elektra/mountpoints/new/mountpoint", KEY_VALUE, "something", KEY_END),
		keyNew ("system/users", KEY_END),
		keyNew ("system/users/markus", KEY_END),
		keyNew ("system/users/harald", KEY_END),
		keyNew ("system/users/n", KEY_END),
		keyNew ("system/users/albert", KEY_END),
		keyNew ("system/hosts", KEY_END),
		keyNew ("system/hosts/markusbyte", KEY_VALUE, "127.0.0.1", KEY_END),
		keyNew ("system/hosts/mobilebyte", KEY_END),
		keyNew ("system/hosts/n900", KEY_END),
		keyNew ("user/sw/apps/app1/default", KEY_END),
		keyNew ("user/sw/apps/app1/default/maximize", KEY_VALUE, "1", KEY_END),
		keyNew ("user/sw/apps/app1/default/download", KEY_VALUE, "0", KEY_END),
		keyNew ("user/sw/apps/app1/default/keys/a", KEY_VALUE, "a", KEY_END),
		keyNew ("user/sw/apps/app1/default/keys/b", KEY_VALUE, "b", KEY_END),
		keyNew ("user/sw/apps/app1/default/keys/c", KEY_VALUE, "c", KEY_END),
		keyNew ("user/outside", KEY_VALUE, "test", KEY_END),
		KS_END);
	KeySet *split0 = ksNew ( 9,
		keyNew ("user/sw/apps/app1/default", KEY_END),
		keyNew ("user/sw/apps/app1/default/maximize", KEY_VALUE, "1", KEY_END),
		keyNew ("user/sw/apps/app1/default/download", KEY_VALUE, "0", KEY_END),
		keyNew ("user/sw/apps/app1/default/keys/a", KEY_VALUE, "a", KEY_END),
		keyNew ("user/sw/apps/app1/default/keys/b", KEY_VALUE, "b", KEY_END),
		keyNew ("user/sw/apps/app1/default/keys/c", KEY_VALUE, "c", KEY_END),
		KS_END);
	KeySet *split3 = ksNew ( 9,
		keyNew ("system/hosts", KEY_END),
		keyNew ("system/hosts/markusbyte", KEY_VALUE, "127.0.0.1", KEY_END),
		keyNew ("system/hosts/mobilebyte", KEY_END),
		keyNew ("system/hosts/n900", KEY_END),
		KS_END);
	KeySet *split6 = ksNew ( 3, 
		keyNew ("user/outside", KEY_VALUE, "test", KEY_END),
		KS_END);
	KeySet *split7 = ksNew ( 9,
		keyNew ("system/users", KEY_END),
		keyNew ("system/users/markus", KEY_END),
		keyNew ("system/users/harald", KEY_END),
		keyNew ("system/users/n", KEY_END),
		keyNew ("system/users/albert", KEY_END),
		KS_END);
	KeySet *split8 = ksNew ( 9,
		keyNew ("system/elektra/mountpoints", KEY_END),
		keyNew ("system/elektra/mountpoints/new", KEY_END),
		keyNew ("system/elektra/mountpoints/new/mountpoint", KEY_VALUE, "something", KEY_END),
		KS_END);


	Split *split = elektraSplitNew();

	succeed_if (elektraSplitBuildup (split, handle, parent) == 1, "should need sync");
	succeed_if (output_error(parent), "error found");
	succeed_if (output_warnings(parent), "warning(s) found");

	succeed_if (split->size == 9, "size not correct");
	succeed_if (split->handles[5] == split->handles[6], "root backends have same handle");
	succeed_if (split->syncbits[5] == 2, "sync state for root not correct");
	succeed_if (split->syncbits[6] == 2, "sync state for root not correct");
	succeed_if (!strcmp(keyName(split->parents[0]), "user/sw/apps/app1/default"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[1]), "user/sw/apps/app2"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[2]), "system/groups"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[3]), "system/hosts"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[4]), "user/sw/kde/default"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[5]), "system"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[6]), "user"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[7]), "system/users"), "parent key not correct");
	succeed_if (!strcmp(keyName(split->parents[8]), "system/elektra"), "parent key not correct");

	succeed_if (elektraSplitAppoint (split, handle, ks) == 1, "should need sync");
	succeed_if (split->size == 10, "size not correct");
	succeed_if (split->syncbits[0] == 0, "sync state for root not correct");
	succeed_if (split->syncbits[1] == 0, "sync state for root not correct");
	succeed_if (split->syncbits[2] == 0, "sync state for root not correct");
	succeed_if (split->syncbits[3] == 0, "sync state for root not correct");
	succeed_if (split->syncbits[4] == 0, "sync state for root not correct");
	succeed_if (split->syncbits[5] == 2, "sync state for root not correct");
	succeed_if (split->syncbits[6] == 2, "sync state for root not correct");
	succeed_if (split->syncbits[7] == 0, "sync state for root not correct");
	succeed_if (split->syncbits[8] == 2, "sync state for root not correct");
	compare_keyset(split->keysets[0], split0);
	compare_keyset(split->keysets[3], split3);
	compare_keyset(split->keysets[6], split6);
	compare_keyset(split->keysets[7], split7);
	compare_keyset(split->keysets[8], split8);

	split->syncbits[0] |= 1;
	split->syncbits[3] |= 1;
	split->syncbits[6] |= 1;
	split->syncbits[7] |= 1;
	succeed_if (elektraSplitGet (split, parent, handle) == 1, "postprocessing failed");
	succeed_if (output_error(parent), "error found");
	succeed_if (output_warnings(parent), "warning(s) found");

	succeed_if (split->handles[0]->usersize == 6, "wrong size");
	succeed_if (split->handles[3]->systemsize == 4, "wrong size");
	succeed_if (split->handles[6]->usersize == 1, "wrong size");
	succeed_if (split->handles[7]->systemsize == 5, "wrong size");


	KeySet *dest = ksNew(5,
			keyNew("user/test", KEY_VALUE, "should be gone", KEY_END),
			KS_END);
	ksClear (dest);
	succeed_if (elektraSplitMerge (split, dest) == 1, "split merge");
	compare_keyset(dest, ks);
	ksDel (dest);

	elektraSplitDel (split);


	ksDel (ks);
	ksDel (split0);
	ksDel (split3);
	ksDel (split6);
	ksDel (split7);
	ksDel (split8);
	keyDel (parent);
	elektraModulesClose(modules, 0);
	ksDel (modules);

	kdbClose (handle, 0);
}



int main(int argc, char** argv)
{
	printf("SPLIT GET   TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_simple();
	test_get();
	test_limit();
	test_nobackend();
	test_sizes();
	test_triesizes();
	test_merge();
	test_realworld();


	printf("\ntest_splitget RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

