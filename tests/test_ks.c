/***************************************************************************
 *          test_ks.c  -  KeySet struct test suite
 *                  -------------------
 *  begin                : Thu Dez 12 2006
 *  copyright            : (C) 2006 by Markus Raab
 *  email                : sizon5@gmail.com
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <tests_internal.h>

void test_ksNew()
{
	KeySet *ks=0;
	KeySet * keys = ksNew (15,0);
	KeySet * config;

	printf("Test ks creation\n");
	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");

	succeed_if (ksAppendKey(ks,keyNew("user/a", KEY_END)) == 1, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/b", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/c", KEY_END)) == 3, "could not append a key");
	succeed_if(ksGetSize(ks) == 3, "size not correct after 3 keys");

	KeySet *ks2=ksNew (0);
	ksCopy (ks2, ks);
	compare_keyset (ks, ks2);

	succeed_if (ksAppendKey(ks,keyNew("user/d", KEY_END)) == 4, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/e", KEY_END)) == 5, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/f", KEY_END)) == 6, "could not append a key");
	succeed_if(ksGetSize(ks) == 6, "could not append 3 more keys");

	ksCopy (ks2, ks);
	compare_keyset (ks, ks2);

	ksClear (ks2); // useless, just test for double free
	ksCopy (ks2, ks);
	compare_keyset (ks, ks2);

	succeed_if(ksDel(ks) == 0, "could not delete keyset");


	succeed_if(ksGetSize(keys) == 0, "could not append 3 more keys");
	succeed_if(ksGetAlloc(keys) == 15, "allocation size wrong");
	succeed_if(ksDel(keys) == 0, "could not delete keyset");

	config = ksNew (100,
		keyNew ("user/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", 0),
		keyNew ("user/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", 0),
		keyNew ("user/sw/app/fixedConfiguration/key3", KEY_VALUE, "value3", 0),0);
	succeed_if(ksGetSize(config) == 3, "could not append 3 keys in keyNew");
	succeed_if(ksGetAlloc(config) == 100, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if(ksGetAlloc(config) == 49, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if(ksGetAlloc(config) == 24, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if(ksGetAlloc(config) == 15, "allocation size wrong");
	succeed_if(ksDel(config) == 0, "could not delete keyset");

	config = ksNew (10,
		keyNew ("user/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", 0),
		keyNew ("user/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", 0),
		keyNew ("user/sw/app/fixedConfiguration/key3", KEY_VALUE, "value1", 0),
		keyNew ("user/sw/app/fixedConfiguration/key4", KEY_VALUE, "value3", 0),0);

	succeed_if(ksGetSize(config) == 4, "could not append 5 keys in keyNew");
	succeed_if(ksGetAlloc(config) == 15, "allocation size wrong");
	ksAppendKey(config, keyNew ("user/sw/app/fixedConfiguration/key6", KEY_VALUE, "value4", 0));

	ksClear (ks2);
	ksCopy (ks2, config);
	compare_keyset (config, ks2);

	succeed_if(ksDel(config) == 0, "could not delete keyset");
	succeed_if(ksDel(ks2) == 0, "could not delete keyset");

	KeySet *ks_c = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	// TODO: why is the cursor here?
	succeed_if (!strcmp(keyName(ksCurrent(ks_c)), "system/valid/key2"),
			"cursor jumped somewhere else");
	ksDel (ks_c);
}

void test_ksEmpty()
{
	printf ("Test empty keysets\n");
	KeySet *ks;
	KeySet *ks2;
	Key *current;

	ks = ksNew(0);
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	succeed_if (ksPop (ks) == 0, "pop empty keyset");
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	ksDel (ks);

	ks = ksNew (1, current=keyNew("user/test", KEY_END), KS_END);
	succeed_if (ksGetSize (ks) == 1, "size not correct");
	succeed_if (ksPop (ks) == current, "pop empty keyset");
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	succeed_if (ksPop (ks) == 0, "pop empty keyset");
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	keyDel (current);
	ksDel (ks);

	ks = ksNew (0);
	ks2 = ksNew (0);
	succeed_if (ksAppend (ks, ks2) == 0, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksPop(ks) == 0, "could not pop empty keyset");
	succeed_if (ksPop(ks2) == 0, "could not pop empty keyset2");
	ksDel (ks);
	ksDel (ks2);

	ks = ksNew (1, current=keyNew("user/test", KEY_END), KS_END);
	ks2 = ksNew (0);
	succeed_if (ksGetSize (ks) == 1, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 1, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 1, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksPop(ks) == current, "could not pop keyset");
	succeed_if (ksPop(ks2) == 0, "could not pop empty keyset2");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 0, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	keyDel (current);
	ksDel (ks);
	ksDel (ks2);


	ks = ksNew (0);
	ks2 = ksNew (1, current=keyNew("user/test", KEY_END), KS_END);
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 1, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 1, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 1, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 1, "empty keyset does not have correct size");
	succeed_if (ksPop(ks) == current, "could not pop keyset");
	succeed_if (ksPop(ks2) == current, "could not pop empty keyset2");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 0, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	keyDel (current); // only one keyDel, because ksPop decrements counter
	ksDel (ks);
	ksDel (ks2);
}

#define NR_KEYSETS 10

void test_ksReference()
{
	KeySet *ks=0;
	KeySet *ks1;
	Key *k1, *k2;
	KeySet *kss[NR_KEYSETS];
	int i;

	printf("Test reference of key\n");

	ks=ksNew(0);
	k1 = keyNew("user/aname", KEY_END);
	succeed_if (keyGetRef(k1) == 0, "reference counter of new key");
	succeed_if (ksAppendKey(ks,k1) == 1, "size should be one");
	succeed_if (keyGetRef(k1) == 1, "reference counter of inserted key");
	succeed_if (ksGetSize(ks) == 1, "wrong size, should stay after inserting duplication");

	k2 = keyDup (k1);
	keySetString(k2, "newvalue");

	succeed_if (keyGetRef(k2) == 0, "reference counter not resetted");
	succeed_if (ksAppendKey(ks,k2) == 1, "size should stay at 1");
	// k1 should be freed by now and instead k2 in the keyset
	succeed_if (ksGetSize(ks) == 1, "wrong size, should stay after inserting duplication");

	ksRewind(ks);
	ksNext(ks);
	succeed_if (strcmp(keyValue(ksCurrent(ks)), "newvalue")==0, "the duplicated key should be in keyset");

	ksDel (ks);

	ks=ksNew(5,
		keyNew ("user/key", KEY_END),
		keyNew ("system/key", KEY_END),
		KS_END);

	k1=ksLookupByName(ks, "user/key", 0);
	k2=ksLookupByName(ks, "system/key", 0);
	succeed_if (keyGetRef(k1) == 1, "reference counter of new inserted key");
	succeed_if (keyGetRef(k2) == 1, "reference counter of new inserted key");

	ksDel (ks);

	ks=ksNew(5,
		keyNew ("user/key", KEY_END),
		keyNew ("system/key", KEY_END),
		KS_END);

	k1=ksLookupByName(ks, "user/key", 0);
	k2=ksLookupByName(ks, "system/key", 0);
	succeed_if (keyGetRef(k1) == 1, "reference counter of new inserted key");
	succeed_if (keyGetRef(k2) == 1, "reference counter of new inserted key");
	ks1=ksDup (ks);

	succeed_if (keyGetRef(k1) == 2, "reference counter after duplication of keyset");
	succeed_if (keyGetRef(k2) == 2, "reference counter after ksdup");
	k1=ksPop (ks);
	succeed_if (keyGetRef(k1) == 1, "reference counter after pop");
	keyDel (k1);
	succeed_if (keyGetRef(k1) == 1, "reference counter");
	succeed_if (keyGetRef(k2) == 2, "reference counter should not be influenced");

	ksDel (ks);
	succeed_if (keyGetRef(k1) == 1, "reference counter, delete from first keyset");
	succeed_if (keyGetRef(k2) == 1, "reference counter, delete from first keyset");
	ksDel (ks1); // k1 and k2 deleted

	ks1=ksNew(0);
	k1=keyNew("user/k1", KEY_END);
	succeed_if (keyGetRef(k1) == 0, "reference counter of new inserted key");
	succeed_if (ksAppendKey(ks1, k1) == 1, "appending did not work");
	succeed_if (ksGetSize(ks1) == 1, "size did not match");
	succeed_if (keyGetRef(k1) == 1, "reference counter of new inserted key");
	succeed_if (ksAppendKey(ks1, k1) == 1, "appending the very same key");
	succeed_if (ksGetSize(ks1) == 1, "size did not match");
	succeed_if (keyGetRef(k1) == 1, "reference counter of new inserted key should stay the same");

	k1=ksPop (ks1);
	succeed_if (keyGetRef(k1) == 0, "reference counter of new inserted key");
	succeed_if (keyDel (k1) == 0, "keyDel did not work");

	succeed_if (ksDel (ks1) == 0, "could not delete key");


	kss[0]=ksNew(5,
		k1=keyNew ("system/key", KEY_END),
		k2=keyNew ("user/key", KEY_END),
		KS_END);
	for (i=1; i< NR_KEYSETS; i++)
	{
		succeed_if (keyGetRef(k1) == i, "reference counter");
		succeed_if (keyGetRef(k2) == 1, "reference counter");
		kss[i] = ksDup (kss[i-1]);
		succeed_if (keyGetRef(k2) == 2, "reference counter");
		succeed_if (!strcmp(keyName(ksPop (kss[i-1])), "user/key"),
				"were not in order");
		succeed_if (keyGetRef(k2) == 1, "reference counter");
		succeed_if (keyDel(k2) == 1, "delete key");
		succeed_if (keyGetRef(k2) == 1, "reference counter");
		
	}
	succeed_if (keyGetRef(k1) == NR_KEYSETS, "reference counter");
	succeed_if (keyGetRef(k2) == 1, "reference counter");

	for (i=0; i< NR_KEYSETS; i++)
	{
		succeed_if (keyGetRef(k1) == NR_KEYSETS-i, "reference counter");
		ksDel (kss[i]);
	}
}

#define NAME_SIZE 250

void test_ksResize()
{
	int i;
	KeySet *ks=0;
	KeySet *copy = ksNew (0);
	char name[NAME_SIZE];

	ks = ksNew (20,
			keyNew("user/test01", KEY_END),
			keyNew("user/test02", KEY_END),
			keyNew("user/test03", KEY_END),
			keyNew("user/test04", KEY_END),
			keyNew("user/test05", KEY_END),
			keyNew("user/test11", KEY_END),
			keyNew("user/test12", KEY_END),
			keyNew("user/test13", KEY_END),
			keyNew("user/test14", KEY_END),
			keyNew("user/test15", KEY_END),
			keyNew("user/test21", KEY_END),
			keyNew("user/test22", KEY_END),
			keyNew("user/test23", KEY_END),
			keyNew("user/test24", KEY_END),
			keyNew("user/test25", KEY_END),
			keyNew("user/test31", KEY_END),
			keyNew("user/test32", KEY_END),
			keyNew("user/test33", KEY_END),
			keyNew("user/test34", KEY_END),
			keyNew("user/test35", KEY_END),
			KS_END);
	succeed_if (ksGetAlloc(ks) == 20, "20 keys with alloc 20 should work");
	ksDel (ks);

	printf("Test resize of keyset\n");
	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");
	for (i=0; i< 100; i++)
	{
		snprintf(name, NAME_SIZE, "user/test%d", i);
		ksAppendKey(ks, keyNew (name, KEY_END));
		if (i >= 63) { succeed_if(ksGetAlloc(ks) == 127, "allocation size wrong"); }
		else if (i >= 31) { succeed_if(ksGetAlloc(ks) == 63, "allocation size wrong"); }
		else if (i >= 15) { succeed_if(ksGetAlloc(ks) == 31, "allocation size wrong"); }
		else if (i >= 0) { succeed_if(ksGetAlloc(ks) == 15, "allocation size wrong"); }
	}
	succeed_if(ksGetSize(ks) == 100, "could not append 100 keys");
	succeed_if(ksGetAlloc(ks) == 127, "allocation size wrong");
	for (i=100; i >= 0; i--)
	{
		keyDel (ksPop(ks));
		if (i >= 64) { succeed_if(ksGetAlloc(ks) == 127, "allocation size wrong"); }
		else if (i >= 32) { succeed_if(ksGetAlloc(ks) == 63, "allocation size wrong"); }
		else if (i >= 16) { succeed_if(ksGetAlloc(ks) == 31, "allocation size wrong"); }
		else if (i >= 0) { succeed_if(ksGetAlloc(ks) == 15, "allocation size wrong"); }
	}
	succeed_if(ksGetSize(ks) == 0, "could not pop 100 keys");
	succeed_if(ksGetAlloc(ks) == 15, "allocation size wrong");
	ksDel (ks);
	
	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");
	ksResize (ks, 100);
	succeed_if(ksGetAlloc(ks) == 100, "allocation size wrong");
	for (i=0; i< 100; i++)
	{
		snprintf(name, NAME_SIZE, "user/test%d", i);
		ksAppendKey(ks, keyNew (name, KEY_END));
		succeed_if(ksGetAlloc(ks) == 100, "allocation size wrong");
	}
	succeed_if(ksGetSize(ks) == 100, "could not append 100 keys");
	succeed_if(ksGetAlloc(ks) == 100, "allocation size wrong");
	ksDel (ks);

	ks =
#include "data_keyset.c"

	succeed_if(ksGetSize(ks) == 102, "Problem loading keyset with 102 keys");
	succeed_if(ksGetAlloc(ks) == 102, "alloc size wrong");

	ksCopy (copy, ks);
	succeed_if(ksGetSize(copy) == 102, "Problem copy keyset with 102 keys");
	succeed_if(ksGetAlloc(copy) == 128, "alloc of copy size wrong");

	compare_keyset(copy, ks);

	ksClear (copy); // useless, just test for double free
	ksCopy (copy, ks);

	succeed_if(ksGetSize(copy) == 102, "Problem copy keyset with 102 keys");
	succeed_if(ksGetAlloc(copy) == 128, "alloc of copy size wrong");
	compare_keyset(copy, ks);

	ksDel (copy);
	ksDel (ks);
}

void test_ksDup()
{
	KeySet *ks=0;
	KeySet *other=0;

	printf("Test ks duplication\n");

	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");
	other=ksDup(ks);
	succeed_if(other, "other creation failed");
	succeed_if(ksGetSize(ks) == 0, "ks has keys");
	succeed_if(ksGetSize(other) == 0, "other has keys");
	ksDel (other);
	ksDel (ks);

	exit_if_fail((ks=ksNew(1, keyNew("user/anything", KEY_END), KS_END)) != 0, "could not create new keyset");
	other=ksDup(ks);
	succeed_if(other, "other creation failed");
	succeed_if(ksGetSize(ks) == 1, "ks has no keys");
	succeed_if(ksGetSize(other) == 1, "other has no keys");
	ksDel (other);
	ksDel (ks);

	exit_if_fail((ks=ksNew(1, keyNew("system/some", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test3", KEY_END)) == 4, "could not append a key");
	other=ksDup(ks);
	succeed_if(other, "other creation failed");
	succeed_if(ksGetSize(ks) == 4, "ks has no keys");
	succeed_if(ksGetSize(other) == 4, "other has no keys");
	ksDel (other);
	ksDel (ks);
	
	exit_if_fail((ks=ksNew(1, keyNew("user/any123", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test3", KEY_END)) == 4, "could not append a key");
	other=ksDup(ks);
	succeed_if(other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if(ksGetSize(ks) == 4, "ks has no keys");
	succeed_if(ksGetSize(other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);
	
	exit_if_fail((ks=ksNew(1, keyNew("system/test", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test3", KEY_END)) == 4, "could not append a key");
	other=ksDup(ks);
	succeed_if(other, "other creation failed");
	keyDel(ksPop (other));
	succeed_if (ksAppendKey(ks,keyNew("user/test4", KEY_END)) == 5, "could not append a key");
	succeed_if(ksGetSize(ks) == 5, "ks has no keys");
	succeed_if(ksGetSize(other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);
}

void test_ksCopy()
{
	KeySet *ks=0;
	KeySet *other=0;

	printf("Test ks copy\n");

	other = ksNew(0);
	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");
	succeed_if(ksCopy(other,ks)==1, "Copy failed");
	succeed_if(other, "other creation failed");
	succeed_if(ksGetSize(ks) == 0, "ks has keys");
	succeed_if(ksGetSize(other) == 0, "other has keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew(0);
	exit_if_fail((ks=ksNew(1, keyNew("user/test3", KEY_END), 0)) != 0, "could not create new keyset");
	succeed_if(ksCopy(other,ks)==1, "Copy failed");
	succeed_if(other, "other creation failed");
	succeed_if(ksGetSize(ks) == 1, "ks has no keys");
	succeed_if(ksGetSize(other) == 1, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew(0);
	exit_if_fail((ks=ksNew(1, keyNew("user/testro", KEY_END), 0)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test3", KEY_END)) == 4, "could not append a key");
	succeed_if(ksCopy(other,ks)==1, "Copy failed");
	succeed_if(other, "other creation failed");
	succeed_if(ksGetSize(ks) == 4, "ks has no keys");
	succeed_if(ksGetSize(other) == 4, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew(0);
	exit_if_fail((ks=ksNew(1, keyNew("system/test", KEY_END), 0)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test3", KEY_END)) == 4, "could not append a key");
	succeed_if(ksCopy(other,ks)==1, "Copy failed");
	succeed_if(other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if(ksGetSize(ks) == 4, "ks has no keys");
	succeed_if(ksGetSize(other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew(0);
	exit_if_fail((ks=ksNew(1, keyNew("user/mykeys", KEY_END), 0)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/test3", KEY_END)) == 4, "could not append a key");
	succeed_if(ksCopy(other,ks)==1, "Copy failed");
	succeed_if(other, "other creation failed");
	keyDel(ksPop (other));
	succeed_if (ksAppendKey(ks,keyNew("user/test", KEY_END)) == 5, "could not append a key");
	succeed_if(ksGetSize(ks) == 5, "ks has no keys");
	succeed_if(ksGetSize(other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew(0);
	exit_if_fail((ks=ksNew(1, keyNew("user/a/b/c", KEY_END), 0)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,keyNew("user/a/test", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/a/b/test", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey(ks,keyNew("user/a/b/ctest", KEY_END)) == 4, "could not append a key");
	succeed_if(ksCopy(other,ks)==1, "Copy failed");
	succeed_if(other, "other creation failed");
	keyDel(ksPop (other));
	succeed_if (ksAppendKey(ks,keyNew("user/test", KEY_END)) == 5, "could not append a key");
	succeed_if(ksGetSize(ks) == 5, "ks has no keys");
	succeed_if(ksGetSize(other) == 3, "other has no keys");

	succeed_if(ksCopy(ks,0)==0, "Clear failed");
	succeed_if(ksGetSize(ks) == 0, "ks has keys");

	succeed_if(ksCopy(other,0)==0, "Clear failed");
	succeed_if(ksGetSize(other) == 0, "other has keys");
	ksDel (other);
	ksDel (ks);



	ks = ksNew(0);
	ksAppendKey(ks, keyNew ("user/abc", KEY_META, "def", "egh", KEY_END));

	other = ksNew(0);
	ksCopy (other, ks);
	compare_keyset(ks, other);

	ksDel (other);
	ksDel (ks);
}

void test_ksIterate()
{
	KeySet *ks=ksNew(0);
	KeySet *other=ksNew(0);
	Key * key;
	int i;
	char name [] = "user/n";
	char output [] = "2 key not on its place";

	printf("Test keyset iterate\n");
	ksAppendKey(ks,keyNew("user/1", KEY_END));
	ksAppendKey(ks,keyNew("user/2", KEY_END));
	ksAppendKey(ks,keyNew("user/3", KEY_END));
	ksAppendKey(ks,keyNew("user/4", KEY_END));
	ksAppendKey(ks,keyNew("user/5", KEY_END));
	succeed_if(ksGetSize(ks) == 5, "could not append 5 keys");

	succeed_if (ksRewind(ks) == 0, "Could not rewind keyset");
	succeed_if (ksRewind(ks) == 0, "Could not rewind keyset twice");

	succeed_if (ksNext(ks) != 0, "Could not get first key");
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/1"), "This is not the first key");

	succeed_if (ksNext(ks) != 0, "Could not get second key");
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/2"), "This is not the second key");

	succeed_if (ksNext(ks) != 0, "Could not get third key");
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/3"), "This is not the third key");

	succeed_if (ksNext(ks) != 0, "Could not get fourth key");
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/4"), "This is not the fourth key");

	succeed_if (ksNext(ks) != 0, "Could not get fifth key");
	succeed_if (!strcmp(keyName(ksCurrent(ks)), "user/5"), "This is not the fifth key");

	succeed_if (ksNext(ks) == 0, "Could not iterate over last");
	succeed_if (ksCurrent(ks) == 0, "This is not the beyond last key");

	succeed_if (ksNext(ks) == 0, "Could not iterate over last (again)");
	succeed_if (ksCurrent(ks) == 0, "This is not the beyond last key (again)");

	key = ksPop(ks);
	succeed_if(strcmp(keyName(key), "user/5") == 0, "1 key not on first place");
	succeed_if (keyDel (key) == 0, "could not del popped key");

	succeed_if(ksAppend(other,ks) == 4, "could not append keys");

	for (i=4; i>=1; i--)
	{
		key = ksPop(other);
		succeed_if (key != 0, "got null pointer key");
		name[5] = '0' + i;
		output[0] = '0' + i;
		succeed_if(strcmp(keyName(key), name) == 0, output);
		keyDel (key);
	}
	
	succeed_if (ksAppendKey(other, keyNew ("user/3", KEY_END)) == 1, "could not append one key");
	key = ksPop(other);
	succeed_if (key != 0, "got null pointer key");
	succeed_if(strcmp(keyName(key), "user/3") == 0, "only key to pop not found");
	succeed_if (keyDel (key) == 0, "could not del popped key");
	ksDel(other);
	ksDel(ks);

	ks = ksNew(10,
		keyNew("user/0", KEY_END),
		keyNew("user/1", KEY_END),
		keyNew("user/2", KEY_END),
		keyNew("user/3", KEY_END),
		0);
	
	other = ksNew(10,
		keyNew("user/4", KEY_END),
		keyNew("user/5", KEY_END),
		keyNew("user/6", KEY_END),
		keyNew("user/7", KEY_END),
		0);

	succeed_if(ksAppend(ks,other) == 8, "could not append keys");

	for (i=7; i >= 0; i--)
	{
		key = ksPop(ks);
		succeed_if (key != 0, "got null pointer key");
		name[5] = '0' + i;
		output[0] = '0' + i;
		succeed_if(strcmp(keyName(key), name) == 0, output);
		keyDel (key);
	}
	ksDel (ks);
	ksDel (other);
}

void test_ksCursor()
{
	KeySet *ks=ksNew(0);
	Key * key;
	cursor_t cursor;
	Key *cur;
	int i;
	char name [] = "user/n";
	char output [] = "n key not on its place";

	printf("Test keyset cursor\n");

	ksAppendKey(ks,cur=keyNew("user/1", KEY_END));
	succeed_if (ksCurrent(ks) == cur, "cursor not set after append key");
	ksAppendKey(ks,cur=keyNew("user/2", KEY_END));
	succeed_if (ksCurrent(ks) == cur, "cursor not set after append key");
	ksAppendKey(ks,cur=keyNew("user/3", KEY_END));
	succeed_if (ksCurrent(ks) == cur, "cursor not set after append key");
	cursor = ksGetCursor (ks);
	succeed_if_same_string (keyName(ksAtCursor(ks, cursor)), "user/3");
	ksAppendKey(ks,cur=keyNew("user/4", KEY_END));
	succeed_if (ksCurrent(ks) == cur, "cursor not set after append key");
	ksAppendKey(ks,cur=keyNew("user/5", KEY_END));
	succeed_if (ksCurrent(ks) == cur, "cursor not set after append key");
	succeed_if(ksGetSize(ks) == 5, "could not append 5 keys");

	succeed_if_same_string (keyName(ksAtCursor(ks, cursor)), "user/3");
	ksSetCursor(ks, cursor);
	succeed_if (cursor == ksGetCursor(ks), "cursor not set to 3");
	succeed_if_same_string (keyName(ksAtCursor(ks, cursor)), "user/3");
	ksSetCursor (ks, cursor);
	succeed_if (cursor == ksGetCursor(ks), "cursor not set to 3 (again)");

	cursor = ksGetCursor (ks);
	key = ksPop(ks);
	succeed_if (cursor == ksGetCursor(ks), "cursor should stay the same");
	succeed_if(strcmp(keyName(key), "user/5") == 0, "1 key not on first place");
	succeed_if (keyDel (key) == 0, "could not del popped key");

	ksRewind (ks);
	for (i=0; i<5; i++)
	{
		key = ksNext(ks);
		if (i==1)
		{
			cursor = ksGetCursor (ks);
			name[5] = '0' + i;
			output[0] = '0' + i;
		}
	}
	ksSetCursor(ks, cursor);
	key = ksCurrent (ks);

	ksDel(ks);

	ks = ksNew(10,
		keyNew("user/0", KEY_END),
		keyNew("user/1", KEY_END),
		keyNew("user/2", KEY_END),
		keyNew("user/3", KEY_END),
		0);

	ksRewind (ks);
	for (i=0; i < 4; i++)
	{
		key = ksNext(ks);
		if (i==1)
		{
			cursor = ksGetCursor (ks);
			name[5] = '0' + i;
			output[0] = '0' + i;
		}
	}

	ksSetCursor(ks, cursor);
	key = ksCurrent (ks);
	succeed_if(!strcmp (keyName(key), name), output);

	ksDel (ks);

	ks = ksNew(10,
		keyNew("user/0", KEY_END),
		keyNew("user/1", KEY_END),
		keyNew("user/2", KEY_END),
		keyNew("user/3", KEY_END),
		0);

	ksRewind (ks);
	for (i=0; i < 4; i++)
	{
		key = ksNext(ks);
		cursor = ksGetCursor (ks);
		name[5] = '0' + i;
		succeed_if_same_string (keyName(ksAtCursor(ks, cursor)), name);
	}

	succeed_if_same_string (keyName(ksAtCursor(ks, 0)), "user/0");
	succeed_if_same_string (keyName(ksAtCursor(ks, 1)), "user/1");
	succeed_if_same_string (keyName(ksAtCursor(ks, 2)), "user/2");
	succeed_if_same_string (keyName(ksAtCursor(ks, 3)), "user/3");
	succeed_if(ksAtCursor(ks, -1) == 0, "bounds check not correct");
	succeed_if(ksAtCursor(ks, 4) == 0, "bounds check not correct");

	ksDel (ks);
}

void test_ksAtCursor()
{
	KeySet *ks;
	Key *current;
	Key *other;
	Key *testKeys[5];
	ks = ksNew (0);

	testKeys[0] = keyNew ("user/test1", KEY_END);
	testKeys[1] = keyNew ("user/test2", KEY_END);
	testKeys[2] = keyNew ("user/test3", KEY_END);
	testKeys[3] = keyNew ("user/test4", KEY_END);
	testKeys[4] = keyNew ("user/test5", KEY_END);

	for (size_t index = 0; index < 5; index++)
	{
		ksAppendKey (ks, testKeys[index]);
	}

	ksRewind (ks);

	cursor_t cursor;

	/* test whether the correct key is returned */
	for (size_t index = 0; index < 5; index++)
	{
		current = testKeys[index];
		ksNext (ks);
		cursor = ksGetCursor (ks);
		other = ksAtCursor (ks, cursor);
		succeed_if(!strcmp (keyName (current), keyName (other)),
				"Key with wrong name returned");
	}

	/* test whether the correct key is returned even if
	 * the internal cursor is positioned somewhere else */
	ksRewind (ks);
	ksNext (ks);
	cursor = ksGetCursor (ks);
	ksNext (ks);
	ksNext (ks);
	current = ksAtCursor (ks, cursor);
	succeed_if (!strcmp (keyName(current), "user/test1"), "wrong key returned after internal cursor was moved");

	/* test whether the internal cursor is modified */
	ksRewind (ks);
	ksNext (ks);
	cursor = ksGetCursor (ks);
	ksNext (ks);
	current = ksAtCursor (ks, cursor);
	succeed_if (!strcmp (keyName (current), "user/test1"), "initial cursor position wrong");
	current = ksNext (ks);
	succeed_if (!strcmp (keyName (current), "user/test3"), "internal cursor not correct");

	/* test postconditions */
	succeed_if (!ksAtCursor (0, cursor), "did not return NULL on NULL keyset");
	succeed_if (!ksAtCursor (ks, -1), "did not return NULL on negative cursor");
	succeed_if (!ksAtCursor (ks, 10), "did not return NULL on invalid cursor");

	ksDel (ks);

}

void test_ksSort()
{
	KeySet	*ks;
	Key	*key, *k1, *k2;
	int	i;

	printf("Test ks sort\n");

	ks=ksNew(0);
	ksAppendKey(ks, keyNew("user/bname", KEY_END));
	ksAppendKey(ks, keyNew("user/aname", KEY_END));
	ksAppendKey(ks, keyNew("user/cname", KEY_END));
	
	ksRewind(ks);
	key = ksNext(ks);
	succeed_if (strcmp (keyName (key), "user/aname") == 0, "a should be 1.");
	
	key = ksNext(ks);
	succeed_if (strcmp (keyName (key), "user/bname") == 0, "b should be 2.");
	
	key = ksNext(ks);
	succeed_if (strcmp (keyName (key), "user/cname") == 0, "c should be 3.");
	ksDel (ks);
	
	ks=ksNew(0);
	ksAppendKey(ks, keyNew("user/a", KEY_END));
	ksAppendKey(ks, keyNew("user/e", KEY_END));
	ksAppendKey(ks, keyNew("user/b1", KEY_END));
	ksAppendKey(ks, keyNew("user/h2", KEY_END));
	ksAppendKey(ks, keyNew("user/b2", KEY_END));
	ksAppendKey(ks, keyNew("user/d", KEY_END));
	ksAppendKey(ks, keyNew("user/a", KEY_END));
	ksAppendKey(ks, keyNew("user/g", KEY_END));
	ksAppendKey(ks, keyNew("user/g", KEY_END));
	ksAppendKey(ks, keyNew("user/c2", KEY_END));
	ksAppendKey(ks, keyNew("user/c1", KEY_END));
	ksAppendKey(ks, keyNew("user/g", KEY_END));
	ksAppendKey(ks, keyNew("user/h1", KEY_END));
	ksAppendKey(ks, keyNew("user/f", KEY_END));

	ksRewind (ks);
	for (i=0; (key=ksNext(ks)) != 0; i++)
	{
		switch (i)
		{
		case 0:	succeed_if (strcmp (keyName (key), "user/a") == 0, "wrong name found.");
			break;
		case 1:	succeed_if (strcmp (keyName (key), "user/b1") == 0, "wrong name found.");
			break;
		case 2:	succeed_if (strcmp (keyName (key), "user/b2") == 0, "wrong name found.");
			break;
		case 3:	succeed_if (strcmp (keyName (key), "user/c1") == 0, "wrong name found.");
			break;
		case 4:	succeed_if (strcmp (keyName (key), "user/c2") == 0, "wrong name found.");
			break;
		case 5:	succeed_if (strcmp (keyName (key), "user/d") == 0, "wrong name found.");
			break;
		case 6:	succeed_if (strcmp (keyName (key), "user/e") == 0, "wrong name found.");
			break;
		case 7:	succeed_if (strcmp (keyName (key), "user/f") == 0, "wrong name found.");
			break;
		case 8:	succeed_if (strcmp (keyName (key), "user/g") == 0, "wrong name found.");
			break;
		case 9:	succeed_if (strcmp (keyName (key), "user/h1") == 0, "wrong name found.");
			break;
		case 10:succeed_if (strcmp (keyName (key), "user/h2") == 0, "wrong name found.");
			break;
		default:succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);

	ks=ksNew(0);
	k1 = keyNew("user/xname", KEY_END);
	ksAppendKey(ks,k1);

	k2 = keyDup (k1);

	succeed_if (keyGetRef(k2) == 0, "reference counter not resetted");
	ksAppendKey(ks,k2);
	succeed_if (keyGetRef(k2) == 1, "reference counter not incremented after insertion");

	ksRewind (ks);
	key = ksNext(ks);
	ksDel(ks);
	
	ks=ksNew(0);
	k1 = keyNew("user/yname", KEY_END);
	k2 = keyDup (k1);
	ksAppendKey(ks,k2);
	ksAppendKey(ks,k1);

	ksRewind (ks);
	key = ksNext(ks);
	ksDel(ks);

	ks=ksNew(0);
	ksAppendKey(ks, keyNew("user/a", KEY_END));
	ksAppendKey(ks, keyNew("user/e", KEY_END));
	ksAppendKey(ks, keyNew("user/b", KEY_END));
	ksAppendKey(ks, keyNew("user/b", KEY_END));
	ksAppendKey(ks, keyNew("user/d", KEY_END));
	ksAppendKey(ks, keyNew("user/c", KEY_END));
	ksAppendKey(ks, keyNew("user/c", KEY_END));
	ksAppendKey(ks, keyNew("user/g", KEY_END));
	ksAppendKey(ks, keyNew("user/h", KEY_END));
	ksAppendKey(ks, keyNew("user/h", KEY_END));
	ksAppendKey(ks, keyNew("user/f", KEY_END));

	ksRewind (ks);
	for (i=0; (key=ksNext(ks)) != 0; i++)
	{
		switch (i)
		{
		case 0:	succeed_if (strcmp (keyName (key), "user/a") == 0, "wrong name found.");
			break;
		case 1:	succeed_if (strcmp (keyName (key), "user/b") == 0, "wrong name found.");
			break;
		case 2:	succeed_if (strcmp (keyName (key), "user/c") == 0, "wrong name found.");
			break;
		case 3:	succeed_if (strcmp (keyName (key), "user/d") == 0, "wrong name found.");
			break;
		case 4:	succeed_if (strcmp (keyName (key), "user/e") == 0, "wrong name found.");
			break;
		case 5:	succeed_if (strcmp (keyName (key), "user/f") == 0, "wrong name found.");
			break;
		case 6:	succeed_if (strcmp (keyName (key), "user/g") == 0, "wrong name found.");
			break;
		case 7:	succeed_if (strcmp (keyName (key), "user/h") == 0, "wrong name found.");
			break;
		default:succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);

	
	ks=ksNew(0);
	ksAppendKey(ks, keyNew("user/a", KEY_END));
	ksAppendKey(ks, keyNew("user/e", KEY_END));
	ksAppendKey(ks, keyNew("user/b/a", KEY_END));
	ksAppendKey(ks, keyNew("user/b", KEY_END));
	ksAppendKey(ks, keyNew("user/d", KEY_END));
	ksAppendKey(ks, keyNew("user/c", KEY_END));
	ksAppendKey(ks, keyNew("user/c/a", KEY_END));
	ksAppendKey(ks, keyNew("user/g", KEY_END));
	ksAppendKey(ks, keyNew("user/h/a", KEY_END));
	ksAppendKey(ks, keyNew("user/h", KEY_END));
	ksAppendKey(ks, keyNew("user/f", KEY_END));

	ksRewind (ks);
	// output_keyset(ks,0);
	for (i=0; (key=ksNext(ks)) != 0; i++)
	{
		switch (i)
		{
		case 10:succeed_if (strcmp (keyName (key), "user/h/a") == 0, "wrong name found.");
			break;
		case 9:	succeed_if (strcmp (keyName (key), "user/h") == 0, "wrong name found.");
			break;
		case 8:	succeed_if (strcmp (keyName (key), "user/g") == 0, "wrong name found.");
			break;
		case 7:	succeed_if (strcmp (keyName (key), "user/f") == 0, "wrong name found.");
			break;
		case 6:	succeed_if (strcmp (keyName (key), "user/e") == 0, "wrong name found.");
			break;
		case 5:	succeed_if (strcmp (keyName (key), "user/d") == 0, "wrong name found.");
			break;
		case 4:	succeed_if (strcmp (keyName (key), "user/c/a") == 0, "wrong name found.");
			break;
		case 3:	succeed_if (strcmp (keyName (key), "user/c") == 0, "wrong name found.");
			break;
		case 2:	succeed_if (strcmp (keyName (key), "user/b/a") == 0, "wrong name found.");
			break;
		case 1:	succeed_if (strcmp (keyName (key), "user/b") == 0, "wrong name found.");
			break;
		case 0:	succeed_if (strcmp (keyName (key), "user/a") == 0, "wrong name found.");
			break;
		default:succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);
	
	ks=ksNew(0);
	ksAppendKey(ks, keyNew("user/dir1/key1", KEY_END));
	ksAppendKey(ks, keyNew("user/dir1/key2", KEY_END));
	ksAppendKey(ks, keyNew("user/dir1/key3", KEY_END));
	ksAppendKey(ks, keyNew("user/dir2",      KEY_END));
	ksAppendKey(ks, keyNew("user/dir2/key1", KEY_END));
	ksAppendKey(ks, keyNew("user/dir3/key1", KEY_END));
	ksAppendKey(ks, keyNew("user/dir3",      KEY_END));
	ksAppendKey(ks, keyNew("user/dir3/key2", KEY_END));
	ksAppendKey(ks, keyNew("user/dir4",      KEY_END));
	ksAppendKey(ks, keyNew("user/dir5/key1", KEY_END));
	ksAppendKey(ks, keyNew("user/dir6/key1", KEY_END));

	ksRewind (ks);
	// output_keyset(ks,0);
	for (i=0; (key=ksNext(ks)) != 0; i++)
	{
		switch (i)
		{
		case 9:	succeed_if (strcmp (keyName (key), "user/dir5/key1") == 0, "wrong name found.");
			break;
		case 4:	succeed_if (strcmp (keyName (key), "user/dir2/key1") == 0, "wrong name found.");
			break;
		case 3:	succeed_if (strcmp (keyName (key), "user/dir2") == 0, "wrong name found.");
			break;
		case 2:	succeed_if (strcmp (keyName (key), "user/dir1/key3") == 0, "wrong name found.");
			break;
		case 0:	succeed_if (strcmp (keyName (key), "user/dir1/key1") == 0, "wrong name found.");
			break;
		case 1:	succeed_if (strcmp (keyName (key), "user/dir1/key2") == 0, "wrong name found.");
			break;
		case 5:	succeed_if (strcmp (keyName (key), "user/dir3") == 0, "wrong name found.");
			break;
		case 6:	succeed_if (strcmp (keyName (key), "user/dir3/key1") == 0, "wrong name found.");
			break;
		case 7:	succeed_if (strcmp (keyName (key), "user/dir3/key2") == 0, "wrong name found.");
			break;
		case 8:	succeed_if (strcmp (keyName (key), "user/dir4") == 0, "wrong name found.");
			break;
		case 10:succeed_if (strcmp (keyName (key), "user/dir6/key1") == 0, "wrong name found.");
			break;
		default:succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);
}

void ksUnsort (KeySet *ks)
{
	Key *cur;
	size_t size = 0;
	KeySet *randks=ksNew(0); /*This is the final randomized keyset*/
	KeySet *tempks=ksNew(0); /*Temporary storage for keys not chosen to be inserted*/

	while (ksGetSize(ks) > 0)
	{
		ksRewind(ks);
		size = ksGetSize(ks);
		/* printf ("iterating %d\n", size); */
		while ((cur=ksPop(ks)) != 0)
		{
			/* printf ("\titerating %s\n", keyName(cur)); */
			if (!(rand()%size)) ksAppendKey(randks, cur);
			else ksAppendKey(tempks,cur);
		}
		ksAppend(ks, tempks);
		ksCopy(tempks,0);
	}

	ksCopy (ks, randks);

	ksDel (randks);
	ksDel (tempks);
}

void test_ksLookup()
{
	printf ("Test lookup\n");

	int i,j;
	Key *k[1000];
	KeySet *ks = ksNew (30,
		k[0]=keyNew ("user/rem3", KEY_DIR, KEY_END),
		k[1]=keyNew ("user/rem2", KEY_DIR, KEY_END),
		k[2]=keyNew ("user/rem1/key2", KEY_END),
		k[3]=keyNew ("user/rem1/key1", KEY_END),
		k[4]=keyNew ("user/rem1", KEY_DIR, KEY_END),
		k[5]=keyNew ("user/dir1", KEY_DIR, KEY_END),
		k[6]=keyNew ("user/dir1/key1", KEY_VALUE, "value1", KEY_END),
		k[7]=keyNew ("user/dir1/key2", KEY_VALUE, "value2", KEY_END),
		k[8]=keyNew ("user/dir1/key3", KEY_VALUE, "value3", KEY_END),
		k[9]=keyNew ("user/dir1/key4", KEY_VALUE, "value4", KEY_END),
		k[10]=keyNew ("user/dir1/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		k[11]=keyNew ("user/dir1/.inactive2", KEY_COMMENT, "additional information", KEY_END),
		k[12]=keyNew ("user:max/dir2", KEY_DIR, KEY_END),
		k[13]=keyNew ("user:max/dir2/key1", KEY_VALUE, "value1", KEY_END),
		k[14]=keyNew ("user/dir2/key2", KEY_VALUE, "value2", KEY_END),
		k[15]=keyNew ("user/dir2/key3", KEY_VALUE, "value3", KEY_END),
		k[16]=keyNew ("user:hugo/dir2/key4", KEY_VALUE, "value4", KEY_END),
		k[17]=keyNew ("user/dir3", KEY_DIR, KEY_END),
		k[18]=keyNew ("user/dir3/key1", KEY_VALUE, "value1", KEY_END),
		k[19]=keyNew ("user:sb/dir3/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		k[20]=keyNew ("user/dir3/.inactive2", KEY_COMMENT, "a users comment", KEY_END),
		k[21]=keyNew ("user/dir4", KEY_DIR, KEY_END),
		k[22]=keyNew ("user/dir5", KEY_DIR, KEY_END),
		KS_END);

	/* key 5 */
	k[23] = keyNew ("user/DiR1", KEY_END);
	/* key 6 */
	k[24] = keyNew ("user/DiR1/KEY1", KEY_END);
	k[25] = keyNew ("user:wrongowner/DiR1/KEY1", KEY_END);
	k[26] = keyNew ("user:nop/DiR1/KEY1", KEY_END);
	k[27] = keyNew ("user:wrongowner/dir1/key1", KEY_END);
	k[28] = keyNew ("user:nop/dir1/key1", KEY_END);
	/*key 13 */
	k[29] = keyNew ("user:wrongowner/dir2/key1", KEY_END);
	k[30] = keyNew ("user/dir2/key1", KEY_END);
	k[31] = keyNew ("user:max/dir2/key1", KEY_END);

	srand(23);

	succeed_if (ksLookup(0, k[23], 0) == 0, "null pointer");
	succeed_if (ksLookup(ks, 0, 0) == 0, "null pointer");

	for (i=0; i<100; i++)
	{
		ksUnsort(ks);
		for (j=0; j<23;j++)
			succeed_if (ksLookup(ks, k[j], 0)==k[j], "did not found key");
		succeed_if (ksLookup(ks, k[23], KDB_O_NOCASE) == k[5], "did not found key");
		succeed_if (ksLookup(ks, k[23], 0) == 0, "found wrong key");
		succeed_if (ksLookup(ks, k[24], KDB_O_NOCASE) == k[6], "did not found key");
		succeed_if (ksLookup(ks, k[24], 0) == 0, "found wrong key");
		succeed_if (ksLookup(ks, k[25], KDB_O_NOCASE) == k[6], "did not found key");
		succeed_if (ksLookup(ks, k[25], KDB_O_WITHOWNER|KDB_O_NOCASE) == 0, "found wrong key");
		succeed_if (ksLookup(ks, k[28], 0) == k[6], "did not found key");
		succeed_if (ksLookup(ks, k[28], KDB_O_WITHOWNER) == 0, "found wrong key");
		succeed_if (ksLookup(ks, k[31], KDB_O_WITHOWNER) == k[13], "did not found key");
		/* Empty lines to add more tests:
		succeed_if (ksLookup(ks, k[], ) == k[], "did not found key");
		succeed_if (ksLookup(ks, k[], ) == 0, "found wrong key");
		*/
	}

	ksDel (ks);
	for (i=23; i<32;i++) keyDel (k[i]);
}

void test_ksLookupByName()
{
	printf ("Test lookup by name\n");

	int i,j;
	char *name[1000];
	Key *k[1000];
	KeySet *ks = ksNew (30,
		k[0]=keyNew (name[0] = "user/rem3", KEY_DIR, KEY_END),
		k[1]=keyNew (name[1] = "user/rem2", KEY_DIR, KEY_END),
		k[2]=keyNew (name[2] = "user/rem1/key2", KEY_END),
		k[3]=keyNew (name[3] = "user/rem1/key1", KEY_END),
		k[4]=keyNew (name[4] = "user/rem1", KEY_DIR, KEY_END),
		k[5]=keyNew (name[5] = "user/dir1", KEY_DIR, KEY_END),
		k[6]=keyNew (name[6] = "user/dir1/key1", KEY_VALUE, "value1", KEY_END),
		k[7]=keyNew (name[7] = "user/dir1/key2", KEY_VALUE, "value2", KEY_END),
		k[8]=keyNew (name[8] = "user/dir1/key3", KEY_VALUE, "value3", KEY_END),
		k[9]=keyNew (name[9] = "user/dir1/key4", KEY_VALUE, "value4", KEY_END),
		k[10]=keyNew (name[10] ="user/dir1/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		k[11]=keyNew (name[11] ="user/dir1/.inactive2", KEY_COMMENT, "additional information", KEY_END),
		k[12]=keyNew (name[12] ="user:max/dir2", KEY_DIR, KEY_END),
		k[13]=keyNew (name[13] ="user:max/dir2/key1", KEY_VALUE, "value1", KEY_END),
		k[14]=keyNew (name[14] ="user/dir2/key2", KEY_VALUE, "value2", KEY_END),
		k[15]=keyNew (name[15] ="user/dir2/key3", KEY_VALUE, "value3", KEY_END),
		k[16]=keyNew (name[16] ="user:hugo/dir2/key4", KEY_VALUE, "value4", KEY_END),
		k[17]=keyNew (name[17] ="user/dir3", KEY_DIR, KEY_END),
		k[18]=keyNew (name[18] ="user/dir3/key1", KEY_VALUE, "value1", KEY_END),
		k[19]=keyNew (name[19] ="user:sb/dir3/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		k[20]=keyNew (name[20] ="user/dir3/.inactive2", KEY_COMMENT, "a users comment", KEY_END),
		k[21]=keyNew (name[21] ="user/dir4", KEY_DIR, KEY_END),
		k[22]=keyNew (name[22] ="user/dir5", KEY_DIR, KEY_END),
		KS_END);

	name[23] = "user/DiR1";
	name[24] = "user/DiR1/KEY1";
	name[25] = "user:wrongowner/DiR1/KEY1";
	name[26] = "user:nop/DiR1/KEY1";
	name[27] = "user:wrongowner/dir1/key1";
	name[28] = "user:nop/dir1/key1";
	name[29] = "user:wrongowner/dir2/key1";
	name[30] = "user/dir2/key1";
	name[31] = "user:max/dir2/key1";

	srand(23);

	succeed_if (ksLookupByName(0, name[23], 0) == 0, "null pointer");
	succeed_if (ksLookup(ks, 0, 0) == 0, "null pointer");

	for (i=0; i<100; i++)
	{
		ksUnsort(ks);
		for (j=0; j<23;j++)
			succeed_if (ksLookupByName(ks, name[j], 0)==k[j], "did not found key");
		succeed_if (ksLookupByName(ks, name[23], KDB_O_NOCASE) == k[5], "did not found key");
		succeed_if (ksLookupByName(ks, name[23], 0) == 0, "found wrong key");
		succeed_if (ksLookupByName(ks, name[24], KDB_O_NOCASE) == k[6], "did not found key");
		succeed_if (ksLookupByName(ks, name[24], 0) == 0, "found wrong key");
		succeed_if (ksLookupByName(ks, name[25], KDB_O_NOCASE) == k[6], "did not found key");
		succeed_if (ksLookupByName(ks, name[25], KDB_O_WITHOWNER|KDB_O_NOCASE) == 0, "found wrong key");
		succeed_if (ksLookupByName(ks, name[28], 0) == k[6], "did not found key");
		succeed_if (ksLookupByName(ks, name[28], KDB_O_WITHOWNER) == 0, "found wrong key");
		succeed_if (ksLookupByName(ks, name[31], KDB_O_WITHOWNER) == k[13], "did not found key");
		/* Empty lines to add more tests:
		succeed_if (ksLookupByName(ks, name[], ) == name[], "did not found key");
		succeed_if (ksLookupByName(ks, name[], ) == 0, "found wrong key");
		*/
	}

	ksDel (ks);
}


void test_ksLookupName()
{
	Key * found;
	KeySet *ks= ksNew(0);
	
	printf ("Test lookup functions\n");

	ksAppendKey(ks, keyNew("user/domain/key",  KEY_VALUE, "domainvalue",
		KEY_OWNER, "markus", KEY_END));
	ksAppendKey(ks, keyNew("user/single/key",  KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey(ks, keyNew("system/named/syskey", KEY_VALUE, "syskey",  KEY_END));
	ksAppendKey(ks, keyNew("system/sysonly/key", KEY_VALUE, "sysonlykey",  KEY_END));
	ksAppendKey(ks, keyNew("user/named/bin", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "binary\1\2data", KEY_END));
	ksAppendKey(ks, keyNew("system/named/bin", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "sys\1bin\2", KEY_END));
	ksAppendKey(ks, keyNew("system/named/key", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "syskey", KEY_END));
	succeed_if (ksGetSize(ks) == 8, "could not append all keys");

	// a positive testcase
	found = ksLookupByName (ks, "user/named/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");

	succeed_if (found != 0, "did not found correct name");
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	ksAppendKey(ks, found = keyNew("user/single/key",  KEY_VALUE, "singlevalue", KEY_END));
	succeed_if (ksCurrent(ks) == found, "current update after append");
	succeed_if (strcmp (keyName(found), "user/single/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");

	// here you can't find the keys
	succeed_if (ksLookupByName (ks, "named/key", 0) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "u/named/key", 0) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "usea/named/key", 0) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, " user/named/key", 0) == 0, "found key with bad prefix");

	succeed_if (ksLookupByName (ks, "user/named/Key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User/Named/key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User/named/key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "user/NAMED/key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "USER/NAMED/KEY", 0) == 0, "found wrong case key");
	
	succeed_if (ksLookupByName (ks, "user/named/keys", 0) == 0, "wrong postfix");
	succeed_if (ksLookupByName (ks, "user/named/key_", 0) == 0, "wrong postfix");

	succeed_if (ksLookupByName (ks, "user/named/k/ey", 0) == 0, "seperation that should be");
	succeed_if (ksLookupByName (ks, "user/na/med/key", 0) == 0, "seperation that should be");

	succeed_if (ksLookupByName (ks, "system/domain/key", 0) == 0, "found key in wrong domain");
	
	//now try to find them, and compare value
	found = ksLookupByName (ks, "user/domain/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "user/domain/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "domainvalue") == 0, "not correct value in found key");
	
	found = ksLookupByName (ks, "user/single/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "user/single/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");
	
	found = ksLookupByName (ks, "system/named/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "system/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "syskey") == 0, "not correct value in found key");
	
	found = ksLookupByName (ks, "user/named/bin", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "user/named/bin") == 0, "name not correct in found key");
	succeed_if (strncmp (keyValue(found), "binary\1\2data",10) == 0, "not correct value in found key");

	found = ksLookupByName (ks, "user/named/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	printf ("Test nocase lookup functions\n");
	found = ksLookupByName (ks, "user/named/key", KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	found = ksLookupByName (ks, "user/NameD/KeY", KDB_O_NOCASE);
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	found = ksLookupByName (ks, "user/NameD/KEY", KDB_O_NOCASE);
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	ksDel(ks);
}

void test_ksLookupNameCascading()
{
	Key * found;
	KeySet *ks= ksNew(0);

	printf ("Test cascading lookup functions\n");

	succeed_if (ksLookupByName (ks, "/named/", 0) == 0, "found in empty keyset");
	succeed_if (ksLookupByName (ks, "//named/", 0) == 0, "found in empty keyset");
	succeed_if (ksLookupByName (ks, "////named/", 0) == 0, "found in empty keyset");
	succeed_if (ksLookupByName (ks, "//Person/Visits", 0) == 0, "found in empty keyset");

	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey(ks, keyNew("system/named/key",   KEY_VALUE, "wrong value", KEY_END));
	ksAppendKey(ks, keyNew("user/single/key",  KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey(ks, keyNew("system/sysonly/key", KEY_VALUE, "sysonlykey",  KEY_END));
	ksAppendKey(ks, keyNew("user/named/otherkey",  KEY_VALUE, "singlevalue", KEY_END));

	found = ksLookupByName (ks, "/named/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	found = ksLookupByName (ks, "/single/Key", KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "user/single/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");

	found = ksLookupByName (ks, "/sysonly/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "system/sysonly/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "sysonlykey") == 0, "not correct value in found key");

	succeed_if (ksLookupByName (ks, "/named/", 0) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "/named/keyd", 0) == 0, "found part of key with cascading, bad postfix");


	// cascading double slash

	found = ksLookupByName (ks, "///named/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");

	found = ksLookupByName (ks, "////single/Key", KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "user/single/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");

	found = ksLookupByName (ks, "//sysonly/key", 0);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "system/sysonly/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "sysonlykey") == 0, "not correct value in found key");

	succeed_if (ksLookupByName (ks, "//Person/Visits", 0) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "////named/", 0) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "/////named/keyd", 0) == 0, "found part of key with cascading, bad postfix");

	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	found = ksLookupByName (ks, "//named/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "///named/Otherkey", KDB_O_NOCASE | KDB_O_POP);
	succeed_if (ksGetSize(ks) == 3, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "user/named/otherkey") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "///sysonly/key", KDB_O_POP);
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (ksGetSize(ks) == 2, "did not pop key");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "system/sysonly/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "sysonlykey") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	succeed_if (ksLookupByName (ks, "///named/", KDB_O_POP) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "///named/keyd", KDB_O_POP) == 0, "found part of key with cascading, bad postfix");


	ksDel(ks);
}

void test_ksLookupNameDomain()
{
	Key * found;
	KeySet *ks= ksNew(0);

	printf ("Test domain lookup functions\n");

	ksAppendKey(ks, keyNew("user/domain/key",  KEY_VALUE, "domainvalue",
		KEY_OWNER, "markus", KEY_END));

	found = ksLookupByName (ks, "user:markus/domain/key", KDB_O_WITHOWNER);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find domain key");
	succeed_if (strcmp (keyValue(found), "domainvalue") == 0, "not correct value in domain key");
	succeed_if (ksLookupByName (ks, "user:hugo/domain/key", KDB_O_WITHOWNER) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:y/domain/key", KDB_O_WITHOWNER) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:markuss/domain/key", KDB_O_WITHOWNER) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:marku/domain/key", KDB_O_WITHOWNER) == 0, "found key in wrong domain");

	ksAppendKey(ks, keyNew("user:markus/domain/key",   KEY_VALUE, "domainvalue", KEY_END));
	found = ksLookupByName (ks, "user:markus/domain/key", KDB_O_WITHOWNER | KDB_O_POP);
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (ksGetSize(ks) == 0, "did not pop key");
	succeed_if (found != 0, "could not find domain key");
	succeed_if (strcmp (keyValue(found), "domainvalue") == 0, "not correct value in domain key");
	succeed_if (ksLookupByName (ks, "user:hugo/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:y/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:markuss/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:marku/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksGetSize(ks) == 0, "did not pop key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksDel(ks);
}

void test_ksLookupNameAll()
{
	Key * found;
	cursor_t cursor;
	KeySet *ks= ksNew(0);
	printf ("Test lookup functions with KDB_O_NOALL\n");
	ksAppendKey(ks, keyNew("user/a", KEY_END));
	ksAppendKey(ks, keyNew("user/b", KEY_END));
	ksAppendKey(ks, keyNew("user/c", KEY_END));
	ksAppendKey(ks, keyNew("user/d", KEY_END));
	ksAppendKey(ks, keyNew("user/e", KEY_END));
	ksAppendKey(ks, keyNew("user/00", KEY_DIR, KEY_END));
	ksAppendKey(ks, keyNew("user/00/a", KEY_END));
	ksAppendKey(ks, keyNew("user/00/b", KEY_END));
	ksAppendKey(ks, keyNew("user/00/c", KEY_END));
	ksAppendKey(ks, keyNew("user/01", KEY_DIR, KEY_END));
	ksAppendKey(ks, keyNew("user/01/a", KEY_END));
	ksAppendKey(ks, keyNew("user/01/b", KEY_END));
	ksAppendKey(ks, keyNew("user/01/c", KEY_END));
	ksAppendKey(ks, keyNew("user/01/d", KEY_END));
	ksAppendKey(ks, keyNew("user/02", KEY_DIR, KEY_END));
	ksAppendKey(ks, keyNew("user/02/a", KEY_END));
	ksAppendKey(ks, keyNew("user/02/b", KEY_END));
	ksAppendKey(ks, keyNew("user/02/c", KEY_END));
	ksAppendKey(ks, keyNew("user/02/d", KEY_END));
	ksAppendKey(ks, keyNew("user/02/e", KEY_END));
	ksAppendKey(ks, keyNew("user/02/f", KEY_END));

	found = ksLookupByName (ks, "user/e", KDB_O_NOALL);
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/e") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/e", KDB_O_NOALL);
	succeed_if (found == 0, "should not find");

	ksRewind(ks);
	found = ksLookupByName (ks, "user/a", KDB_O_NOALL);
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/a") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/e", KDB_O_NOALL);
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/e") == 0, "name not correct in found key");

	ksRewind(ks);
	found = ksLookupByName (ks, "user/00", KDB_O_NOALL);
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/00") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/01", KDB_O_NOALL);
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/01") == 0, "name not correct in found key");

	found = ksNext (ks);
	succeed_if (found != 0, "could not get next key");
	succeed_if (strcmp (keyName(found), "user/01/a") == 0, "name not correct in next key");

	found = ksLookupByName (ks, "user/02", KDB_O_NOALL);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/02") == 0, "name not correct in found key");

	cursor = ksGetCursor (ks);
	found = ksLookupByName (ks, "user/01", KDB_O_NOALL);
	succeed_if (ksGetCursor(ks) == cursor, "cursor should not change");
	succeed_if (found == 0, "should not find");

	ksRewind(ks);
	found = ksLookupByName (ks, "user/a", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/a") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/E", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/e") == 0, "name not correct in found key");

	ksRewind(ks);
	found = ksLookupByName (ks, "user/00", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/00") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/01", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/01") == 0, "name not correct in found key");

	found = ksNext (ks);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not get next key");
	succeed_if (strcmp (keyName(found), "user/01/a") == 0, "name not correct in next key");

	found = ksLookupByName (ks, "user/02", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/02") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/02/F", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksCurrent(ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/02/f") == 0, "name not correct in found key");

	cursor = ksGetCursor (ks);
	found = ksLookupByName (ks, "user/02", KDB_O_NOALL | KDB_O_NOCASE);
	succeed_if (ksGetCursor(ks) == cursor, "cursor should stay as is if not found");
	succeed_if (found == 0, "should not find");

	ksRewind(ks);
	found = ksLookupByName (ks, "user/00", KDB_O_NOALL | KDB_O_WITHOWNER | KDB_O_NOCASE);
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/00") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/01", KDB_O_NOALL | KDB_O_WITHOWNER | KDB_O_NOCASE);
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/01") == 0, "name not correct in found key");

	found = ksNext (ks);
	succeed_if (found != 0, "could not get next key");
	succeed_if (strcmp (keyName(found), "user/01/a") == 0, "name not correct in next key");

	found = ksLookupByName (ks, "user/02", KDB_O_NOALL | KDB_O_WITHOWNER | KDB_O_NOCASE);
	succeed_if (found != 0, "could not find key");
	succeed_if (keyIsDir (found) == 1, "should be dir");
	succeed_if (strcmp (keyName(found), "user/02") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/a", KDB_O_NOALL | KDB_O_WITHOWNER | KDB_O_NOCASE);
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/a") == 0, "name not correct in found key");

	found = ksLookupByName (ks, "user/E", KDB_O_NOALL | KDB_O_WITHOWNER | KDB_O_NOCASE);
	succeed_if (found != 0, "could not find key");
	succeed_if (strcmp (keyName(found), "user/e") == 0, "name not correct in found key");


	found = ksLookupByName (ks, "user:notvalid/02/F", KDB_O_NOALL | KDB_O_WITHOWNER | KDB_O_NOCASE);
	succeed_if (found == 0, "should not find key");

	ksDel (ks);
}

/*
void test_ksLookupValue()
{
	KeySet *ks = ksNew(0);
	Key *found;
	printf ("test lookups for values\n");
	
	ksAppendKey(ks, keyNew("user/1",  KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey(ks, keyNew("user/2",   KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey(ks, keyNew("user/3", KEY_VALUE, "syskey",  KEY_END));
	
	ksAppendKey(ks, keyNew("user/b1", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "binary\1\2data", KEY_END));
	ksAppendKey(ks, keyNew("user/b2", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "sys\1bin\2", KEY_END));
	ksAppendKey(ks, keyNew("user/b3", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "sy\1\33\12\32skey", KEY_END));
	succeed_if(ksGetSize(ks) == 6, "could not append keys");

	succeed_if (ksLookupByString (ks, "value", 0) == 0, "found part of value");
	succeed_if (ksLookupByString (ks, " singlevalue", 0) == 0, "prefix of value");
	succeed_if (ksLookupByString (ks, "/singlevalue", 0) == 0, "prefix of value");
	succeed_if (ksLookupByString (ks, "singlevalue ", 0) == 0, "postfix of value");

	found = ksLookupByString(ks, "singlevalue", 0);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/1") == 0, "not correct key found");

	found = ksLookupByString(ks, "singlevalue", 0);
	succeed_if (found != 0, "could not find value again");
	succeed_if (strcmp (keyName(found), "user/1") == 0, "not correct key found");
	
	found = ksLookupByString(ks, "myvalue", 0);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/2") == 0, "not correct key found");

	found = ksLookupByString(ks, "syskey", 0);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/3") == 0, "not correct key found");

	ksRewind(ks);
	found = ksLookupByString(ks, "singlevalue", KDB_O_NOALL);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/1") == 0, "not correct key found");

	found = ksLookupByString(ks, "singlevalue", 0);
	succeed_if (found != 0, "could not find value again");
	succeed_if (strcmp (keyName(found), "user/1") == 0, "not correct key found");

	ksRewind(ks);
	found = ksLookupByString(ks, "myvalue", KDB_O_NOALL);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/2") == 0, "not correct key found");

	found = ksLookupByString(ks, "syskey", 0);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/3") == 0, "not correct key found");

	// TODO: BUG found = ksLookupByString(ks, "singlevalue", KDB_O_NOALL);
	// succeed_if (found == 0, "could find value");

	// found = ksLookupByString(ks, "singlevalue", KDB_O_NOALL);
	// succeed_if (found == 0, "found value again");

	ksRewind(ks);
	found = ksLookupByString(ks, "myvalue", KDB_O_NOALL);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/2") == 0, "not correct key found");

	found = ksLookupByString(ks, "syskey", 0);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/3") == 0, "not correct key found");

	found = ksLookupByString(ks, "syskey", KDB_O_NOALL);
	succeed_if (found != 0, "could not find value");
	succeed_if (strcmp (keyName(found), "user/3") == 0, "not correct key found");

	found = ksLookupByString(ks, "syskey", KDB_O_NOALL);
	succeed_if (found == 0, "found value");

	ksDel(ks);
}
*/

//copied out from example	
void test_ksExample()
{
	KeySet *ks=ksNew(0);
	Key * key;

	ksAppendKey(ks,keyNew("user/test", KEY_END));       // an empty key
		
	ksAppendKey(ks,keyNew("user/sw",              // the name of the key
		KEY_END));                      // no more args
		
	ksAppendKey(ks,keyNew("user/tmp/ex1",
		KEY_VALUE,"some data",          // set a string value
		KEY_END));                      // end of args
		
	ksAppendKey(ks,keyNew("user/tmp/ex2",
		KEY_VALUE,"some data",          // with a simple value
		KEY_MODE,0777,                  // permissions
		KEY_END));                      // end of args
		
	ksAppendKey(ks,keyNew("user/tmp/ex4",
		KEY_BINARY,	// key type
		KEY_SIZE,7,			// assume binary length 7
		KEY_VALUE,"some data",		// value that will be truncated in 7 bytes
		KEY_COMMENT,"value is truncated",
		KEY_OWNER,"root",		// owner (not uid) is root
		KEY_UID,0,			// root uid
		KEY_END));			// end of args

	ksAppendKey(ks,keyNew("user/tmp/ex5",
		KEY_BINARY,	// binary value
		KEY_SIZE,7,
		KEY_VALUE,"some data",		// value that will be truncated in 7 bytes
		KEY_COMMENT,"value is truncated",
		KEY_OWNER,"root",              // owner (not uid) is root
		KEY_UID,0,                      // root uid
		KEY_END));                      // end of args
	
	ksRewind(ks);
	// end of example

	key=ksNext(ks);
	succeed_if(key != NULL, "no next key");
	succeed_if(strcmp(keyName(key), "user/sw") == 0, "keyNew: Key's name setted incorrectly");

	key=ksNext(ks);
	succeed_if(key != NULL, "no next key");
	succeed_if(strcmp(keyName(key), "user/test") == 0, "keyNew: Key's name setted incorrectly");

	key=ksNext(ks);
	succeed_if(key != NULL, "no next key");
	succeed_if(strcmp(keyName(key), "user/tmp/ex1") == 0, "keyNew: Key's name setted incorrectly");

	key=ksNext(ks);
	succeed_if(key != NULL, "no next key");
	succeed_if(strcmp(keyName(key), "user/tmp/ex2") == 0, "keyNew: Key's name setted incorrectly");

	key=ksNext(ks);
	succeed_if(key != NULL, "no next key");
	succeed_if(strcmp(keyName(key), "user/tmp/ex4") == 0, "keyNew: Key's name setted incorrectly");

	key=ksNext(ks);
	succeed_if(key != NULL, "no next key");
	succeed_if(strcmp(keyName(key), "user/tmp/ex5") == 0, "keyNew: Key's name setted incorrectly");

	ksDel(ks);
}

#define MAX_SIZE 200
void test_ksCommonParentName()
{
	char ret [MAX_SIZE+1];
	KeySet *ks = ksNew (10,
		keyNew("system/sw/xorg/Monitors/Monitor1/vrefresh",0),
		keyNew("system/sw/xorg/Monitors/Monitor1/hrefresh",0),
		keyNew("system/sw/xorg/Monitors/Monitor2/vrefresh",0),
		keyNew("system/sw/xorg/Monitors/Monitor2/hrefresh",0),
		keyNew("system/sw/xorg/Devices/Device1/driver",0),
		keyNew("system/sw/xorg/Devices/Device1/mode",0),KS_END);

	printf ("Test common parentname\n");

	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could not find correct parentname");
	succeed_if (strcmp (ret, "system/sw/xorg") == 0, "parentname not correct");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system",0),
		keyNew("user",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if (strcmp (ret, "") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/some/thing",0),
		keyNew("system/other/thing",0), KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 7, "could find correct parentname");
	succeed_if (strcmp (ret, "system") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/here/in/deep/goes/ok/thing",0),
		keyNew("system/here/in/deep/goes/ok/other/thing",0),
		KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if (strcmp (ret, "system/here/in/deep/goes/ok") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("system/here/in/deep/goes/ok/thing",0),
		keyNew("system/here/in/deep/goes/ok/other/thing",0),
		keyNew("user/unique/thing",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) == 0, "could find correct parentname");
	succeed_if (strcmp (ret, "") == 0, "parentname not empty");
	ksDel (ks);

	ks = ksNew (10,
		keyNew("user/unique/thing",0),KS_END);
	succeed_if (ksGetCommonParentName(ks, ret, MAX_SIZE) > 0, "could find correct parentname");
	succeed_if (strcmp (ret, "user/unique/thing") == 0, "parentname not empty");
	ksDel (ks);
}

void test_ksAppend()
{
	int i;

	printf ("Test appending keys\n");

	KeySet *returned =
#include "data_keyset.c"
	KeySet *testDirectBelow =
#include "data_dbelow.c"
	KeySet *testReturned =
#include "data_others.c"
	Key *parentKey[2];
	parentKey[0] = keyNew ("user/test/keyset", KEY_END);
	parentKey[1] = keyNew ("user/test/keyset/dir1", KEY_END);
	Key *current;

	/* A real world example out in kdb.c */
	for (i=0; i<2; i++)
	{
		KeySet *tmp = ksNew(ksGetSize(returned), KS_END);
		KeySet *keys = ksNew (0);

		/* add all keys direct below parentKey */
		ksRewind (returned);
		while ((current = ksPop(returned)) != 0)
		{
			if (keyIsDirectBelow(parentKey[i], current))
			{
				ksAppendKey(keys, current);
			} else {
				ksAppendKey(tmp, current);
			}
		}
		ksAppend (returned, tmp);

		/*
		ksOutput (tmp, stdout, KDB_O_HEADER);
		ksOutput (returned, stdout, KDB_O_HEADER);
		printf (" ----- keys -------\n");
		ksOutput (keys, stdout, KDB_O_HEADER);
		*/

		if (!i)
		{
			compare_keyset (returned, testReturned);
			compare_keyset (keys, testDirectBelow);

			succeed_if (ksGetSize (tmp) == 84, "size not correct");
			succeed_if (ksGetSize (returned) == 84, "size not correct");
			succeed_if (ksGetSize (keys) == 18, "size not correct");

			succeed_if (ksGetAlloc (tmp) == 102, "alloc not correct");
			succeed_if (ksGetAlloc (returned) == 127, "alloc not correct");
			succeed_if (ksGetAlloc (keys) == 31, "alloc not correct");
		}

		ksAppend (returned, keys); /* add the keys back */

		ksDel (tmp);
		ksDel (keys);

	}

	keyDel (parentKey[0]);
	keyDel (parentKey[1]);

	ksDel (testReturned);
	ksDel (testDirectBelow);
	ksDel (returned);

	KeySet * ks = ksNew(0);
	ksAppendKey(ks, keyNew ("user/abc", KEY_META, "xyz", "egh", KEY_END));

	KeySet * other = ksNew(0);
	ksAppend (other, ks);
	compare_keyset(ks, other);
	compare_keyset(ks, ks);

	ksDel (other);
	ksDel (ks);
}



/**A functional mode to keys.
 *
 * Instead of writing your own loop you can write
 * a function working with a key and pass it to
 * this method.
 *
 * The function will be executed for all keys in
 * the keyset.
 *
 * @param ks the keyset to work with
 * @param func the function to execute on every key of the keyset
 * @return the sum of all return values
 * @return -1 if any function returned -1, the execution will stop there, but
 * 	ksCurrent() will tell you where it stopped.
 * @see ksFilter()
 */
int ksForEach (KeySet *ks, int (*func) (Key *k))
{
	int rc = 0;
	int ret = 0;
	Key *current;

	cursor_t cursor = ksGetCursor (ks);
	ksRewind (ks);
	while ((current = ksNext (ks)) != 0)
	{
		rc = func (current);
		if (rc == -1) return -1;
		ret += rc;
	}
	ksSetCursor(ks, cursor);
	return ret;
}


/**Filter a keyset.
 *
 * filter is executed for every key in the keyset result. When it returns 0,
 * the key will be dropped, when it returns 1 it will be ksAppendKey()ed to result,
 * when it returns -1 the processing will be stopped. You can use ksCurrent()
 * on input to see where the problem was. Because of this input is not const,
 * apart from ksCurrent() the input will not be changed. The keys that have
 * been in result before will stay untouched.
 *
 * @param result is the keyset where keys are added.
 * @param input is the keyset the filter works on.
 * @param filter is the function to execute on every key of the keyset to decide if
 * 	it should be ksAppendKey()ed to the result.
 * @return the number of keys added on success
 * @return 0 when nothing was done
 * @return -1 when filter returned an error (-1), ksCurrent() of input will
 * 	be the problematic key.
 * @see ksForEach()
 **/
int ksFilter (KeySet *result, KeySet *input, int (*filter) (Key *k))
{
	int rc = 0;
	int ret = 0;
	Key *current;

	cursor_t cursor = ksGetCursor (input);
	ksRewind (input);
	while ((current = ksNext (input)) != 0)
	{
		rc = filter (current);
		if (rc == -1) return -1;
		else if (rc != 0)
		{
			++ ret;
			ksAppendKey(result, keyDup (current));
		}
	}
	ksSetCursor(input, cursor);
	return ret;
}


Key *global_a;

int add_string (Key *check) { return keySetString (check, "string"); }
int add_comment (Key *check) { return keySetComment (check, "comment"); }
int has_a (Key *check) { return keyName(check)[5]=='a'; }
int below_a (Key *check) { return keyIsBelow(global_a, check); }
int direct_below_a (Key *check) { return keyIsDirectBelow(global_a, check); }

int sum_helper (Key *check) { return atoi(keyValue(check)); }
int below_30 (Key *check) { return atoi(keyValue(check))<30; }
int find_80 (Key *check) { int n=atoi(keyValue(check)); return n>70?-1:1; }

void test_ksFunctional()
{
	Key *found;
	Key *current;
	KeySet *out;
	KeySet *ks = ksNew (64,
		keyNew ("user/a/1", KEY_END),
		keyNew ("user/a/2", KEY_END),
		keyNew ("user/a/b/1", KEY_END),
		keyNew ("user/a/b/2", KEY_END),
		keyNew ("user/ab/2", KEY_END),
		keyNew ("user/b/1", KEY_END),
		keyNew ("user/b/2", KEY_END),
		KS_END);
	global_a = keyNew ("user/a", KEY_END);

	printf ("Test functional style\n");

	ksForEach (ks, add_string);
	ksForEach (ks, add_comment);

	ksRewind (ks);
	while ((current = ksNext(ks)) != 0)
	{
		succeed_if (strcmp (keyValue (current), "string") == 0, "for each did not add string");
		succeed_if (strcmp (keyComment (current), "comment") == 0, "for each did not add comment");
	}

	out = ksNew (0);
	succeed_if (ksGetSize(ks) == 7, "initial size wrong");
	succeed_if (ksGetSize(out) == 0, "initial size wrong");
	ksFilter (out, ks, has_a);
	succeed_if (ksGetSize(out) == 5, "has_a cutted more then the user/b");
	ksDel (out);

	out = ksNew (0);
	ksFilter (out, ks, below_a);
	succeed_if (ksGetSize(out) == 4, "below_a cutted more then the user/ab/2");
	ksDel (out);

	out = ksNew (0);
	ksFilter (out, ks, direct_below_a);
	succeed_if (ksGetSize(out) == 2, "direct_below_a cutted more then the user/a/b/*");
	ksDel (out);

	ksDel (ks);
	keyDel (global_a); global_a = 0;

	KeySet *values = ksNew (64,
		keyNew ("user/a", KEY_VALUE, "40", KEY_END),
		keyNew ("user/b", KEY_VALUE, "20", KEY_END),
		keyNew ("user/c", KEY_VALUE, "80", KEY_END),
		keyNew ("user/d", KEY_VALUE, "24", KEY_END),
		keyNew ("user/e", KEY_VALUE, "32", KEY_END),
		keyNew ("user/f", KEY_VALUE, "12", KEY_END),
		keyNew ("user/g", KEY_VALUE, "43", KEY_END),
		KS_END);

	succeed_if (ksForEach (values, sum_helper) == 251, "could not sum up");

	KeySet *values_below_30 = ksNew (0);
	ksFilter (values_below_30, values, below_30);
	succeed_if (ksGetSize (values_below_30) == 3, "could not filter out everything above 30");
	succeed_if (ksForEach (values_below_30, sum_helper) == 56, "could not sum up");

	succeed_if (ksForEach (values, find_80) == -1, "did not find 80");
	found = ksCurrent (values);
	succeed_if (ksLookupByName (values, "user/c", 0) == found, "did not find 80");
	/*succeed_if (ksLookupByString (values, "80", 0) == found, "lookup by value did not find 80");*/
	ksDel (values);
	ksDel (values_below_30);
}

void test_ksLookupPop()
{
	printf ("Test ksLookup with KDB_O_POP\n");

	Key * found;
	Key *a, *b, *c;
	KeySet *small =ksNew (5,
			a=keyNew ("user/a", KEY_END),
			b=keyNew ("user/b", KEY_END),
			c=keyNew ("user/c", KEY_END), KS_END);

	ksRewind (small);
	ksNext(small);
	succeed_if (ksCurrent(small) == a, "current not set correctly");

	succeed_if (ksGetSize(small) == 3, "could not append all keys");
	found = ksLookupByName (small, "user/a", KDB_O_POP);
	succeed_if (found == a, "not correct key");
	succeed_if (strcmp (keyName(found), "user/a") == 0, "name not correct in found key");
	succeed_if (ksCurrent(small) == 0, "current not set correctly");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksNext(small);
	ksNext(small);
	succeed_if (ksCurrent(small) == c, "current not set correctly");

	succeed_if (ksGetSize(small) == 2, "could not append all keys");
	found = ksLookupByName (small, "user/b", KDB_O_POP);
	succeed_if (found == b, "not correct key");
	succeed_if (strcmp (keyName(found), "user/b") == 0, "name not correct in found key");
	succeed_if (ksCurrent(small) == 0, "current not set correctly");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	succeed_if (ksGetSize(small) == 1, "could not append all keys");
	found = ksLookupByName (small, "user/b", KDB_O_POP);
	succeed_if (found == 0, "found something, but shouldnt");
	succeed_if (ksCurrent(small) == 0, "current not set correctly");

	succeed_if (ksGetSize(small) == 1, "could not append all keys");
	found = ksLookupByName (small, "user/c", KDB_O_POP);
	succeed_if (found == c, "not correct key");
	succeed_if (strcmp (keyName(found), "user/c") == 0, "name not correct in found key");
	succeed_if (ksCurrent(small) == 0, "current not set correctly");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	succeed_if (ksGetSize(small) == 0, "could not append all keys");
	found = ksLookupByName (small, "user/d", KDB_O_POP);
	succeed_if (found == 0, "found something, but shouldnt");
	succeed_if (ksCurrent(small) == 0, "current not set correctly");

	ksDel (small);

	KeySet *ks= ksNew(0);

	ksAppendKey(ks, keyNew("user/domain/key",  KEY_VALUE, "domainvalue",
		KEY_OWNER, "markus", KEY_END));
	ksAppendKey(ks, keyNew("user/single/key",  KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey(ks, keyNew("system/named/skey", KEY_VALUE, "syskey",  KEY_END));
	ksAppendKey(ks, keyNew("system/sysonly/key", KEY_VALUE, "sysonlykey",  KEY_END));
	ksAppendKey(ks, keyNew("user/named/bin", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "binary\1\2data", KEY_END));
	ksAppendKey(ks, keyNew("system/named/bin", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "sys\1bin\2", KEY_END));
	ksAppendKey(ks, keyNew("system/named/key", KEY_BINARY, KEY_SIZE, 10,
		KEY_VALUE, "syskey", KEY_END));
	succeed_if (ksGetSize(ks) == 8, "could not append all keys");

	// a positive testcase
	found = ksLookupByName (ks, "user/named/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 7, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksAppendKey(ks, keyNew("user/named/key",  KEY_VALUE, "singlevalue", KEY_END));
	succeed_if (ksGetSize(ks) == 8, "did not append key");

	// here you can't find the keys
	succeed_if (ksLookupByName (ks, "named/key", KDB_O_POP) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "u/named/key", KDB_O_POP) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "usea/named/key", KDB_O_POP) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, " user/named/key", KDB_O_POP) == 0, "found key with bad prefix");

	succeed_if (ksLookupByName (ks, "user/named/Key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User/Named/key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User/named/key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "user/NAMED/key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "USER/NAMED/KEY", KDB_O_POP) == 0, "found wrong case key");
	
	succeed_if (ksLookupByName (ks, "user/named/keys", KDB_O_POP) == 0, "wrong postfix");
	succeed_if (ksLookupByName (ks, "user/named/key_", KDB_O_POP) == 0, "wrong postfix");

	succeed_if (ksLookupByName (ks, "user/named/k/ey", KDB_O_POP) == 0, "seperation that should be");
	succeed_if (ksLookupByName (ks, "user/na/med/key", KDB_O_POP) == 0, "seperation that should be");

	// a positive testcase
	found = ksLookupByName (ks, "user/named/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 7, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksAppendKey(ks, keyNew("user/named/otherkey",  KEY_VALUE, "singlevalue", KEY_END));
	succeed_if (ksGetSize(ks) == 8, "did not append key");

	succeed_if (ksLookupByName (ks, "system/domain/key", KDB_O_POP) == 0, "found key in wrong domain");

	//now try to find them, and compare value
	found = ksLookupByName (ks, "user/domain/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 7, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "user/domain/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "domainvalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "user/single/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 6, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "user/single/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "system/named/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 5, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "system/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "syskey") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "user/named/bin", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not found correct name");
	succeed_if (strcmp (keyName(found), "user/named/bin") == 0, "name not correct in found key");
	succeed_if (strncmp (keyValue(found), "binary\1\2data",10) == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "user/named/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found == 0, "could find same key again");

	printf ("Test nocase lookup functions\n");
	found = ksLookupByName (ks, "user/named/key", KDB_O_NOCASE | KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found == 0, "could find same key again, nocase");

	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	found = ksLookupByName (ks, "user/NameD/KeY", KDB_O_NOCASE | KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "did not pop key");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	found = ksLookupByName (ks, "user/NameD/KEY", KDB_O_NOCASE | KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "did not pop key");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	// cascading

	ksAppendKey(ks, keyNew("user/named/key",   KEY_VALUE, "myvalue", KEY_END));
	printf ("Test cascading lookup functions\n");
	found = ksLookupByName (ks, "/named/key", KDB_O_POP);
	succeed_if (ksGetSize(ks) == 4, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if (strcmp (keyName(found), "user/named/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "myvalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "/named/Otherkey", KDB_O_NOCASE | KDB_O_POP);
	succeed_if (ksGetSize(ks) == 3, "did not pop key");
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "user/named/otherkey") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "singlevalue") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "/sysonly/key", KDB_O_POP);
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (ksGetSize(ks) == 2, "did not pop key");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if (strcmp (keyName(found), "system/sysonly/key") == 0, "name not correct in found key");
	succeed_if (strcmp (keyValue(found), "sysonlykey") == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	succeed_if (ksLookupByName (ks, "/named/", KDB_O_POP) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "/named/keyd", KDB_O_POP) == 0, "found part of key with cascading, bad postfix");

	ksAppendKey(ks, keyNew("user:markus/domain/key",   KEY_VALUE, "domainvalue", KEY_END));
	printf ("Test domain lookup functions\n");
	found = ksLookupByName (ks, "user:markus/domain/key", KDB_O_WITHOWNER | KDB_O_POP);
	succeed_if (ksCurrent(ks) == 0, "current not set correctly");
	succeed_if (ksGetSize(ks) == 2, "did not pop key");
	succeed_if (found != 0, "could not find domain key");
	succeed_if (strcmp (keyValue(found), "domainvalue") == 0, "not correct value in domain key");
	succeed_if (ksLookupByName (ks, "user:hugo/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:y/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:markuss/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksLookupByName (ks, "user:marku/domain/key", KDB_O_WITHOWNER | KDB_O_POP) == 0, "found key in wrong domain");
	succeed_if (ksGetSize(ks) == 2, "did not pop key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksDel(ks);
	
}

void test_ksSync()
{
	printf ("Test sync flag of KeySet\n");

	KeySet *ks;
	Key *key;

	ks = ksNew(0);
	succeed_if (ksNeedSync(ks) == 0, "need sync after creation");

	keyDel (ksPop (ks));
	succeed_if (ksNeedSync(ks) == 1, "need sync after pop");
	ksDel (ks);


	ks = ksNew(0);
	succeed_if (ksNeedSync(ks) == 0, "need sync after creation");

	ksAppendKey (ks, keyNew ("user/key", KEY_END));
	succeed_if (ksNeedSync(ks) == 0, "need sync after new key");

	keyDel (ksPop (ks));
	succeed_if (ksNeedSync(ks) == 1, "need sync after new key");
	ksDel (ks);


	ks = ksNew(0);
	succeed_if (ksNeedSync(ks) == 0, "need sync after creation");

	ksAppendKey (ks, keyNew ("user/key", KEY_END));
	succeed_if (ksNeedSync(ks) == 0, "need sync after new key");

	ksLookupByName(ks, "user/key", 0);
	succeed_if (ksNeedSync(ks) == 0, "need sync after new key");

	key = ksLookupByName(ks, "user/key", KDB_O_POP);
	succeed_if (ksNeedSync(ks) == 1, "need sync after new key");
	keyDel (key);

	ksDel (ks);
}

void test_ksDoubleFree()
{
	/* Valgrind only test */
	KeySet *ks1 = ksNew (5,
		keyNew ("user/abc1", KEY_VALUE, "abc1", KEY_END),
		keyNew ("user/abc2", KEY_VALUE, "abc1", KEY_END),
		keyNew ("user/abc3", KEY_VALUE, "abc1", KEY_END),
		KS_END);

	KeySet *ks2 = ksNew (5,
		keyNew ("user/abc1", KEY_VALUE, "abc2", KEY_END),
		keyNew ("user/abc2", KEY_VALUE, "abc2", KEY_END),
		keyNew ("user/abc3", KEY_VALUE, "abc2", KEY_END),
		KS_END);

	Key *cur;
	ksRewind (ks1);
	while ((cur = ksNext(ks1)) != 0)
	{
		ksAppendKey (ks2, cur);
	}

	ksDel (ks1);
	ksDel (ks2);
}

void test_ksDoubleAppend()
{
	printf ("Test double appending\n");

	KeySet *ks1 = ksNew (5,
		keyNew ("user/abc1", KEY_VALUE, "abc1", KEY_END),
		keyNew ("user/abc2", KEY_VALUE, "abc1", KEY_END),
		keyNew ("user/abc3", KEY_VALUE, "abc1", KEY_END),
		KS_END);

	KeySet *ks2 = ksNew (5,
		keyNew ("user/abc1", KEY_VALUE, "abc2", KEY_END),
		keyNew ("user/abc2", KEY_VALUE, "abc2", KEY_END),
		keyNew ("user/abc3", KEY_VALUE, "abc2", KEY_END),
		KS_END);

	ksAppend (ks1, ks2);
	succeed_if (ksGetSize (ks1) == 3, "size not correct");
	succeed_if (ksGetSize (ks2) == 3, "size not correct");

	ksDel (ks1);
	ksDel (ks2);
}

void test_ksDoubleAppendKey()
{
	printf ("Test double appending of key\n");

	Key *k = keyNew("user/my_double_key", KEY_END);
	KeySet *ks = ksNew(0);

	ksAppendKey (ks, k);
	succeed_if (ksGetSize (ks) == 1, "size not correct");

	ksAppendKey (ks, k);
	succeed_if (ksGetSize (ks) == 1, "size not correct");

	ksAppendKey (ks, k);
	succeed_if (ksGetSize (ks) == 1, "size not correct");

	keyDel (k); // has no effect

	ksDel (ks);
	// don't free key here!!
}

void test_ksAppendKey()
{
	printf ("Test cursor after appending key\n");
	KeySet *ks=0;
	Key *cur;

	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/a", KEY_END)) == 1, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksAppendKey(ks,cur=keyNew("user/b", KEY_END)) == 2, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksAppendKey(ks,cur=keyNew("user/x", KEY_END)) == 3, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksGetSize(ks) == 3, "size not correct after 3 keys");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/b", KEY_END)) == 3, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position (same key)");
	succeed_if (ksGetSize(ks) == 3, "size not correct after double append");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/0", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position (front key)");
	succeed_if (ksGetSize(ks) == 4, "size not correct after 4 keys");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/c", KEY_END)) == 5, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position (key in between)");
	succeed_if (ksGetSize(ks) == 5, "size not correct after 5 keys");

	ksDel (ks);

	exit_if_fail ((ks=ksNew(0)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey(ks,cur=keyNew("user", KEY_END)) == 1, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksGetSize(ks) == 1, "size not correct after 1 keys");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/tests", KEY_END)) == 2, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksGetSize(ks) == 2, "size not correct after 2 keys");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/tests/folder", KEY_END)) == 3, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksGetSize(ks) == 3, "size not correct after 3 keys");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/tests/folder/bool_key", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (ksGetSize(ks) == 4, "size not correct after 4 keys");

	Key *newKey = keyDup(cur);
	keySetBaseName(newKey, "second_bool_key");

	succeed_if (ksAppendKey(ks, newKey) == 5, "could not append a key");
	succeed_if (ksCurrent(ks) == newKey, "did not update current position");
	succeed_if (ksGetSize(ks) == 5, "size not correct after 5 keys");
	ksDel (ks);
}

void test_ksModifyKey()
{
	// TODO: broken, it is allowed to change keyname!
	printf ("Test modify key after insertion\n");

	KeySet *ks=0;
	Key *cur;

	exit_if_fail((ks=ksNew(0)) != 0, "could not create new keyset");

	succeed_if (ksAppendKey(ks,cur=keyNew("user/a", KEY_END)) == 1, "could not append a key");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");
	succeed_if (keySetName (cur, "user/b") == -1, "set name with appended key should be disallowed");
	succeed_if (ksCurrent(ks) == cur, "did not update current position");

	ksDel (ks);
}

void test_ksPopAtCursor()
{
	KeySet *ks = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *ks_c = ksNew (
		5,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		KS_END);
	ksRewind(ks);
	ksNext(ks);
	ksNext(ks);
	cursor_t c = ksGetCursor(ks);
	keyDel (ksPopAtCursor(ks, c));
	succeed_if(ksCurrent(ks) == 0, "cursor position wrong");

	compare_keyset(ks, ks_c);
	ksDel(ks);
	ksDel(ks_c);
}

void test_ksToArray()
{
	KeySet *ks = ksNew (5,
			keyNew ("user/test1", KEY_END),
			keyNew ("user/test2", KEY_END),
			keyNew ("user/test3", KEY_END),
	KS_END);

	Key **keyArray = calloc (ksGetSize (ks), sizeof (Key *));
	elektraKsToMemArray(ks, keyArray);

	succeed_if (!strcmp ("user/test1", keyName(keyArray[0])), "first key in array incorrect");
	succeed_if (!strcmp ("user/test2", keyName(keyArray[1])), "second key in array incorrect");
	succeed_if (!strcmp ("user/test3", keyName(keyArray[2])), "third key in array incorrect");

	/* test if cursor is restored */
	ksNext(ks);
	cursor_t cursor = ksGetCursor(ks);
	elektraKsToMemArray(ks, keyArray);

	succeed_if (ksGetCursor(ks) == cursor, "cursor was not restored");

	succeed_if (elektraKsToMemArray(0, keyArray) < 0, "wrong result on null pointer");
	succeed_if (elektraKsToMemArray(ks, 0) < 0, "wrong result on null buffer");
	KeySet *empty = ksNew(0);
	succeed_if (elektraKsToMemArray(empty, keyArray) == 0, "wrong result on empty keyset");
	ksDel(empty);

	free (keyArray);
	ksDel (ks);
}

void test_keyCmpOrder()
{
	Key *k1 = keyNew ("user/a", KEY_META, "order", "20", KEY_END);
	Key *k2 = keyNew ("user/b", KEY_META, "order", "10", KEY_END);

	succeed_if (elektraKeyCmpOrder(0, 0) == 0, "null keys are not equal");
	succeed_if (elektraKeyCmpOrder(k1, 0) == 1, "not null key is not greater than null key");
	succeed_if (elektraKeyCmpOrder(0, k1) == -1, "null key is not smaller than not null key");

	succeed_if (elektraKeyCmpOrder(k1, k2) > 0, "user/a is not greater than user/b");
	succeed_if (elektraKeyCmpOrder(k2, k1) < 0, "user/b is not smaller than user/a");

	keySetMeta(k2, "order", "20");
	succeed_if (elektraKeyCmpOrder(k1, k2) == 0, "keys with same order are not equal");
	succeed_if (elektraKeyCmpOrder(k2, k1) == 0, "keys with same order are not equal");

	keySetMeta(k2, "order", 0);
	succeed_if (elektraKeyCmpOrder(k1, k2) > 0, "key with metadata is not greater than key without");
	succeed_if (elektraKeyCmpOrder(k2, k1) < 0, "key with metadata is not greater than key without");

	keySetMeta(k1, "order", 0);
	succeed_if (elektraKeyCmpOrder(k1, k2) == 0, "keys without metadata are not equal");
	succeed_if (elektraKeyCmpOrder(k2, k1) == 0, "keys without metadata are not equal");

	keyDel (k1);
	keyDel (k2);
}

void test_ksOrder()
{
	KeySet* ks = ksNew(20,
		keyNew("user/test/test", KEY_END),
		keyNew("user/test/test/bar", KEY_END),
		keyNew("user/test/test/foo", KEY_END),
		keyNew("user/test/test-foo", KEY_END),
		KS_END);
	ksRewind(ks);
	ksNext(ks);
	succeed_if_same_string(keyName(ksCurrent(ks)), "user/test/test");
	ksNext(ks);
	succeed_if_same_string(keyName(ksCurrent(ks)), "user/test/test/bar");
	ksNext(ks);
	succeed_if_same_string(keyName(ksCurrent(ks)), "user/test/test/foo");
	ksNext(ks);
	succeed_if_same_string(keyName(ksCurrent(ks)), "user/test/test-foo");
	succeed_if(0, "does not succeed");
}

int main(int argc, char** argv)
{
	printf("KEYSET       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_ksNew();
	test_ksEmpty();
	test_ksReference();
	test_ksDup();
	test_ksCopy();
	// test_ksResize();
	test_ksIterate();
	test_ksCursor();
	test_ksAtCursor();
	test_ksSort();
	test_ksLookup();
	test_ksLookupByName();
	test_ksLookupName();
	test_ksLookupNameDomain();
	test_ksLookupNameAll();
	test_ksLookupNameCascading();
	//test_ksLookupValue();
	test_ksExample();
	test_ksCommonParentName();
	test_ksAppend();
	test_ksFunctional();
	test_ksLookupPop();
	test_ksSync();
	test_ksDoubleFree();
	test_ksDoubleAppend();
	test_ksDoubleAppendKey();
	test_ksAppendKey();
	test_ksPopAtCursor();
	test_ksToArray();
	test_keyCmpOrder();
	// test_ksModifyKey(); // TODO: Bug, not handled correctly
	// test_ksOrder(); // TODO: Bug, not handled correctly

	printf("\ntest_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

