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

/*Needs private declarations*/
#include <kdbbackend.h>

void test_create()
{
	Split *split;
	split = malloc (sizeof (Split));
	split->keysets = 0;
	split->handles = 0;
	elektraSplitInit (split);


	printf ("Test create and resize split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");

	elektraSplitResize (split);
	split->keysets[0] = ksNew(0);
	succeed_if (split->no == 1, "resize not correct");
	elektraSplitResize (split);
	split->keysets[1] = ksNew(0);
	succeed_if (split->no == 2, "resize not correct");
	elektraSplitResize (split);
	split->keysets[2] = ksNew(0);
	succeed_if (split->no == 3, "resize not correct");

	elektraSplitClose (split);
}


void test_emptysplit()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (0);
	Key *parentKey = 0;
	unsigned long options = 0;
	Split *split = elektraSplitKeySet (handle, ks, parentKey, options);

	printf ("Test empty split\n");
	succeed_if (split->no == 0, "empty requires no data");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");

	elektraSplitClose (split);
	ksDel (ks);
	kdbClose (handle);
}

void test_easysplit()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (
		3,
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	Key *parentKey = 0;
	unsigned long options = 0;
	Split *split = elektraSplitKeySet (handle, ks, parentKey, options);

	printf ("Test easy split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 1, "everything is in one keyset");
	compare_keyset (split->keysets[0], ks, 0, 0);

	elektraSplitClose (split);
	ksDel (ks);
	kdbClose (handle);
}

void test_singlesplit()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (
		3,
		keyNew ("user/valid/key", KEY_END),
		KS_END);
	Key *parentKey = 0;
	unsigned long options = 0;
	Split *split = elektraSplitKeySet (handle, ks, parentKey, options);

	printf ("Test single split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 1, "everything is in one keyset");
	compare_keyset (split->keysets[0], ks, 0, 0);

	elektraSplitClose (split);
	ksDel (ks);
	kdbClose (handle);
}

void test_mount()
{
	KDB *handle = kdbOpen();
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
	Key *parentKey = 0;
	unsigned long options = 0;
	Key *mnt;
	KeySet *config;
	Split *split;

	kdbMount (handle, mnt=keyNew ("user", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);
	kdbMount (handle, mnt=keyNew ("system", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);

	split = elektraSplitKeySet (handle, ks, parentKey, options);
	/*ksOutput (split->keysets[0], stdout, KDB_O_HEADER);*/


	printf ("Test mount split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->syncbits[0] == 1, "user part need to by synced");
	succeed_if (split->syncbits[1] == 1, "system part need to by synced");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	kdbClose (handle);
}

void test_optimize()
{
	KDB *handle = kdbOpen();
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
	Key *parentKey = 0;
	unsigned long options = 0;
	Key *mnt;
	KeySet *config;
	Split *split;
	Key *key;

	kdbMount (handle, mnt=keyNew ("user", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);
	kdbMount (handle, mnt=keyNew ("system", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);

	ksRewind (ks);
	while ((key = ksNext(ks)) != 0)
	{
		if (keyIsUser(key) == 1) key->flags &= ~KEY_FLAG_SYNC;
	}

	split = elektraSplitKeySet (handle, ks, parentKey, options);


	printf ("Test optimization split\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 0, "user part need to by synced");
	succeed_if (split->syncbits[1] == 1, "system part not optimized");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);


	ksRewind (ks);
	while ((key = ksNext(ks)) != 0)
	{
		key->flags = 0;
	}

	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 0, "user part not optimized");
	succeed_if (split->syncbits[1] == 0, "system part not optimized");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	options = KDB_O_SYNC;
	elektraSplitClose (split);

	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user part ignoresync");
	succeed_if (split->syncbits[1] == 1, "system part ignoresync");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);


	ksRewind (ks);
	while ((key = ksNext(ks)) != 0)
	{
		key->flags = KEY_FLAG_SYNC;
	}

	split = elektraSplitKeySet (handle, ks, parentKey, options);

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "optimized too much");
	succeed_if (split->syncbits[1] == 1, "optimized too much");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);


	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	kdbClose (handle);
}

void test_removed()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (
		5,
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		3,
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		3,
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	Key *parentKey = 0;
	unsigned long options = 0;
	Key *mnt;
	KeySet *config;
	Split *split;
	Key *key;

	kdbMount (handle, mnt=keyNew ("user", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);
	kdbMount (handle, mnt=keyNew ("system", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);

	split = elektraSplitKeySet (handle, ks, parentKey, options);


	printf ("Test optimization split with removed keys\n");
	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[1] == 1, "user part need to be synced");
	succeed_if (split->syncbits[0] == 1, "second part need to by synced");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);


	ksRewind (ks);
	while ((key = ksNext(ks)) != 0)
	{
		/*only removed, no sync*/
		if (keyIsUser(key) == 1) key->flags &= ~KEY_FLAG_SYNC;
	}

	split = elektraSplitKeySet (handle, ks, parentKey, options);

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 0, "optimized too much");
	succeed_if (split->syncbits[1] == 1, "user part does not need to be synced");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);

	options = KDB_O_SYNC;
	split = elektraSplitKeySet (handle, ks, parentKey, options);

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[1] == 1, "user part ignoresync");
	succeed_if (split->syncbits[0] == 1, "optimized too much");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	kdbClose (handle);
}

void test_easyparent()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (
		8,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		5,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		5,
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	unsigned long options = 0;
	Key *parentKey;
	Key *mnt;
	KeySet *config;
	Split *split;

	printf ("Test parent separation of user and system\n");

	/*ksOutput (ks, stdout, 0);
	ksSort (ks);
	ksOutput (ks, stdout, 0);*/


	parentKey = keyNew ("user", KEY_END);
	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	/*printf ("%d\n", split->no);*/
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user part need to by synced");
	succeed_if (split->syncbits[1] == 1, "system part need to be synced");
	succeed_if (split->belowparents[0] == 1, "user part is below parent");
	succeed_if (split->belowparents[1] == 0, "system part is not below parent");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	keyDel (parentKey);

	parentKey = keyNew ("system", KEY_END);
	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user part need to by synced");
	succeed_if (split->syncbits[1] == 1, "system part need to be synced");
	succeed_if (split->belowparents[0] == 0, "user part is not below parent");
	succeed_if (split->belowparents[1] == 1, "system part is below parent");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	keyDel (parentKey);

	kdbMount (handle, mnt=keyNew ("user", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);
	kdbMount (handle, mnt=keyNew ("system", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);

	parentKey = keyNew ("user", KEY_END);
	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user part need to by synced");
	succeed_if (split->syncbits[1] == 1, "system part need to be synced");
	succeed_if (split->belowparents[0] == 1, "user part is below parent");
	succeed_if (split->belowparents[1] == 0, "system part is not below parent");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	keyDel (parentKey);

	parentKey = keyNew ("system", KEY_END);
	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	succeed_if (split->no == 2, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user part need to by synced");
	succeed_if (split->syncbits[1] == 1, "system part need to be synced");
	succeed_if (split->belowparents[0] == 0, "user part is not below parent");
	succeed_if (split->belowparents[1] == 1, "system part is below parent");
	succeed_if (compare_keyset (split->keysets[0], split1, 0, 0) == 0, "user keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	keyDel (parentKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
	kdbClose (handle);
}

void test_parent()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (
		18,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("user/invalid", KEY_END),
		keyNew ("user/invalid/key1", KEY_END),
		keyNew ("user/invalid/key2", KEY_END),
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split0 = ksNew (
		9,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		9,
		keyNew ("user/invalid", KEY_END),
		keyNew ("user/invalid/key1", KEY_END),
		keyNew ("user/invalid/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		9,
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	unsigned long options = 0;
	Key *parentKey;
	Key *mnt;
	KeySet *config;
	Split *split;

	printf ("Test parent separation of subpath\n");

	/*ksOutput (ks, stdout, 0);
	ksSort (ks);
	ksOutput (ks, stdout, 0);*/

	kdbMount (handle, mnt=keyNew ("user", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);
	kdbMount (handle, mnt=keyNew ("system", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);


	parentKey = keyNew ("user/valid", KEY_END);
	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	/*printf ("%d\n", split->no);*/
	succeed_if (split->no == 3, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user valid part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user invalid part need to by synced");
	succeed_if (split->syncbits[2] == 1, "system part need to be synced");
	succeed_if (split->belowparents[0] == 1, "user valid part is below parent");
	succeed_if (split->belowparents[1] == 0, "user invalid part is not below parent");
	succeed_if (split->belowparents[2] == 0, "system part is not below parent");
	succeed_if (compare_keyset (split->keysets[0], split0, 0, 0) == 0, "user valid keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split1, 0, 0) == 0, "user invalid keyset not correct");
	succeed_if (compare_keyset (split->keysets[2], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	keyDel (parentKey);

	ksDel (ks);
	ksDel (split0);
	ksDel (split1);
	ksDel (split2);
	kdbClose (handle);
}


void test_mountparent()
{
	KDB *handle = kdbOpen();
	KeySet *ks = ksNew (
		18,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		keyNew ("user/valid/mount", KEY_END),
		keyNew ("user/valid/mount/key1", KEY_END),
		keyNew ("user/valid/mount/key2", KEY_END),
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	KeySet *split0 = ksNew (
		9,
		keyNew ("user/valid", KEY_END),
		keyNew ("user/valid/key1", KEY_END),
		keyNew ("user/valid/key2", KEY_END),
		KS_END);
	KeySet *split1 = ksNew (
		9,
		keyNew ("user/valid/mount", KEY_END),
		keyNew ("user/valid/mount/key1", KEY_END),
		keyNew ("user/valid/mount/key2", KEY_END),
		KS_END);
	KeySet *split2 = ksNew (
		9,
		keyNew ("system/valid", KEY_END),
		keyNew ("system/valid/key1", KEY_END),
		keyNew ("system/valid/key2", KEY_END),
		KS_END);
	unsigned long options = 0;
	Key *parentKey;
	Key *mnt;
	KeySet *config;
	Split *split;

	printf ("Test parent separation with mounted backend below\n");

	/*ksOutput (ks, stdout, 0);
	ksSort (ks);
	ksOutput (ks, stdout, 0);*/

	kdbMount (handle, mnt=keyNew ("user/valid/mount", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);
	kdbMount (handle, mnt=keyNew ("system", KEY_VALUE, "filesys", KEY_END), config=ksNew(0));
	keyDel (mnt); ksDel (config);


	parentKey = keyNew ("user/valid", KEY_END);
	split = elektraSplitKeySet (handle, ks, parentKey, options);


	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");
	/*printf ("%d\n", split->no);*/
	succeed_if (split->no == 3, "not splitted according user, system");
	succeed_if (split->syncbits[0] == 1, "user valid part need to by synced");
	succeed_if (split->syncbits[1] == 1, "user invalid part need to by synced");
	succeed_if (split->syncbits[2] == 1, "system part need to be synced");
	succeed_if (split->belowparents[0] == 1, "user valid part is below parent");
	succeed_if (split->belowparents[1] == 1, "user mounted part is not below parent");
	succeed_if (split->belowparents[2] == 0, "system part is not below parent");
	succeed_if (compare_keyset (split->keysets[0], split0, 0, 0) == 0, "user valid keyset not correct");
	succeed_if (compare_keyset (split->keysets[1], split1, 0, 0) == 0, "user invalid keyset not correct");
	succeed_if (compare_keyset (split->keysets[2], split2, 0, 0) == 0, "system keyset not correct");

	elektraSplitClose (split);
	keyDel (parentKey);

	ksDel (ks);
	ksDel (split0);
	ksDel (split1);
	ksDel (split2);
	kdbClose (handle);
}



int main(int argc, char** argv)
{
	printf("SPLIT       TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_create();
	test_emptysplit();
	test_easysplit();
	test_singlesplit();
	test_mount();
	test_optimize();
	test_removed();
	test_easyparent();
	test_parent();
	test_mountparent();

	printf("\ntest_split RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

