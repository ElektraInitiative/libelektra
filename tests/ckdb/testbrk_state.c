/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <tests.h>

static void test_kdbOpenClose()
{
	printf ("Test open close\n");

	KeySet *myConfig = ksNew(0, KS_END);
	Key *parentKey = keyNew(ELEKTRA_TEST_ROOT, KEY_CASCADING_NAME, KEY_END);
	KDB *handle = kdbOpen(parentKey);

	succeed_if(handle, "got no handle");

	ksDel (myConfig);

	kdbClose(handle, parentKey);
	keyDel(parentKey);

}

static void test_kdbOpenGetClose()
{
	printf ("Test open get close\n");

	KeySet *myConfig = ksNew(0, KS_END);
	Key *parentKey = keyNew(ELEKTRA_TEST_ROOT, KEY_CASCADING_NAME, KEY_END);
	KDB *handle = kdbOpen(parentKey);

	exit_if_fail(handle, "got no handle");

	succeed_if(kdbGet(handle, myConfig, parentKey) >= 0, "get failed");
	succeed_if(kdbSet(handle, myConfig, parentKey) >= 0, "set failed");

	ksDel (myConfig);

	kdbClose(handle, parentKey);
	keyDel(parentKey);
}

static void test_kdbWrongState()
{
	printf ("Test wrong state\n");

	KeySet *myConfig = ksNew(0, KS_END);
	Key *parentKey = keyNew(ELEKTRA_TEST_ROOT, KEY_CASCADING_NAME, KEY_END);
	KDB *handle = kdbOpen(parentKey);

	succeed_if(kdbSet(handle, myConfig, parentKey) == 0, "empty keyset, nothing to do");
	ksAppendKey(myConfig, keyNew("user" ELEKTRA_TEST_ROOT "akey", KEY_END));
	succeed_if(kdbSet(handle, myConfig, parentKey) == 1, "could set even though its wrong state"); // TODO: should be -1?
	output_warnings(parentKey);
	output_error(parentKey);

	ksDel (myConfig);

	kdbClose(handle, parentKey);
	keyDel(parentKey);
}

static void test_kdbOpenGetSetClose()
{
	printf ("Test open get set close\n");

	KeySet *myConfig = ksNew(0, KS_END);
	Key *parentKey = keyNew(ELEKTRA_TEST_ROOT, KEY_CASCADING_NAME, KEY_END);
	KDB *handle = kdbOpen(parentKey);
	exit_if_fail(handle, "got no handle");

	succeed_if(kdbGet(handle, myConfig, parentKey) >= 0, "get failed");
	ksAppendKey(myConfig, keyNew("user" ELEKTRA_TEST_ROOT "akey", KEY_END));
	succeed_if(kdbSet(handle, myConfig, parentKey) == 1, "set failed");

	kdbClose(handle, parentKey);
	ksDel (myConfig);

	myConfig = ksNew(0, KS_END);
	handle = kdbOpen(parentKey);
	exit_if_fail(handle, "got no handle");

	succeed_if(kdbGet(handle, myConfig, parentKey) == 1, "get failed");
	Key *k = ksLookupByName(myConfig, ELEKTRA_TEST_ROOT "akey", KDB_O_POP);
	succeed_if(k, "could not get key set before");
	keyDel(k);
	succeed_if(kdbSet(handle, myConfig, parentKey) == 1, "set failed");

	kdbClose(handle, parentKey);
	ksDel (myConfig);

	myConfig = ksNew(0, KS_END);
	handle = kdbOpen(parentKey);
	exit_if_fail(handle, "got no handle");

	succeed_if(kdbGet(handle, myConfig, parentKey) == 1, "get failed");
	k = ksLookupByName(myConfig, ELEKTRA_TEST_ROOT "akey", 0);
	succeed_if(!k, "got removed key");
	keyDel(k);
	succeed_if(kdbSet(handle, myConfig, parentKey) == 0, "set failed/was something to do");

	kdbClose(handle, parentKey);

	ksDel (myConfig);
	keyDel(parentKey);
}


int main(int argc, char** argv)
{
	printf("KDB STATE  TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_kdbOpenClose();
	test_kdbOpenGetClose();
	test_kdbWrongState();
	test_kdbOpenGetSetClose();

	printf("\ntestckdb_state RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
