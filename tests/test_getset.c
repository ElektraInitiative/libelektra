/***************************************************************************
 *          test_setget.c  -  setting and getting test suite
 *                -------------------
 *  begin                : Wed Aug 26 2006
 *  copyright            : (C) 2006 by Patrick Sabin
 *  email                : patricksabin@gmx.at
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/


#include <tests.h>

void test_subdir()
{
	KDB *kdb;
	Key *key,*key1,*key2;
	KeySet *ks;
	KeySet *returned;
	int key1_found;
	const char* name;
	Key *parent;

	printf("testing subdir\n");

	kdb=kdbOpen();
	exit_if_fail(kdb,"kdbOpen() failed");

	ks=ksNew(0);
	exit_if_fail(ks,"ksNew(0) failed");

	/* Test creating two keys */
	key=keyNew(KEY_ROOT "/test_d", KEY_DIR, KEY_VALUE, "dirkey", KEY_COMMENT, "dircomment", KEY_END);
	succeed_if(key!=NULL, "keyNew: Unable to create a new key: user/test");
	succeed_if(keySetString(key,"test value")!=0, "keySetString: Cannot set value");
	succeed_if(keySetComment(key,"empty")!=0, "keySetComment: Cannot set the key Comment");
	ksAppendKey(ks,key);

	key=keyNew(KEY_ROOT "/test_d/test", KEY_END);
	succeed_if(key!=NULL, "keyNew: Unable to create a new key: user/test/test");
	succeed_if(keySetString(key,"test value2")!=0, "keySetString: Cannot set value");
	succeed_if(keySetComment(key,"empty")!=0, "keySetComment: Cannot set the key Comment");
	ksAppendKey(ks,key);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "kdbSet failed");

	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
	exit_if_fail((kdb=kdbOpen())!=NULL,"Could not open elektra");
	
	// read the directory key

	succeed_if(ksDel(ks)==0,"Could not close keySet");
	ks=ksNew(0);

	returned=ksNew(0);
	parent=keyNew(KEY_ROOT "/",KEY_END);
	succeed_if (kdbGet(kdb,returned,parent,0)!=-1, "kdbGet failed");

	key1_found=0;
	for (key=ksNext(returned);key;key=ksNext(returned)) {
		name=keyName(key);
		if (!strcmp(name, KEY_ROOT "/test_d")) {
			key1_found=1;
			/*
			output_key(key);
			*/
			succeed_if(!strcmp(keyValue(key),"test value"),"value of directory key not correct");
			succeed_if(!strcmp(keyComment(key),"empty"),"comment of directory key not correct");
		}
	}
	succeed_if(key1_found, KEY_ROOT "/test1 not found");
	ksDel(returned);
	keyDel(parent);

	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
	exit_if_fail((kdb=kdbOpen())!=NULL,"Could not open elektra");

	// user/test and user/test/test keys are written. Now overwrite them

	key1=keyNew(KEY_ROOT "/test_d", KEY_DIR,  KEY_END);
	succeed_if(key1!=NULL, "keyNew: Unable to create a new key: user/test");
	succeed_if(keySetString(key1,"test value")!=0, "keySetString: Cannot set value");
	succeed_if(keySetComment(key1,"empty")!=0, "keySetComment: Cannot set the key Comment");
	ksAppendKey(ks,key1);

	key2=keyNew(KEY_ROOT "/test_d/test", KEY_END);
	succeed_if(key2!=NULL, "keyNew: Unable to create a new key: user/test/test");
	succeed_if(keySetString(key2,"test value2")!=0, "keySetString: Cannot set value");
	succeed_if(keySetComment(key2,"empty")!=0, "keySetComment: Cannot set the key Comment");
	ksAppendKey(ks,key2);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "kdbSet failed");

	keyRemove (key2);
	succeed_if(kdbSetKey(kdb,key2)==0,"Cannot remove key2");
	keyRemove (key1);
	succeed_if(kdbSetKey(kdb,key1)==0,"Cannot remove key1");
	succeed_if(ksDel(ks)==0,"Could not close keySet");
	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
}

void test_writeread()
{
	KDB *kdb;
	Key *parent,*key,*key1,*key2;
	KeySet *ks;
	KeySet *returned;
	const char *name;
	int key1_found,key2_found;
	//char buffer[1024];

	printf("testing write/read\n");
	kdb=kdbOpen();
	exit_if_fail(kdb,"kdbOpen() failed");

	ks=ksNew(0);
	exit_if_fail(ks,"ksNew(0) failed");

	/* Test creating two keys */
	key1=keyNew(KEY_ROOT "/test1", KEY_VALUE,"value1", KEY_END);
	succeed_if(key1!=NULL, "keyNew: Unable to create a new key: user/test");
	ksAppendKey(ks,key1);

	key2=keyNew(KEY_ROOT "/test2", KEY_VALUE,"value2",KEY_COMMENT,"comment2",KEY_END);
	succeed_if(key2!=NULL, "keyNew: Unable to create a new key: user/test2");
	ksAppendKey(ks,key2);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "kdbSet failed");
	succeed_if(ksDel(ks)==0,"Could not close keySet");

	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
	kdb=kdbOpen();

	/* Test reading the written keys */

	returned=ksNew(0);
	parent=keyNew(KEY_ROOT "/",KEY_END);

	succeed_if (kdbGet(kdb,returned,parent,0)!=-1, "could not get keys");

	key1_found=key2_found=0;
	for (key=ksNext(returned);key;key=ksNext(returned)) {
		name=keyName(key);
		if (!strcmp(name, KEY_ROOT "/test1")) {
			key1_found=1;
			/*
			output_key(key);
			*/
			succeed_if(!strcmp(keyValue(key),"value1"),"value of first written key not correct");
			succeed_if(!strcmp(keyComment(key),""),"comment of first written key not correct");
		}
		if (!strcmp(name, KEY_ROOT "/test2")) {
			key2_found=1;
			/*
			output_key(key);
			*/
			succeed_if(!strcmp(keyValue(key),"value2"),"value of second written key not correct");
			succeed_if(!strcmp(keyComment(key),"comment2"),"value of second written key not correct");
		}
	}
	succeed_if(key1_found, KEY_ROOT "/test1 not found");
	succeed_if(key2_found, KEY_ROOT "/test2 not found");
	ksDel(returned);
	keyDel(parent);

	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
	exit_if_fail((kdb=kdbOpen())!=NULL,"Could not open elektra");

	/* Test overwriting the keys */

	ks=ksNew(0);
	exit_if_fail(ks,"ksNew(0) failed");

	key1=keyNew(KEY_ROOT "/test1", KEY_VALUE,"value_new1",KEY_COMMENT,"comment1",KEY_END);
	succeed_if(key1!=NULL, "keyNew: Unable to create a new key: user/test");
	ksAppendKey(ks,key1);

	key2=keyNew(KEY_ROOT "/test2", KEY_VALUE,"value_new2",KEY_END);
	succeed_if(key2!=NULL, "keyNew: Unable to create a new key: user/test/test");
	ksAppendKey(ks,key2);

	succeed_if(kdbSet(kdb,ks,0,0)!=-1, "kdbSet failed");

	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
	exit_if_fail((kdb=kdbOpen())!=NULL,"Could not open elektra");

	/* Test reading the overwritten keys */

	returned=ksNew(0);
	parent=keyNew(KEY_ROOT "/",KEY_END);

	succeed_if (kdbGet(kdb,returned,parent,0)!=-1, "could not kdbGet keys");
	succeed_if (keyDel (parent) == 0, "Could not delete parent key");

	key1_found=key2_found=0;
	ksRewind(returned);
	while ((key=ksNext(returned))!=0)
	{
		name=keyName(key);
		if (!strcmp(name, KEY_ROOT "/test1")) {
			key1_found=1;
			succeed_if(!strcmp(keyValue(key),"value_new1"),"value of first overwritten key not correct");
			succeed_if(!strcmp(keyComment(key),"comment1"),"comment of first overwritten key not correct");
		}
		if (!strcmp(name, KEY_ROOT "/test2")) {
			key2_found=1;
			succeed_if(!strcmp(keyValue(key),"value_new2"),"value of second overwritten key not correct");
			succeed_if(!strcmp(keyComment(key),""),"comment of second overwritten key not correct");
		}
	}
	succeed_if(key1_found, KEY_ROOT "/test1 not found");
	succeed_if(key2_found, KEY_ROOT "/test2 not found");
	ksDel(returned);

	keyRemove (key2);
	succeed_if(kdbSetKey(kdb,key2)==0,"Cannot remove key2");
	keyRemove (key1);
	succeed_if(kdbSetKey(kdb,key1)==0,"Cannot remove key1");
	succeed_if(ksDel(ks)==0,"Could not close keySet");
	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");
}

void test_mode()
{
	KDB *kdb;
	Key *key,*key1,*key2,*parent;
	KeySet *returned,*ks;
	int key1_found;
	int ret;
	int mode, setMode;

	printf("testing mode\n");
	kdb=kdbOpen();
	exit_if_fail(kdb,"kdbOpen() failed");

	//TODO: mode 0..5
	for (mode = 6; mode < 8; mode++)
	{
		setMode = mode | mode << 3 | mode << 6;
		printf ("Testing mode: %o\n", setMode);
		
		exit_if_fail(ks=ksNew(0),"ksNew(0) failed");
		/* Create a key with permission setMode */
		key=keyNew(KEY_ROOT "/mode", KEY_MODE, setMode, KEY_VALUE,"value1",
				KEY_COMMENT,"comment1",KEY_END);
		succeed_if(key!=NULL, "keyNew: Unable to create a new key: user/test");
		succeed_if(keyGetMode(key) == setMode, "Could not set mode");
		ksAppendKey(ks,key);
		/*errno = KDB_ERR_OK;*/
		ret = kdbSet(kdb,ks,0,0);
		if (mode & MODE_WRITE)
		{
			// succeed_if (ret == 0, "could not write key");
		} else {
			succeed_if (ret == -1, "wrote key without write mode?");
			/*succeed_if (errno == KDB_ERR_NOCRED, "Errno not set correctly");*/
			// TODO: exit_if_fail (ksCurrent(ks), "no current key, where did it fail?");
			// succeed_if (strcmp (keyName(ksCurrent(ks)), KEY_ROOT "/mode") == 0, "did stop at wrong key");
			// succeed_if (strcmp (keyValue(ksCurrent(ks)), "value1") == 0, "wrong value");
			// succeed_if (strcmp (keyComment(ksCurrent(ks)), "comment1") == 0, "wrong comment");
		}
		ksDel (ks);

		returned=ksNew(0);
		parent=keyNew(KEY_ROOT, KEY_END);
		ret = kdbGet(kdb,returned,parent,0);
		keyDel(parent);

		key1_found=0;

		ksRewind(returned);
		while ((key=ksNext(returned))!=0)
		{
			if (!strcmp(keyName(key), KEY_ROOT "/mode")) {
				key1_found=1;
				// succeed_if(keyGetMode(key) == setMode, "Mode not correct after get");
				if ((mode & MODE_READ) && (mode & MODE_WRITE))
				{
					succeed_if(strcmp (keyValue (key), "value1") == 0,
							"Could not read value, but should be allowed to");
					succeed_if(strcmp (keyComment (key), "comment1") == 0,
							"Could not read comment, but should be allowed to");
				}
				else if (mode & MODE_WRITE)
				{
					succeed_if(!strcmp (keyValue (key), "value1") == 0,
							"Could read value, but should not be allowed to");
					succeed_if(strcmp (keyValue (key), "") == 0,
							"Value not empty, but write should have failed");
					succeed_if(!strcmp (keyComment (key), "comment1") == 0,
							"Could read comment, but should not be allowed to");
					succeed_if(strcmp (keyComment (key), "") == 0,
							"Comment not empty, but write should have failed");
				}
				else if (mode & MODE_READ)
				{
					succeed_if(!strcmp (keyValue (key), "value1") == 0,
							"Could read value, but should not be allowed to");
					succeed_if(strcmp (keyValue (key), "") == 0,
							"Value not empty after not allowed read");
					succeed_if(!strcmp (keyComment (key), "comment1") == 0,
							"Could read comment, but should not be allowed to");
					succeed_if(strcmp (keyComment (key), "") == 0,
							"Comment not empty after not allowed read");
				}
			}
		}

		succeed_if(key1_found, KEY_ROOT "/test1 key not found");
		ksDel(returned);

		/* Overwrite the key*/
		// TODO: copy code from above

		
		/* Create a sub key for /mode */
		exit_if_fail(ks=ksNew(0), "ksNew(0) failed");
		key2=keyNew(KEY_ROOT "/mode/key", KEY_VALUE,"value_sub",KEY_COMMENT,"comment_sub",KEY_END);
		succeed_if(key2!=NULL, "keyNew: Unable to create a new subkey");
		ksAppendKey(ks,key2);

		if (kdbSet(kdb,ks,0,0)==-1) {
			if (!(mode & 1)) // not a directory, can't write subkey
			{
				succeed_if(0,"kdbSet failed");
			}
		}
		ksDel(ks);

		/* Overwrite the directory key if there is already a subkey */
		ks=ksNew(0);
		key1=keyNew(KEY_ROOT "/mode/key", KEY_VALUE,"value_new",KEY_COMMENT,"comment_new",KEY_END);
		key2=keyNew(KEY_ROOT "/mode", KEY_VALUE,"value_new",KEY_COMMENT,"comment_new",KEY_END);
		ksAppendKey(ks,key1);
		ksAppendKey(ks,key2);

		if (kdbSet(kdb,ks,0,0)==-1)
		{
			// succeed_if(errno==KDB_ERR_NOTEMPTY, "kdbSet delete nonempty key KDB_ERR_NOTEMPTY expected");
			// TODO exit_if_fail (ksCurrent(ks) != 0, "key is Null but should be set to incorrect key");
			// printf("trapped: %s\n",keyName(ksCurrent(ks)));
			// succeed_if (!strcmp(KEY_ROOT "/mode",keyName(ksCurrent(ks))), "kdbSet messed up setting ksCurrent()");
		} else {
			/*
			if (!(mode & 1)) // is not directory key
			{
				succeed_if(0,"Could overwrite a directory with subkeys");
			}
			*/
		}

		/* Cleanup */
		succeed_if (kdbRemove (kdb, KEY_ROOT "/mode/key") == 0, "Could not remove mode/key");
		succeed_if (kdbRemove (kdb, KEY_ROOT "/mode") == 0, "Could not remove mode");
		succeed_if(ksDel(ks)==0,"Could not close keySet");
	}
	exit_if_fail(kdbClose(kdb)==0,"Could not close libelektra");


	return;
}

/* This tests the lowest-level way to set a key and get it afterwards */
void test_simple_getset()
{
	KDB *kdb;
	Key *key,*key1;
	KeySet *ks;
	KeySet *returned;

	printf("testing simple write/read\n");

	// create key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/test1", KEY_VALUE,"value", KEY_COMMENT,"comment", KEY_END);
	ksAppendKey(ks,key1);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	// get key test1
	kdb=kdbOpen();
	key=keyNew(KEY_ROOT, KEY_END);
	returned = ksNew(0);
	if (kdbGet(kdb,returned,key,0)==-1) {
		succeed_if(0,"kdbGet failed");
	}
	keyDel(key);
	key1 = ksLookupByName (returned, KEY_ROOT "/test1", 0);
	exit_if_fail (key1 != 0, "could not lookup key");
	succeed_if (strcmp (keyValue (key1), "value") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "comment") == 0, "Comment not correct");
	ksDel (returned);
	kdbClose(kdb);

	// delete key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/test1", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	return;
}

/* This tests the highlevel-level way to set a key and get it afterwards */
void test_highlevel_getset()
{
	KDB *kdb;
	Key *key1;

	printf("testing simple write/read\n");

	// create key test3
	kdb=kdbOpen();
	key1=keyNew(KEY_ROOT "/test3", KEY_VALUE,"value", KEY_COMMENT,"comment", KEY_END);
	succeed_if (kdbSetKey(kdb,key1)!=-1, "could not kdbSet keys");
	keyDel (key1);
	kdbClose(kdb);

	// get key test3
	kdb=kdbOpen();
	key1=keyNew(KEY_ROOT "/test3", KEY_END);
	if (kdbGetKey(kdb,key1)==-1) {
		succeed_if(0,"kdbGetKey failed");
	}
	succeed_if (strcmp (keyValue (key1), "value") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "comment") == 0, "Comment not correct");
	keyDel (key1);
	kdbClose(kdb);

	// delete key test3
	kdb=kdbOpen();
	key1=keyNew(KEY_ROOT "/test3", KEY_REMOVE, KEY_END);
	succeed_if (kdbSetKey (kdb,key1)==0, "could not get a key");
	keyDel (key1);
	kdbClose(kdb);

	return;
}

/* This tests do recursive set and get keys */
void test_simple_recursive_getset()
{
	KDB *kdb;
	Key *key,*key1;
	KeySet *ks;
	KeySet *returned;

	printf("testing recursive write/read\n");

	// create key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/dir", KEY_DIR, KEY_VALUE,"dirvalue", KEY_COMMENT,"dircomment", KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir/key", KEY_VALUE,"value", KEY_COMMENT,"comment", KEY_END);
	ksAppendKey(ks,key1);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	// get key test1
	kdb=kdbOpen();
	key=keyNew(KEY_ROOT, KEY_END);
	returned = ksNew(0);
	if (kdbGet(kdb,returned,key,0)==-1) {
		succeed_if(0,"kdbGet failed");
	}
	keyDel(key);
	key1 = ksLookupByName (returned, KEY_ROOT "/dir/key", 0);
	exit_if_fail (key1 != 0, "could not lookup key");
	succeed_if (strcmp (keyValue (key1), "value") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "comment") == 0, "Comment not correct");
	
	key1 = ksLookupByName (returned, KEY_ROOT "/dir", 0);
	exit_if_fail (key1 != 0, "could not lookup key");
	succeed_if (strcmp (keyValue (key1), "dirvalue") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "dircomment") == 0, "Comment not correct");
	ksDel (returned);
	kdbClose(kdb);

	// delete key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/dir/key", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir", KEY_DIR, KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	return;
}

/* This tests do recursive set and get keys */
void test_recursive_getset()
{
	KDB *kdb;
	Key *key,*key1;
	KeySet *ks;
	KeySet *returned;
	int found [5];
	int i;

	for (i=0; i<5; i++) found[i] = 0;

	printf("testing recursive write/read\n");

	// create key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/key1", KEY_VALUE,"value1", KEY_COMMENT,"comment", KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir2", KEY_DIR, KEY_VALUE,"dirvalue", KEY_COMMENT,"dircomment", KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir1", KEY_DIR, KEY_VALUE,"dirvalue", KEY_COMMENT,"dircomment", KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir1/key2", KEY_VALUE,"value2", KEY_COMMENT,"comment", KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir1/key1", KEY_VALUE,"value1", KEY_COMMENT,"comment", KEY_END);
	ksAppendKey(ks,key1);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	// get key test1
	kdb=kdbOpen();
	key=keyNew(KEY_ROOT, KEY_END);
	returned = ksNew(0);
	if (kdbGet(kdb,returned,key,0)==-1) {
		succeed_if(0,"kdbGet failed");
	}
	keyDel(key);

	// check if all keys are in set and exactly once
	// output_keyset(returned, 0);
	ksRewind(returned);
	while ((key=ksNext(returned))!=0)
	{
		if (strcmp (keyName(key), KEY_ROOT "/key1") == 0) found [0]++;
		else if (strcmp (keyName(key), KEY_ROOT "/dir2") == 0) found [1]++;
		else if (strcmp (keyName(key), KEY_ROOT "/dir1") == 0) found [2]++;
		else if (strcmp (keyName(key), KEY_ROOT "/dir1/key2") == 0) found [3]++;
		else if (strcmp (keyName(key), KEY_ROOT "/dir1/key1") == 0) found [4]++;
	}

	for (i=0; i<5;i++)
	{
		// printf("%d", found[i]);
		succeed_if (found[i] == 1, "key did not appear exactly once");
	}


	key1 = ksLookupByName (returned, KEY_ROOT "/dir1/key1", 0);
	exit_if_fail (key1 != 0, "could not lookup key");
	succeed_if (strcmp (keyValue (key1), "value1") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "comment") == 0, "Comment not correct");
	
	key1 = ksLookupByName (returned, KEY_ROOT "/dir1", 0);
	exit_if_fail (key1 != 0, "could not lookup key");
	succeed_if (strcmp (keyValue (key1), "dirvalue") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "dircomment") == 0, "Comment not correct");
	ksDel (returned);
	kdbClose(kdb);

	// delete key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/dir1/key1", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir1/key2", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir1", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/dir2", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	key1=keyNew(KEY_ROOT "/key1", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	return;
}

/* This tests the lowest-level way to set a directory key and get it afterwards */
void test_directory_getset()
{
	KDB *kdb;
	Key *key,*key1;
	KeySet *ks;
	KeySet *returned;

	printf("testing simple write/read\n");

	// create key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/dir4", KEY_DIR, KEY_VALUE,"value4", KEY_COMMENT,"comment4", KEY_END);
	ksAppendKey(ks,key1);

	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	// get key test1
	kdb=kdbOpen();
	key=keyNew(KEY_ROOT, KEY_END);
	returned = ksNew(0);
	if (kdbGet(kdb,returned,key,0)==-1) {
		succeed_if(0,"kdbGet failed");
	}
	keyDel(key);
	key1 = ksLookupByName (returned, KEY_ROOT "/dir4", 0);
	succeed_if (strcmp (keyValue (key1), "value4") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "comment4") == 0, "Comment not correct");
	succeed_if (keyIsDir(key1) , "not a directory");
	ksDel (returned);
	kdbClose(kdb);

	// delete key test1
	kdb=kdbOpen();
	ks=ksNew(0);
	key1=keyNew(KEY_ROOT "/dir4", KEY_REMOVE, KEY_END);
	ksAppendKey(ks,key1);
	succeed_if (kdbSet(kdb,ks,0,0)!=-1, "could not kdbSet keys");
	ksDel(ks);
	kdbClose(kdb);

	return;
}

/* This tests the highlevel-level way to set a directory and get it afterwards */
void test_directory_highlevel_getset()
{
	KDB *kdb;
	Key *key1;

	printf("testing simple write/read\n");

	// create key test3
	kdb=kdbOpen();
	key1=keyNew(KEY_ROOT "/dir5", KEY_DIR, KEY_VALUE,"value5", KEY_COMMENT,"comment5", KEY_END);
	succeed_if (kdbSetKey(kdb,key1)!=-1, "could not kdbSet a key");
	keyDel (key1);
	kdbClose(kdb);

	// get key test3
	kdb=kdbOpen();
	key1=keyNew(KEY_ROOT "/dir5", KEY_END);
	if (kdbGetKey(kdb,key1)==-1) {
		succeed_if(0,"kdbGetKey failed");
	}
	succeed_if (strcmp (keyValue (key1), "value5") == 0, "Value not correct");
	succeed_if (strcmp (keyComment(key1), "comment5") == 0, "Comment not correct");
	succeed_if (keyIsDir(key1) , "not a directory");
	keyDel (key1);
	kdbClose(kdb);

	// delete key test3
	kdb=kdbOpen();
	key1=keyNew(KEY_ROOT "/dir5", KEY_REMOVE, KEY_END);
	succeed_if (kdbSetKey (kdb,key1)!=-1, "could not set a key");
	keyDel (key1);
	kdbClose(kdb);

	return;
}

int main()
{
	printf("\n\n");
	printf("ELEKTRA SET/GET TEST SUITE\n");
	printf("========================================\n\n");

	exit_if_fail (loadToolsLib()==0, "Unable to load elektratools");

	init ();

	test_simple_getset();
	test_writeread();
	test_subdir();
	test_mode();
	test_highlevel_getset();
	test_simple_recursive_getset();
	test_recursive_getset();
	test_directory_getset();
	test_directory_highlevel_getset();

	printf("\ntest_getset RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	
	return nbError;
}

