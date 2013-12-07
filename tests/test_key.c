/***************************************************************************
 *          test_key.c  -  Key struct test suite
 *                  -------------------
 *  begin                : Thu Aug 03 2006
 *  copyright            : (C) 2006 by Yannick Lecaillez
 *  email                : sizon5@gmail.com
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <tests_internal.h>

struct test {
	char	*testName;
	char	*keyName;
	
	char	*expectedKeyName;
	char	*expectedBaseName;
	char	*expectedFRootName;
	char	*expectedParentName;
};

struct test tstKeyName[] = 
{
	{ "Normal key", "system/foo/bar",
		"system/foo/bar",
		"bar",
		"system",
		"system/foo"
	},

	{ "Key containing redundant & trailing separator", "system//foo//bar//",
		"system/foo/bar", 	/* keyName 	*/
		"bar", 			/* keyBaseName	*/
		"system",		/* keyGetFullRootName	*/
		"system/foo"		/* keyGetParentName	*/
	},

	{ "Normal user key", "user/key",
		"user/key", 			/* keyName 	*/
		"key", 				/* keyBaseName 	*/
		"user",				/* keyGetFullRootName 	*/ 
		"user"				/* keyGetParentName	*/
	
	},

	{ "Normal user key with owner", "user:owner/key",
		"user/key", 			/* keyName 	*/
		"key", 				/* keyBaseName 	*/
		"user:owner",			/* keyGetFullRootName 	*/ 
		"user"				/* keyGetParentName	*/
	
	},

	{ "Depth user key with owner", "user:owner/folder/long/base/dir/key",
		"user/folder/long/base/dir/key", /* keyName 	*/
		"key", 				/* keyBaseName 	*/
		"user:owner",			/* keyGetFullRootName 	*/ 
		"user/folder/long/base/dir"	/* keyGetParentName	*/
	
	},

	{ "Key containing escaped separator", "user:yl///foo\\///bar\\/foo_bar\\",
		"user/foo\\//bar\\/foo_bar\\", 	/* keyName 	*/
		"bar\\/foo_bar\\", 		/* keyBaseName 	*/
		"user:yl",			/* keyGetFullRootName 	*/ 
		"user/foo\\/"			/* keyGetParentName	*/
	
	},

	{ "Key containing escaped separator at the end", "user:yl///foo\\///bar\\/foo_bar\\/",
		"user/foo\\//bar\\/foo_bar\\/",	/* keyName 	*/
		"bar\\/foo_bar\\/", 		/* keyBaseName 	*/
		"user:yl",			/* keyGetFullRootName 	*/ 
		"user/foo\\/"			/* keyGetParentName	*/
	
	},
	
	{ NULL, NULL,
		NULL, /* keyName 	*/
		NULL, /* keyBaseName 	*/
		NULL, /* keyGetFullRootName 	*/ 
		NULL  /* keyGetParentName	*/
	}
};

void test_keyComparing()
{
	Key *key1 = keyNew(0);
	Key *key2 = keyNew(0);

	succeed_if(keyCompare(key1,key2) == 0, "the keys don't differ of course");

	keySetName (key1, "user/myname");
	succeed_if(keyCompare(key1,key2) == KEY_NAME, "the keys should differ in name");
	keySetName (key2, "user/myname");
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in name");

	keySetOwner (key1, "myowner");
	succeed_if(keyCompare(key1,key2) == KEY_OWNER, "the keys should differ in owner");
	keySetOwner (key2, "myowner");
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in owner");

	keySetString (key1, "myvalue");
	succeed_if(keyCompare(key1,key2) == (KEY_VALUE), "the keys should differ in value");

	keySetString (key2, "myvalue");
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in value");

	keySetComment (key1, "mycomment");
	succeed_if(keyCompare(key1,key2) == KEY_COMMENT, "the keys should differ in comment");
	keySetComment (key2, "mycomment");
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in comment");

	keySetUID (key1, 50);
	succeed_if(keyCompare(key1,key2) == KEY_UID, "the keys should differ in uid");
	keySetUID (key2, 50);
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in uid");

	keySetGID (key1, 50);
	succeed_if(keyCompare(key1,key2) == KEY_GID, "the keys should differ in gid");
	keySetGID (key2, 50);
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in gid");

	keySetMode (key1, 0222);
	succeed_if(keyCompare(key1,key2) == KEY_MODE, "the keys should differ in mode");
	keySetMode (key2, 0222);
	succeed_if(keyCompare(key1,key2) == 0, "the keys should not differ in mode");

	keyDel (key1);
	keyDel (key2);
}

void test_keyNewSpecial()
{
	printf ("Test special key creation\n");

	Key *k = keyNew (KEY_END);
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");
	keyDel (k);

	k = keyNew (0, KEY_END); //  might break, useless arguments?
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");
	keyDel (k);

	k = keyNew ("", KEY_END);
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");
	keyDel (k);

	k = keyNew ("invalid", KEY_END);
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");
	keyDel (k);


	k = keyNew ("other invalid", KEY_END);
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");
	keyDel (k);

	k = keyNew ("system spaces", KEY_END);
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");
	keyDel (k);
}

void test_keyNewSystem()
{
	Key     *key;
	char array[] = "here is some data stored";
	char * getBack;
	Key *k1;
	Key *k2;
	Key *k3;

	printf("Test system key creation\n");

	// Empty key
	key = keyNew(0);
	succeed_if(key != NULL, "keyNew: Unable to create a new empty key");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete empty key");

	// Key with name
	key = keyNew("system/sw/test", KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name");
	succeed_if(strcmp(keyName(key), "system/sw/test") == 0, "keyNew: Key's name setted incorrectly");
	keyCopy (key, 0);
	succeed_if (strcmp (keyName(key), "") == 0, "name after keyCopy(,0)");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");
	
	// Key with name
	key = keyNew("system/sw/test", KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name");
	succeed_if(strcmp(keyName(key), "system/sw/test") == 0, "keyNew: Key's name setted incorrectly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");
	
	// Key with name + value
	key = keyNew("system/sw/test",
			KEY_VALUE, "test",
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(strcmp(keyValue(key), "test") == 0, "keyNew: Value not set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + value");
	
	// Key with name + UID/GID
	key = keyNew("system/sw/test",
			KEY_UID, 123,
			KEY_GID, 456,
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + UID + GID");
	succeed_if(keyGetUID(key) == 123, "keyNew: UID no set correctly");
	succeed_if(keyGetGID(key) == 456, "keyNew: GID not set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + UID + GID");

	// Key with name + MODE
	key = keyNew("system/sw/test",
			KEY_MODE, 0644,
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + mode");
	succeed_if(keyGetMode(key) == 0644, "keyNew: mode no set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + mode");

	key = keyNew("system/valid/there",
			KEY_BINARY,
			KEY_SIZE, sizeof(array),
			KEY_VALUE, array,
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsBinary (key), "Could not set type to binary");
	succeed_if(keyGetValueSize(key) == sizeof(array), "Value size not correct");
	succeed_if(memcmp ((char *) keyValue(key), array, sizeof(array)) == 0, "could not get correct binary value");
	getBack = malloc (keyGetValueSize(key));
	keyGetBinary(key, getBack, keyGetValueSize(key));
	succeed_if(memcmp(getBack, array, sizeof(array)) == 0, "could not get correct value with keyGetBinary");
	free (getBack);
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + owner");

	key = keyNew("system", KEY_END);
	succeed_if (strcmp (keyName(key), "system") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 7, "empty name size" );
	succeed_if (strcmp (keyOwner(key), "") == 0, "owner for a system key?");
	succeed_if (keyGetOwnerSize(key) == 1, "owner y size" );
	keyDel (key);

	// testing multiple values at once
	k1=keyNew("system/1",  KEY_VALUE, "singlevalue", KEY_END);
	k2=keyNew("system/2",   KEY_VALUE, "myvalue", KEY_END);
	k3=keyNew("system/3", KEY_VALUE, "syskey",  KEY_END);
	succeed_if(k1 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(k1), "keyNew: Default key value isn't set to string");
	succeed_if(strcmp(keyValue(k1), "singlevalue") == 0, "keyNew: Value not set correctly");
	
	succeed_if(k2 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(k2), "keyNew: Default key value isn't set to string");
	succeed_if(strcmp(keyValue(k2), "myvalue") == 0, "keyNew: Value not set correctly");
	
	succeed_if(k3 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(k3), "keyNew: Default key value isn't set to string");
	succeed_if(strcmp(keyValue(k3), "syskey") == 0, "keyNew: Value not set correctly");

	succeed_if(keyDel(k1) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if(keyDel(k2) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if(keyDel(k3) == 0, "keyDel: Unable to delete key with name + value");
}

void test_keyNewUser()
{
	Key     *key;
	char array[] = "here is some data stored";
	char fullroot [KDB_MAX_PATH_LENGTH];
	char * getBack;
	Key *k1;
	Key *k2;
	Key *k3;

	printf("Test user key creation\n");

	key = keyNew ("user/test/test", KEY_END);
	succeed_if (keySetOwner(key, "hugo") == sizeof("hugo"), "could not set owner");
	succeed_if( strcmp(keyOwner(key), "hugo") == 0, "keyNew: owner not set correctly");
	succeed_if( keyGetOwnerSize(key) == 5, "owner length not correct");
	keyGetFullName (key, fullroot, KDB_MAX_PATH_LENGTH);
	succeed_if( strcmp(keyOwner(key), "hugo") == 0, "keyNew: owner not set correctly");
	/* printf ("%s, %s, %s\n", keyName(key), keyBaseName(key), fullroot); */
	succeed_if(strcmp(keyName(key),"user/test/test") == 0, "Wrong keyname: keyName");
	succeed_if(strcmp(keyBaseName(key),"test") == 0, "Wrong keyname: keyBaseName");
	// printf ("%s\n", fullroot);
	succeed_if(strcmp(fullroot,"user:hugo/test/test") == 0, "Wrong keyname: keyGetFullName");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + owner");

	key = keyNew ("user/test/test", KEY_END);
	succeed_if (keySetOwner(key, "tommy") == sizeof("tommy"), "could not set owner");
	succeed_if( strcmp(keyOwner(key), "tommy") == 0, "keyNew: owner not set correctly");
	succeed_if( keyGetOwnerSize(key) == 6, "owner length not correct");
	keyGetFullName (key, fullroot, KDB_MAX_PATH_LENGTH);
	succeed_if( strcmp(keyOwner(key), "tommy") == 0, "keyNew: owner not set correctly");
	/* printf ("%s, %s, %s\n", keyName(key), keyBaseName(key), fullroot); */
	succeed_if(strcmp(keyName(key),"user/test/test") == 0, "Wrong keyname: keyName");
	succeed_if(strcmp(keyBaseName(key),"test") == 0, "Wrong keyname: keyBaseName");
	succeed_if(strcmp(fullroot,"user:tommy/test/test") == 0, "Wrong keyname: keyGetFullName");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + owner");
	
	// Key with name + owner
	key = keyNew("user/test/test",
			KEY_OWNER, "yl",
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + owner");
	succeed_if( strcmp(keyOwner(key), "yl") == 0, "keyNew: owner not set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + owner");
	
	key = keyNew("user/valid/there",
			KEY_BINARY,
			KEY_SIZE, sizeof(array),
			KEY_VALUE, array,
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsBinary (key), "Could not set type to binary");
	succeed_if(keyGetValueSize(key) == sizeof(array), "Value size not correct");
	succeed_if(memcmp ((char *) keyValue(key), array, sizeof(array)) == 0, "could not get correct binary value");
	getBack = malloc (keyGetValueSize(key));
	keyGetBinary(key, getBack, keyGetValueSize(key));
	succeed_if(memcmp(getBack, array, sizeof(array)) == 0, "could not get correct value with keyGetBinary");
	free (getBack);
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + owner");

	key = keyNew("user:y", KEY_END);
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 5, "empty name size" );
	succeed_if (strcmp (keyOwner(key), "y") == 0, "Should be a name not length 0");
	succeed_if (keyGetOwnerSize(key) == 2, "owner y size" );
	keyDel (key);

	succeed_if (key = keyNew("user:perfectowner", KEY_END), "could not create new key");
	succeed_if (keySetName(key, "user:perfectowner") == 5, "could not set to user with owner");
	succeed_if (keyGetOwnerSize (key) == 13, "owner size not correct");
	succeed_if (strcmp (keyOwner(key), "perfectowner") == 0, "Owner not same as set");
	succeed_if (keyDel (key) == 0, "could not delete key");

	// testing multiple values at once
	k1=keyNew("user/1",  KEY_VALUE, "singlevalue", KEY_END);
	k2=keyNew("user/2",   KEY_VALUE, "myvalue", KEY_END);
	k3=keyNew("user/3", KEY_VALUE, "syskey",  KEY_END);
	succeed_if(k1 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(k1), "keyNew: Default key value isn't set to string");
	succeed_if(strcmp(keyValue(k1), "singlevalue") == 0, "keyNew: Value not set correctly");
	
	succeed_if(k2 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(k2), "keyNew: Default key value isn't set to string");
	succeed_if(strcmp(keyValue(k2), "myvalue") == 0, "keyNew: Value not set correctly");
	
	succeed_if(k3 != NULL, "keyNew: Unable to create a key with name + value of default type");
	succeed_if(keyIsString(k3), "keyNew: Default key value isn't set to string");
	succeed_if(strcmp(keyValue(k3), "syskey") == 0, "keyNew: Value not set correctly");

	succeed_if(keyDel(k1) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if(keyDel(k2) == 0, "keyDel: Unable to delete key with name + value");
	succeed_if(keyDel(k3) == 0, "keyDel: Unable to delete key with name + value");

	k1 = keyNew ("invalid", KEY_END);
	succeed_if (k1 != 0, "should construct key even on invalid names");
	succeed_if (strcmp (keyName(k1), "") == 0, "no name should be set");
	succeed_if (keyGetNameSize(k1) == 1, "namesize");
	keyDel (k1);
}

void test_keyReference()
{
	printf("Test key reference\n");

	Key *key = keyNew ("user/key", KEY_END);
	Key *c = keyNew ("user/c", KEY_END);
	Key *d;
	KeySet *ks1, *ks2;
	succeed_if (keyGetRef(key) == 0, "New created key reference");

	succeed_if (keyIncRef (key) == 1, "keyIncRef return value");
	succeed_if (keyGetRef(key) == 1, "After keyIncRef key reference");
	succeed_if (keyIncRef (key) == 2, "keyIncRef return value");
	succeed_if (keyGetRef(key) == 2, "After keyIncRef key reference");
	succeed_if (keyIncRef (key) == 3, "keyIncRef return value");
	succeed_if (keyGetRef(key) == 3, "After keyIncRef key reference");
	succeed_if (keyIncRef (key) == 4, "keyIncRef return value");
	succeed_if (keyGetRef(key) == 4, "After keyIncRef key reference");

	d = keyDup (key);
	succeed_if (keyGetRef(d) == 0, "After keyDup key reference");
	succeed_if (keyIncRef (d) == 1, "keyIncRef return value");
	succeed_if (keyGetRef(key) == 4, "Reference should not change");
	succeed_if (keyDecRef (d) == 0, "decrement key");
	succeed_if (keyDel(d) == 0, "last keyDel d, key exist");

	keyCopy (c, key);
	succeed_if (keyGetRef(c) == 0, "After keyCopy key reference");
	succeed_if (keyIncRef (c) == 1, "keyIncRef return value");
	succeed_if (keyGetRef(key) == 4, "Reference should not change");

	keyCopy (c, key);
	succeed_if (keyGetRef(c) == 1, "After keyCopy key reference");
	succeed_if (keyDecRef (c) == 0, "keyDecRef return value");
	succeed_if (keyGetRef(key) == 4, "Reference should not change");

	succeed_if (keyIncRef (c) == 1, "keyIncRef return value");
	succeed_if (keyIncRef (c) == 2, "keyIncRef return value");
	keyCopy (c, key);
	succeed_if (keyGetRef(c) == 2, "After keyCopy key reference");
	succeed_if (keyDecRef (c) == 1, "keyDecRef return value");
	succeed_if (keyDecRef (c) == 0, "keyDecRef return value");
	succeed_if (keyDel (c) == 0, "could not delete copy");

	succeed_if (keyGetRef(key) == 4, "After keyIncRef key reference");
	succeed_if (keyDecRef (key) == 3, "keyDel return value");
	succeed_if (keyDel (key) == 3, "should not do anything");
	succeed_if (keyGetRef(key) == 3, "After keyIncRef key reference");
	succeed_if (keyDecRef (key) == 2, "keyDel return value");
	succeed_if (keyDel (key) == 2, "should not do anything");
	succeed_if (keyGetRef(key) == 2, "After keyIncRef key reference");
	succeed_if (keyDecRef (key) == 1, "keyDel return value");
	succeed_if (keyDel (key) == 1, "should not do anything");
	succeed_if (keyGetRef(key) == 1, "Should have no more reference");
	succeed_if (keyDecRef (key) == 0, "last keyDel key, key exist");
	succeed_if (keyDel (key) == 0, "last keyDel key, key exist");

	/* From examples in ksNew () */
	key = keyNew(0); // ref counter 0
	succeed_if (keyGetRef(key) == 0, "reference counter");
	keyIncRef(key); // ref counter of key 1
	succeed_if (keyGetRef(key) == 1, "reference counter");
	keyDel(key);    // has no effect
	succeed_if (keyGetRef(key) == 1, "reference counter");
	keyDecRef(key); // ref counter back to 0
	succeed_if (keyGetRef(key) == 0, "reference counter");
	keyDel(key);    // key is now deleted

	ks1 = ksNew(0);
	ks2 = ksNew(0);
	key = keyNew("user/key", KEY_END); // ref counter 0
	succeed_if (keyGetRef(key) == 0, "reference counter");
	ksAppendKey(ks1, key); // ref counter of key 1
	succeed_if (keyGetRef(key) == 1, "reference counter");
	ksAppendKey(ks2, key); // ref counter of key 2
	succeed_if (keyGetRef(key) == 2, "reference counter");
	ksDel(ks1); // ref counter of key 1
	succeed_if (keyGetRef(key) == 1, "reference counter");
	ksDel(ks2); // key is now deleted

	key = keyNew(0); // ref counter 0
	succeed_if (keyGetRef(key) == 0, "reference counter");
	keyIncRef(key); // ref counter of key 1
	succeed_if (keyGetRef(key) == 1, "reference counter");
	keyDel (key);   // has no effect
	succeed_if (keyGetRef(key) == 1, "reference counter");
	keyIncRef(key); // ref counter of key 2
	succeed_if (keyGetRef(key) == 2, "reference counter");
	keyDel (key);   // has no effect
	succeed_if (keyGetRef(key) == 2, "reference counter");
	keyDecRef(key); // ref counter of key 1
	succeed_if (keyGetRef(key) == 1, "reference counter");
	keyDel (key);   // has no effect
	succeed_if (keyGetRef(key) == 1, "reference counter");
	keyDecRef(key); // ref counter is now 0
	succeed_if (keyGetRef(key) == 0, "reference counter");
	keyDel (key); // key is now deleted

	return;

	/* This code needs very long to execute, especially on 64bit
	 * systems. */

	key = keyNew(0); // ref counter 0
	while (keyGetRef(key) < SSIZE_MAX) keyIncRef(key);
	succeed_if (keyGetRef(key) == SSIZE_MAX, "reference counter");
	succeed_if (keyIncRef(key) == SSIZE_MAX, "should stay at maximum");
	succeed_if (keyGetRef(key) == SSIZE_MAX, "reference counter");
	succeed_if (keyIncRef(key) == SSIZE_MAX, "should stay at maximum");

	key->ksReference = 5;
	while (keyGetRef(key) > 0) keyDecRef(key);
	succeed_if (keyGetRef(key) == 0, "reference counter");
	succeed_if (keyDecRef(key) == 0, "should stay at minimum");
	succeed_if (keyGetRef(key) == 0, "reference counter");
	succeed_if (keyDecRef(key) == 0, "should stay at minimum");
	keyDel (key);

}

void test_keyName()
{
	Key	*key;
	size_t	size;
	char	*buf;
	char	ret [1000];
	size_t	i;
	char *getBack;
	char testName[] = "user/name";
	char testFullName[] = "user:max/name";
	char testBaseName[] = "name";
	char testOwner[] = "max";

	Key * copy = keyNew (KEY_END);

#ifdef HAVE_CLEARENV
	clearenv();
#else
	unsetenv("USER");
#endif

	printf ("Test Key Name\n");

	key = keyNew (testName, KEY_END);
	succeed_if (keyGetName (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetName (key,0,100) == -1, "string null pointer");
	succeed_if (keyGetName (key,ret,0) == -1, "length checking");
	for (i=1; i< sizeof(testName);i++)
	{
		succeed_if (keyGetName (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testName); i<sizeof(testName)*2; i++)
	{
		succeed_if (keyGetName (key,ret,i) == sizeof(testName), "length checking longer");
	}
	succeed_if (keyGetName (key,ret, (size_t)-1) == -1, "maxSize exceeded");
	keyDel (key);

	succeed_if (keyName(0) == 0, "null pointer");

	key = keyNew (0);
	succeed_if (strcmp(keyName(key), "") == 0, "empty name");
	succeed_if (keyGetName (key,ret, 1000) == 1, "get empty name");
	succeed_if (strcmp(ret, "") == 0, "not empty name");
	succeed_if (keyGetName (key,ret, 0) == -1, "get empty name");
	keyDel (key);

	succeed_if (keySetName(0,ret) == -1, "Null pointer");



	printf ("Test Key Full Name\n");

	key = keyNew (testFullName, KEY_END);
	succeed_if (keyGetFullName (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetFullName (key,0,100) == -1, "string null pointer");
	succeed_if (keyGetFullName (key,ret,0) == -1, "length checking");
	for (i=1; i< sizeof(testFullName);i++)
	{
		succeed_if (keyGetFullName (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testFullName); i<sizeof(testFullName)*2; i++)
	{
		succeed_if (keyGetFullName (key,ret,i) == sizeof(testFullName), "length checking longer");
	}
	succeed_if (keyGetFullName (key,ret, (size_t)-1) == -1, "maxSize exceeded");
	keyDel (key);

	key = keyNew (0);
	succeed_if (keyIsUser(key) == 0, "empty user key?");
	succeed_if (keyIsSystem(key) == 0, "empty user key?");
	succeed_if (keyGetFullName (key,ret, 1000) == 1, "get empty name");
	succeed_if (strcmp(ret, "") == 0, "not empty name");
	succeed_if (keyGetFullName (key,ret, 0) == -1, "get empty name");
	keyDel (key);

	succeed_if (keySetName(0,ret) == -1, "Null pointer");




	printf ("Test Key Base Name\n");

	key = keyNew (testFullName, KEY_END);
	succeed_if (keyGetBaseName (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetBaseName (key,0,100) == -1, "string null pointer");
	succeed_if (keyGetBaseName (key,ret,0) == -1, "length checking");
	for (i=1; i< sizeof(testBaseName);i++)
	{
		succeed_if (keyGetBaseName (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testBaseName); i<sizeof(testBaseName)*2; i++)
	{
		succeed_if (keyGetBaseName (key,ret,i) == sizeof(testBaseName), "length checking longer");
	}
	succeed_if (keyGetBaseName (key,ret, (size_t)-1) == -1, "maxSize exceeded");
	keyDel (key);

	succeed_if (keyBaseName(0) == 0, "null pointer");

	key = keyNew (0);
	succeed_if (strcmp(keyBaseName(key), "") == 0, "empty name");
	succeed_if (keyIsUser(key) == 0, "empty user key?");
	succeed_if (keyIsSystem(key) == 0, "empty user key?");
	succeed_if (keyGetBaseName (key,ret, 1000) == 1, "get empty name");
	succeed_if (strcmp(ret, "") == 0, "not empty name");
	succeed_if (keyGetBaseName (key,ret, 0) == -1, "get empty name");
	keyDel (key);

	succeed_if (keySetName(0,ret) == -1, "Null pointer");

	key = keyNew ("user", KEY_END);
	succeed_if (keyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (keyGetBaseName (key,ret,1) == 1, "GetBaseName for root key");
	succeed_if (strcmp (ret, "") == 0, "did not return correct basename");
	succeed_if (keyGetBaseName (key,ret,2) == 1, "GetBaseName for root key");
	succeed_if (strcmp (ret, "") == 0, "did not return correct basename");
	keyDel (key);

	key = keyNew ("system", KEY_END);
	succeed_if (keyGetBaseNameSize (key) == 1, "length checking");
	succeed_if (keyGetBaseName (key,ret,1) == 1, "GetBaseName for root key");
	succeed_if (strcmp (ret, "") == 0, "did not return correct basename");
	succeed_if (keyGetBaseName (key,ret,2) == 1, "GetBaseName for root key");
	succeed_if (strcmp (ret, "") == 0, "did not return correct basename");
	keyDel (key);





	printf ("Test Key Owner\n");

	key = keyNew (testFullName, KEY_END);
	succeed_if (keyGetOwner (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetOwner (key,0,100) == -1, "string null pointer");
	succeed_if (keyGetOwner (key,ret,0) == -1, "length checking");
	for (i=1; i< sizeof(testOwner);i++)
	{
		succeed_if (keyGetOwner (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testOwner); i<sizeof(testOwner)*2; i++)
	{
		succeed_if (keyGetOwner (key,ret,i) == sizeof(testOwner), "length checking longer");
	}
	succeed_if (keyGetOwner (key,ret, (size_t)-1) == -1, "maxSize exceeded");

	succeed_if (keySetOwner(key,0) == 1, "delete owner");
	succeed_if (keyGetOwner (key,ret,i) == 1, "length checking deleting");
	succeed_if (strcmp(ret, "") == 0, "not empty owner");

	succeed_if (keySetOwner(key,testOwner) == sizeof(testOwner), "set owner");
	succeed_if (keyGetOwner (key,ret,i) == sizeof(testOwner), "length checking working");
	succeed_if (strcmp(ret, testOwner) == 0, "not empty owner");

	succeed_if (keySetOwner(key,"") == 1, "delete owner");
	succeed_if (keyGetOwner (key,ret,i) == 1, "length checking deleting");
	succeed_if (strcmp(ret, "") == 0, "not empty owner");

	succeed_if (keySetOwner(key,testOwner) == sizeof(testOwner), "set owner");
	succeed_if (keyGetOwner (key,ret,i) == sizeof(testOwner), "length checking working");
	succeed_if (strcmp(ret, testOwner) == 0, "not empty owner");
	keyDel (key);

	succeed_if (keyOwner(0) == 0, "null pointer");

	key = keyNew (0);
	succeed_if (strcmp(keyOwner(key), "") == 0, "empty owner");
	succeed_if (keyGetOwner (key,ret, 1000) == 1, "get empty owner");
	succeed_if (strcmp(ret, "") == 0, "not empty owner");
	succeed_if (keyGetOwner (key,ret, 0) == -1, "get empty owner");
	keyDel (key);

	succeed_if (keySetOwner(0,"") == -1, "null pointer");





	printf("Test Slashes in Key Name\n");
	key = keyNew(0);
	succeed_if (keyGetNameSize(key) == 1, "empty name size" );
	keyDel (key);

	key = keyNew("", KEY_END);
	succeed_if (key != 0, "key should not be null!");
	succeed_if (strcmp (keyName(key), "") == 0, "keyName should be "" string");
	succeed_if (keyGetName(key,ret, 999) == 1, "keyGetName should return 1");
	succeed_if (strcmp (ret, "") == 0, "\\0 should be first character in array");
	succeed_if (keyGetNameSize(key) == 1, "empty name size" );
	keyDel (key);

	key = keyNew (KEY_END);
	keySetName(key,"user");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: User as basename");
	succeed_if (keyGetNameSize(key) == 5, "empty name size" );
	succeed_if (keyGetOwnerSize(key) >= 1, "empty owner size");

	keySetName(key,"system");
	succeed_if (strcmp (keyName(key), "system") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 7, "empty name size" );
	succeed_if (keyGetOwnerSize(key) == 1, "empty owner size");
	keyDel (key);

	key = keyNew (KEY_END);
	keySetName(key,"system");
	succeed_if (strcmp (keyName(key), "system") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 7, "empty name size" );
	succeed_if (keyGetOwnerSize(key) == 1, "empty owner size");

	keySetName(key,"user");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: User as basename");
	succeed_if (keyGetNameSize(key) == 5, "empty name size" );
	succeed_if (keyGetOwnerSize(key) == 1, "empty owner size");
	keyDel (key);

	key = keyNew(0);
	succeed_if (keySetName(key,"user:") == 5, "setting user: generates error");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 5, "empty name size" );
	succeed_if (strcmp (keyOwner(key), "") == 0, "Should be a name not length 0");
	succeed_if (keyGetOwnerSize(key) == 1, "empty owner size" );
	keyDel (key);

	key = keyNew(0);
	succeed_if (keySetName(key,"user:y") == 5, "setting user: generates error");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 5, "empty name size" );
	succeed_if (strcmp (keyOwner(key), "y") == 0, "Should be a name not length 0");
	succeed_if (keyGetOwnerSize(key) == 2, "owner y size" );
	keyDel (key);

	key = keyNew(0);
	succeed_if (keySetName(key,"no") == -1, "no error code setting invalid name");
	succeed_if (strcmp (keyName(key), "") == 0, "Name Problem: System as basename");
	succeed_if (keyGetNameSize(key) == 1, "empty name size" );
	keyDel (key);

	key = keyNew("user/noname", KEY_END);
	succeed_if(keyGetNameSize(key) == 12, "size not correct after keyNew");
	getBack = malloc (12);
	succeed_if(keyGetName(key, getBack, 12), "could not get name");
	succeed_if(strcmp(getBack, "user/noname") == 0, "did not get correct value back");
	free (getBack);

	keySetName (key, "user/noname");
	succeed_if(keyGetNameSize(key) == 12, "size not correct after keySetName");
	getBack = malloc (12);
	succeed_if(keyGetName(key, getBack, 12), "could not get name");
	succeed_if(strcmp(getBack, "user/noname") == 0, "did not get correct value back");
	free (getBack);

	keySetName (key, "no");
	succeed_if(keyGetNameSize(key) == 1, "size not correct after keySetName");
	getBack = malloc (1);
	succeed_if(keyGetName(key, getBack, 1), "could not get name");
	succeed_if(strcmp(getBack, "") == 0, "did not get correct value back");
	free (getBack);
	keyDel (key);

	key = keyNew("user/noname", KEY_END);
	keySetName(key,"");
	succeed_if (strcmp (keyName(key), "") == 0, "keyName should be "" string");
	succeed_if (keyGetName(key,ret, 999) == 1, "keyGetName should return 1");
	succeed_if (strcmp (ret, "") == 0, "\\0 should be first character in array");
	succeed_if (keyGetNameSize(key) == 1, "empty name size" );

	keySetName(key,"user//hidden");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Two slashes in name");
	succeed_if (keyGetNameSize(key) == 12, "name size minus slashes" );

	keySetName(key,"user///hidden");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Three slashes in name");
	succeed_if (keyGetNameSize(key) == 12, "name size minus slashes" );

	keySetName(key,"user////////////////////////////////////hidden");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Many slashes in name");
	succeed_if (keyGetNameSize(key) == 12, "name size minus slashes" );

	printf("Test trailing Slashes in Key Name\n");
	keySetName(key,"user//hidden/");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Trailing Slashes");
	succeed_if (keyGetNameSize(key) == 12, "name size minus slashes" );

	keySetName(key,"user//hidden//");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Trailing Slashes");
	succeed_if (keyGetNameSize(key) == 12, "name size minus slashes" );

	keySetName(key,"user//hidden///////");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Trailing Slashes");
	succeed_if (keyGetNameSize(key) == 12, "name size minus slashes" );

	keySetName(key,"user/");
	// printf ("Name: %s\n", keyName(key));
	succeed_if (strcmp (keyName(key), "user") == 0, "Trailing Slashes");

	keySetName(key,"user/a");
	succeed_if (strcmp (keyName(key), "user/a") == 0, "Trailing Slashes: One letter problem");

	keySetName(key,"user//");
	succeed_if (strcmp (keyName(key), "user") == 0, "Trailing Slashes");

	keySetName(key,"user/////////");
	succeed_if (strcmp (keyName(key), "user") == 0, "Trailing Slashes");

	printf("Test Dots in Key Name\n");
	keySetName(key,"user/hidden/.");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Name Problem: Dot as basename");

	keySetName(key,"user/.");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: Dot in Middle");

	keySetName(key,"user/./hidden");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Name Problem: Dot in Middle");

	keySetName(key,"user/.valid/.");
	succeed_if (strcmp (keyName(key), "user/.valid") == 0, "Name Problem: Dot as basename (with comment)");

	keySetName(key,"user/./.valid");
	succeed_if (strcmp (keyName(key), "user/.valid") == 0, "Name Problem: Dot in Middle (with comment)");

	keySetName(key,"user/./.valid/.");
	succeed_if (strcmp (keyName(key), "user/.valid") == 0, "Name Problem: More dots");

	keySetName(key,"user/././././.valid/././././.");
	succeed_if (strcmp (keyName(key), "user/.valid") == 0, "Name Problem: Much more dots");

	printf("Test Double Dots in Key Name\n");
	keySetName(key,"user/hidden/parent/..");
	succeed_if (strcmp (keyName(key), "user/hidden") == 0, "Name Problem: Double Dot as basename");

	keySetName(key,"user/hidden/..");
	// printf ("Name: %s\n", keyName(key));
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: Double Dot as basename");

	keySetName(key,"user/..");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: Can't go higher then user in hierachy");

	keySetName(key,"user/hidden/../..");
	succeed_if (strcmp (keyName(key), "user") == 0, "Name Problem: Can't go higher then user in hierachy");

	succeed_if (keySetName(key, "user///sw/../sw//././MyApp")==sizeof("user/sw/MyApp"), "could not set keySet example");
	succeed_if (strcmp (keyName(key), "user/sw/MyApp") == 0, "Example of keySet does not work");
	succeed_if (keyGetNameSize(key) == sizeof("user/sw/MyApp"), "incorrect length for keySet example");

	printf("Test Mixed Dots and Slashes in Key Name\n");
	keySetName(key,"user/hidden/../.");

	keyDel (key);


	printf("Test failure key creation\n");

	key = keyNew ("invalid", KEY_END);
	succeed_if (key!=0, "null pointer for invalid name");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");

	key = keyNew("nonhere/valid/there", KEY_END);
	succeed_if (key != NULL, "keyNew did not accept wrong name");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");

	key = keyNew("nonhere:y/valid/there", KEY_END);
	succeed_if (key != NULL, "keyNew did not accept wrong name");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");

	key = keyNew ("user/validname", KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name");
	succeed_if(strcmp(keyName(key), "user/validname") == 0, "keyNew: Key's name setted incorrectly");

	keySetName(key, "invalid");
	succeed_if (keyGetNameSize(key) == 1, "name size for invalid name");
	succeed_if(strcmp(keyName(key), "") == 0, "keyNew: Key's name setted incorrectly");

	keySetName (key, "user/validname");
	succeed_if(strcmp(keyName(key), "user/validname") == 0, "keyNew: Key's name setted incorrectly");

	keySetName(key, "");
	succeed_if (keyGetNameSize(key) == 1, "name size for invalid name");
	succeed_if(strcmp(keyName(key), "") == 0, "keyNew: Key's name setted incorrectly");

	keySetName (key, "user/validname\\/t");
	succeed_if(strcmp(keyName(key), "user/validname\\/t") == 0, "keyNew: Key's name setted incorrectly");

	keySetName(key, 0);
	succeed_if (keyGetNameSize(key) == 1, "name size for invalid name");
	succeed_if(strcmp(keyName(key), "") == 0, "keyNew: Key's name setted incorrectly");

	keySetName (key, "user/validname\\");
	succeed_if(strcmp(keyName(key), "user/validname\\") == 0, "keyNew: Key's name setted incorrectly");

	keySetName (key, "user/validname\\/");
	succeed_if(strcmp(keyName(key), "user/validname\\/") == 0, "keyNew: Key's name setted incorrectly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name");

	printf("Test key's name manipulation\n");
	for(i = 0 ; tstKeyName[i].testName != NULL ; i++) {
		key = keyNew(tstKeyName[i].keyName, KEY_END);

		/* keyName */
		succeed_if( (strcmp(keyName(key), tstKeyName[i].expectedKeyName) == 0) , "keyName" );

		/* keyBaseName */
		succeed_if( (strcmp(keyBaseName(key), tstKeyName[i].expectedBaseName) == 0), "keyBaseName" );

		/* keyGetFullRootNameSize */
		size = keyGetFullRootNameSize(key);
		succeed_if( (size == elektraStrLen(tstKeyName[i].expectedFRootName)), "keyGetFullRootNameSize" );
		
		/* keyGetFullRootName */
		buf = elektraMalloc(size*sizeof(char));
		keyGetFullRootName(key, buf, size);
		// printf ("comp: %s - %s\n", buf, tstKeyName[i].expectedFRootName);
		succeed_if( (strncmp(buf, tstKeyName[i].expectedFRootName, size) == 0), "keyGetFullRootName" );
		free(buf);

		/* keyGetParentNameSize */
		size = keyGetParentNameSize(key);
		succeed_if( (size == elektraStrLen(tstKeyName[i].expectedParentName)), "ketGetParentNameSize" );

		/* keyGetParentName */
		size = keyGetParentNameSize(key)+1;
		buf = elektraMalloc(size*sizeof(char));
		keyGetParentName(key, buf, size);
		succeed_if( (strncmp(buf, tstKeyName[i].expectedParentName, size) == 0), "keyGetParentName" );
		free(buf);

		/* keyGetBaseNameSize */
		size = keyGetBaseNameSize(key);
		succeed_if( (size == elektraStrLen(tstKeyName[i].expectedBaseName)), "keyGetBaseNameSize" );

		/* keyGetBaseName */
		size = keyGetBaseNameSize(key)+1;
		buf = elektraMalloc(size*sizeof(char));
		keyGetBaseName(key, buf, size);
		succeed_if( (strncmp(buf, tstKeyName[i].expectedBaseName, size) == 0), "keyGetBaseName" );
		free(buf);

		/* keyGetNameSize */
		size = keyGetNameSize(key);
		succeed_if( (size == elektraStrLen(tstKeyName[i].expectedKeyName)), "keyGetKeyNameSize" );
		
		/* keyGetName */
		size = keyGetNameSize(key);
		buf = elektraMalloc(size*sizeof(char));
		keyGetName(key, buf, size);
		succeed_if( (strcmp(buf, tstKeyName[i].expectedKeyName) == 0), "keyGetName" );
		free(buf);

		succeed_if (keyGetRef (copy) == 0, "reference of copy not correct");
		keyCopy (copy, key);
		compare_key (copy, key);
		succeed_if (keyGetRef (copy) == 0, "reference of copy not correct");

		keyDel(key);
	}
	keyDel (copy);
}


void test_keyValue()
{
	Key * key;
	char	ret [1000];
	size_t i;
	char testString[] = "teststring";
	char testBinary[] = "\0tes\1tbinary";
	testBinary[sizeof(testBinary)-1] = 'T';

	printf("Test value of keys\n");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keyGetValueSize (key) == 1, "empty value size");
	succeed_if (keySetString (key,"perfectvalue") == 13, "could not set string");
	succeed_if (keyGetValueSize (key) == 13, "value size not correct");
	succeed_if (strcmp (keyValue(key), "perfectvalue") == 0, "String not same as set");
	succeed_if (keySetString (key,"perfectvalue") == 13, "could not re-set same string");
	succeed_if (strcmp (keyValue(key), "perfectvalue") == 0, "String not same as set");
	succeed_if (keySetString (key,"nearperfectvalue") == 17, "could not re-set other string");
	succeed_if (keyGetValueSize (key) == 17, "value size not correct");
	succeed_if (strcmp (keyValue(key), "nearperfectvalue") == 0, "String not same as set");
	succeed_if (keyGetString (key, ret, keyGetValueSize (key)>=999 ? 999 : keyGetValueSize (key))
			== 17, "could not get string");
	succeed_if (strcmp (ret, "nearperfectvalue") == 0, "String not same as set");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (strcmp (keyValue(key), "") == 0, "Empty value problem");
	succeed_if (keyGetValueSize(key) == 1, "Empty value size problem");
	succeed_if (keySetString (key,"") == 1, "could not set empty string");
	succeed_if (strcmp (keyValue(key), "") == 0, "Empty value problem");
	succeed_if (keyGetValueSize(key) == 1, "Empty value size problem");
	succeed_if (keyGetString(key, ret, 0) == -1, "Could not get empty value");
	succeed_if (keyGetString(key, ret, 1) == 1, "Could not get empty value");
	succeed_if (ret[0] == 0, "keyGetValue did not return empty value");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keySetString (key, "a long long string") == 19, "could not set string");
	succeed_if (keyGetString (key, ret, 6) == -1, "string not truncated");
	succeed_if (keyGetBinary (key, ret, 999) == -1, "binary not mismatch");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keySetBinary (key, "a", 1) == 1, "could not set binary");
	succeed_if (keyIsString (key) == 0, "is not a string");
	succeed_if (keyIsBinary (key) == 1, "is not a string");
	succeed_if (keyGetBinary (key, ret, 1) == 1, "binary not truncated");
	succeed_if (!strncmp(ret, "a", 1), "binary value wrong");
	succeed_if (keyGetString (key, ret, 999) == -1, "string not mismatch");
	succeed_if (keySetString (key, 0) == 1, "wrong error code for SetString");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keySetBinary (key, NULL, 0) == 0, "could not set null binary");
	succeed_if (keyIsString (key) == 0, "is not a string");
	succeed_if (keyIsBinary (key) == 1, "is not a string");
	succeed_if (keyGetValueSize(key) == 0, "Empty value size problem");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keySetString (key, "") == 1, "could not set empty string");
	succeed_if (keyIsString (key) == 1, "is not a string");
	succeed_if (keyIsBinary (key) == 0, "is a binary");
	succeed_if (keyGetValueSize(key) == 1, "Empty value size problem");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keySetBinary (key, "a long long binary", 19) == 19, "could not set string");
	succeed_if (keyIsString (key) == 0, "is not a string");
	succeed_if (keyIsBinary (key) == 1, "is not a string");
	succeed_if (keyGetBinary (key, ret, 6) == -1, "binary not truncated");
	succeed_if (keyGetBinary (key, ret, 19) == 19, "could not get binary");
	succeed_if (!strncmp(ret, "a long long binary", 19), "binary value wrong");
	succeed_if (keyGetString (key, ret, 999) == -1, "string not mismatch");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	for (i=1; i<255; i++)
	{
		ret[0] =  i; ret[1] = i; ret[2] = 0;
		//output_key (key);
		succeed_if (keySetString (key,ret) == 3, "could not set string");
		succeed_if (strcmp (keyValue(key), ret) == 0, "String not same as set");
	}
	succeed_if (keyDel (key) == 0, "could not delete key");


	succeed_if (key = keyNew(0), "could not create new key");
	for (i=0; i<255; i++)
	{
		ret[0] =  i; ret[1] = 255-i; ret[2] = i;
		//output_key (key);
		succeed_if (keySetBinary(key,ret,3) == 3, "could not set string");
		succeed_if (memcmp (keyValue(key), ret, 3) == 0, "String not same as set");
	}
	succeed_if (keyDel (key) == 0, "could not delete key");



	printf ("Test string of key\n");

	succeed_if (keyValue(0) == 0, "null pointer");
	succeed_if (keyGetValueSize(0) == -1, "null pointer");
	succeed_if (keySetString(0,"") == -1, "null pointer");

	key = keyNew(0);
	succeed_if (keyGetValueSize(key) == 1, "empty value size");

	keySetString (key, testString);
	succeed_if (keyGetString (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetString (key,0,100) == -1, "string null pointer");
	succeed_if (keyGetString (key,ret,0) == -1, "length checking");

	for (i=1; i< sizeof(testString);i++)
	{
		succeed_if (keyGetString (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testString); i<sizeof(testString)*2; i++)
	{
		succeed_if (keyGetString (key,ret,i) == sizeof(testString), "length checking longer");
	}
	succeed_if (keyGetString (key,ret, (size_t)-1) == -1, "maxSize exceeded");

	succeed_if (keySetString(key,0) == 1, "delete string");
	succeed_if (keyGetString (key,ret,i) == 1, "length checking deleting");
	succeed_if (strcmp(ret, "") == 0, "not empty string");

	succeed_if (keySetString(key,testString) == sizeof(testString), "set string");
	succeed_if (keyGetString (key,ret,i) == sizeof(testString), "length checking working");
	succeed_if (strcmp(ret, testString) == 0, "not empty string");

	succeed_if (keySetString(key,"") == 1, "delete string");
	succeed_if (keyGetString (key,ret,i) == 1, "length checking deleting");
	succeed_if (strcmp(ret, "") == 0, "not empty string");

	succeed_if (keySetString(key,testString) == sizeof(testString), "set string");
	succeed_if (keyGetString (key,ret,i) == sizeof(testString), "length checking working");
	succeed_if (strcmp(ret, testString) == 0, "not empty string");

	succeed_if (keyGetValueSize(key) == sizeof(testString), "testString value size");
	succeed_if (strncmp(keyValue(key), testString, sizeof(testString)) == 0, "testString not same");
	keyDel (key);



	printf ("Test binary of key\n");

	succeed_if (keyValue(0) == 0, "null pointer");
	succeed_if (keyGetValueSize(0) == -1, "null pointer");
	succeed_if (keySetBinary(0,"",1) == -1, "null pointer");

	key = keyNew(0);
	succeed_if (keySetBinary(key,"",0) == -1, "null size");
	succeed_if (keySetBinary(key,"b",0) == -1, "null size");
	succeed_if (keySetBinary(key,"",SIZE_MAX) == -1, "max size");
	succeed_if (keySetBinary(key,"b",SIZE_MAX) == -1, "max size");
	succeed_if (keyGetValueSize(key) == 1, "empty value size");

	keySetBinary (key, testBinary, sizeof(testBinary));
	succeed_if (keyGetBinary (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetBinary (key,0,100) == -1, "binary null pointer");
	succeed_if (keyGetBinary (key,ret,0) == -1, "length checking");

	for (i=1; i< sizeof(testBinary);i++)
	{
		succeed_if (keyGetBinary (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testBinary); i<sizeof(testBinary)*2; i++)
	{
		succeed_if (keyGetBinary (key,ret,i) == sizeof(testBinary), "length checking longer");
	}
	succeed_if (keyGetBinary (key,ret, (size_t)-1) == -1, "maxSize exceeded");

	succeed_if (keySetBinary(key,0,0) == 0, "delete binary");
	succeed_if (keyGetBinary (key,ret,i) == 0, "length checking deleting");

	succeed_if (keySetBinary(key,testBinary, sizeof(testBinary)) == sizeof(testBinary), "set binary");
	succeed_if (keyGetBinary (key,ret,i) == sizeof(testBinary), "length checking working");
	succeed_if (strcmp(ret, testBinary) == 0, "not empty binary");

	succeed_if (keySetBinary(key,0,1) == 0, "delete binary");
	succeed_if (keyGetBinary (key,ret,i) == 0, "length checking deleting");

	succeed_if (keySetBinary(key,testBinary, sizeof(testBinary)) == sizeof(testBinary), "set binary");
	succeed_if (keyGetBinary (key,ret,i) == sizeof(testBinary), "length checking working");
	succeed_if (strcmp(ret, testBinary) == 0, "not empty binary");

	succeed_if (keySetBinary(key,"",1) == 1, "delete binary the string way");
	succeed_if (keyGetBinary (key,ret,i) == 1, "length checking deleting string way");
	succeed_if (strcmp(ret, "") == 0, "not empty binary the string way");

	succeed_if (keySetBinary(key,testBinary, sizeof(testBinary)) == sizeof(testBinary), "set binary");
	succeed_if (keyGetBinary (key,ret,i) == sizeof(testBinary), "length checking working");
	succeed_if (strcmp(ret, testBinary) == 0, "not empty binary");

	succeed_if (keyGetValueSize(key) == sizeof(testBinary), "testBinary value size");
	succeed_if (strncmp(keyValue(key), testBinary, sizeof(testBinary)) == 0, "testBinary not same");
	keyDel (key);

}

void test_keyBinary(void)
{
	Key *key =0;
	char ret [1000];
	int i;
	char binaryData[] = "\0binary \1\34data";
	binaryData[sizeof(binaryData)-1] = 'T';

	printf ("Test binary special cases\n");

	key = keyNew ("user/binary",
		KEY_BINARY,
		KEY_SIZE, sizeof(binaryData),
		KEY_VALUE, binaryData,
		KEY_END);

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == sizeof(binaryData), "size not correct");
	succeed_if (memcmp(binaryData, keyValue(key), sizeof(binaryData)) == 0, "memcmp");
	succeed_if (keyGetBinary(key, ret, 1000) == sizeof(binaryData), "could not get binary data");
	succeed_if (memcmp(binaryData, ret, sizeof(binaryData)) == 0, "memcmp");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	keyDel (key);

	key = keyNew(0);
	keySetBinary (key, binaryData, sizeof(binaryData));

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == sizeof(binaryData), "size not correct");
	succeed_if (memcmp(binaryData, keyValue(key), sizeof(binaryData)) == 0, "memcmp");
	succeed_if (keyGetBinary(key, ret, 1000) == sizeof(binaryData), "could not get binary data");
	succeed_if (memcmp(binaryData, ret, sizeof(binaryData)) == 0, "memcmp");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	keyDel (key);

	key = keyNew(0);
	keySetBinary(key, 0, 0);

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == 0, "size not correct");
	succeed_if (keyValue(key) == 0, "should be null pointer");
	succeed_if (keyGetBinary(key, ret, 1000) == 0, "should write nothing because of no data");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	keyDel (key);

	key = keyNew(0);
	keySetBinary(key, 0, 1);

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == 0, "size not correct");
	succeed_if (keyValue(key) == 0, "should be null pointer");
	succeed_if (keyGetBinary(key, ret, 1000) == 0, "should write nothing because of no data");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	keyDel (key);

	key = keyNew(0);
	keySetBinary(key, "", 1);
	succeed_if (keySetBinary(key, 0, SIZE_MAX) == -1, "should do nothing and fail");
	succeed_if (keySetBinary(key, 0, SSIZE_MAX) == 0, "should free data");

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == 0, "size not correct");
	succeed_if (keyValue(key) == 0, "should be null pointer");
	succeed_if (keyGetBinary(key, ret, 1000) == 0, "should write nothing because of no data");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	keyDel (key);

	key = keyNew(0);
	keySetBinary(key, "", 1);

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == 1, "size not correct");
	succeed_if (memcmp(binaryData, keyValue(key), 1) == 0, "memcmp");
	succeed_if (keyGetBinary(key, ret, 1000) == 1, "could not get binary data");
	succeed_if (memcmp(binaryData, ret, 1) == 0, "memcmp");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	keyDel (key);

	key = keyNew(0);
	i = 23;
	keySetBinary (key, (void*)&i, sizeof(i));

	succeed_if (keyIsBinary(key) == 1, "should be binary");
	succeed_if (keyIsString(key) == 0, "should not be string");
	succeed_if (keyGetValueSize(key) == sizeof(i), "size not correct");
	succeed_if (memcmp((void*)&i, keyValue(key), sizeof(i)) == 0, "memcmp");
	succeed_if (keyGetBinary(key, ret, 1000) == sizeof(i), "could not get binary data");
	succeed_if (memcmp((void*)&i, ret, sizeof(i)) == 0, "memcmp");
	succeed_if (keyGetString(key, ret, 1000) == -1, "should be type mismatch");

	i = *(int*)keyValue(key);
	succeed_if (i==23, "incorrect int");

	keyDel (key);

}

void test_keyComment()
{
	Key * key;
	char	ret [1000];
	size_t i;
	char testComment [] = "testcomment";

	printf("Test comment of key\n");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keyGetCommentSize (key) == 1, "empty comment size");
	succeed_if (keySetComment (key,"perfectcomment") == 15, "could not set comment");
	succeed_if (keyGetCommentSize (key) == 15, "comment size not correct");
	succeed_if (strcmp (keyComment(key), "perfectcomment") == 0, "Comment not same as set");
	succeed_if (keySetComment (key,"perfectcomment") == 15, "could not re-set same comment");
	succeed_if (strcmp (keyComment(key), "perfectcomment") == 0, "Comment not same as set");
	succeed_if (keySetComment (key,"nearperfectcomment") == 19, "could not re-set other comment");
	succeed_if (keyGetCommentSize (key) == 19, "comment size not correct");
	succeed_if (strcmp (keyComment(key), "nearperfectcomment") == 0, "Comment not same as set");
	succeed_if (keyGetComment (key, ret, keyGetCommentSize (key)>=999 ? 999 : keyGetCommentSize (key))
			== 19, "could not get comment");
	succeed_if (strcmp (ret, "nearperfectcomment") == 0, "Comment not same as set");
	succeed_if (keyDel (key) == 0, "could not delete key");
	
	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (strcmp (keyComment(key), "") == 0, "Empty comment problem");
	succeed_if (keyGetCommentSize(key) == 1, "Empty comment size problem");
	succeed_if (keySetComment (key,"") == 1, "could not set comment");
	succeed_if (strcmp (keyComment(key), "") == 0, "Empty comment problem");
	succeed_if (keyGetCommentSize(key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment(key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment(key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");
	succeed_if (keyDel (key) == 0, "could not delete key");
	
	succeed_if (key = keyNew(0), "could not create new key");
	for (i=1; i<255; i++)
	{
		ret[0] = i; ret[1] = i; ret[2] = 0;
		succeed_if (keySetComment (key,ret) == 3, "could not set comment");
		// output_key (key);
		succeed_if (strcmp (keyComment(key), ret) == 0, "Comment not same as set");
	}
	succeed_if (keyDel (key) == 0, "could not delete key");



	printf ("Test comment of key 2\n");

	succeed_if (keyComment(0) == 0, "null pointer");
	succeed_if (keyGetCommentSize(0) == -1, "null pointer");
	succeed_if (keySetComment(0,"") == -1, "null pointer");

	key = keyNew(0);
	succeed_if (keyGetCommentSize(key) == 1, "empty comment size");

	keySetComment (key, testComment);
	succeed_if (keyGetComment (0,ret,100) == -1, "null pointer");
	succeed_if (keyGetComment (key,0,100) == -1, "comment null pointer");
	succeed_if (keyGetComment (key,ret,0) == -1, "length checking");

	for (i=1; i< sizeof(testComment);i++)
	{
		succeed_if (keyGetComment (key,ret,i) == -1, "length checking too short");
	}
	for (i=sizeof(testComment); i<sizeof(testComment)*2; i++)
	{
		succeed_if (keyGetComment (key,ret,i) == sizeof(testComment), "length checking longer");
	}
	succeed_if (keyGetComment (key,ret, (size_t)-1) == -1, "maxSize exceeded");

	succeed_if (keySetComment(key,0) == 1, "delete comment");
	succeed_if (keyGetComment (key,ret,i) == 1, "length checking deleting");
	succeed_if (strcmp(ret, "") == 0, "not empty comment");

	succeed_if (keySetComment(key,testComment) == sizeof(testComment), "set comment");
	succeed_if (keyGetComment (key,ret,i) == sizeof(testComment), "length checking working");
	succeed_if (strcmp(ret, testComment) == 0, "not empty comment");

	succeed_if (keySetComment(key,"") == 1, "delete comment");
	succeed_if (keyGetComment (key,ret,i) == 1, "length checking deleting");
	succeed_if (strcmp(ret, "") == 0, "not empty comment");

	succeed_if (keySetComment(key,testComment) == sizeof(testComment), "set comment");
	succeed_if (keyGetComment (key,ret,i) == sizeof(testComment), "length checking working");
	succeed_if (strcmp(ret, testComment) == 0, "not empty comment");

	succeed_if (keyGetCommentSize(key) == sizeof(testComment), "testComment comment size");
	succeed_if (strncmp(keyComment(key), testComment, sizeof(testComment)) == 0, "testComment not same");
	keyDel (key);

}

void test_keyOwner()
{
	Key * key;
	char	ret [1000];
	int i;
	printf("Test owner of keys\n");
	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keyGetOwnerSize (key) == 1, "empty owner size");
	succeed_if (keySetOwner (key,"perfectowner") == 13, "could not set owner");
	succeed_if (keyGetOwnerSize (key) == 13, "owner size not correct");
	succeed_if (strcmp (keyOwner(key), "perfectowner") == 0, "Owner not same as set");
	succeed_if (keySetOwner (key,"perfectowner") == 13, "could not re-set same owner");
	succeed_if (strcmp (keyOwner(key), "perfectowner") == 0, "Owner not same as set");
	succeed_if (keySetOwner (key,"nearperfectowner") == 17, "could not re-set other owner");
	succeed_if (keyGetOwnerSize (key) == 17, "owner size not correct");
	succeed_if (strcmp (keyOwner(key), "nearperfectowner") == 0, "Owner not same as set");
	succeed_if (keyGetOwner (key, ret, keyGetOwnerSize (key)>=999 ? 999 : keyGetOwnerSize (key))
			== 17, "could not get owner");
	succeed_if (strcmp (ret, "nearperfectowner") == 0, "Owner not same as set");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (keySetName(key, "user:perfectowner") == 5, "could not set to user with owner");
	succeed_if (keyGetOwnerSize (key) == 13, "owner size not correct");
	succeed_if (strcmp (keyOwner(key), "perfectowner") == 0, "Owner not same as set");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew(0), "could not create new key");
	succeed_if (strcmp (keyOwner(key), "") == 0, "Empty owner problem");
	succeed_if (keyGetOwnerSize(key) == 1, "Empty owner size problem");
	succeed_if (keySetOwner (key,"") == 1, "could not set owner");
	succeed_if (strcmp (keyOwner(key), "") == 0, "Empty owner problem");
	succeed_if (keyGetOwnerSize(key) == 1, "Empty owner size problem");
	succeed_if (keyGetOwner(key, ret, 0) == -1, "Could not get empty owner");
	succeed_if (keyGetOwner(key, ret, 1) == 1, "Could not get empty owner");
	succeed_if (ret[0] == 0, "keyGetOwner did not return empty owner");
	succeed_if (keyDel (key) == 0, "could not delete key");
	
	succeed_if (key = keyNew(0), "could not create new key");
	for (i=1; i<255; i++)
	{
		ret[0] = i; ret[1] = i; ret[2] = 0;
		succeed_if (keySetOwner (key,ret) == 3, "could not set owner");
		// output_key (key);
		succeed_if (strcmp (keyOwner(key), ret) == 0, "Owner not same as set");
	}
	succeed_if (keyDel (key) == 0, "could not delete key");
}

void test_keyInactive ()
{
	Key * key = keyNew(0);

	succeed_if (keyIsInactive(0) == -1, "NULL pointer");
	succeed_if (keyIsInactive(key) == -1, "Key has no name");

	printf("Test of active and inactive keys\n");
	keySetName(key,"user/valid");
	succeed_if (!keyIsInactive(key), "Key should not be inactive");

	keySetName(key,"user/.hidden/valid");
	succeed_if (!keyIsInactive(key), "Key should not be inactive");
	
	keySetName(key,"user/.hidden/€valid");
	succeed_if (!keyIsInactive(key), "Key should not be inactive");
	
	keySetName(key,"user/hidden");
	succeed_if (!keyIsInactive(key), "Key should not be inactive");
	
	keySetName(key,"user/.hidden");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	
	keySetName(key,"user/.valid/.hidden");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	
	keySetName(key,"user/.valid/.:hidden");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	
	keySetName(key,"user/.valid/.€hidden");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	
	keySetName(key,"user/.HiddenStringKey");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	
	keySetName(key,"user/.HiddenDirectoryKey");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	
	keySetName(key,"user/tests/file8xdLVS/filesys/.HiddenStringKey");
	succeed_if (keyIsInactive(key), "Key should be inactive");
	keyDel (key);
}

void test_keyBelow()
{
	Key * key1 = keyNew(0);
	Key * key2 = keyNew(0);

	printf("Test of relative positions of keys\n");

	succeed_if (keyIsBelow(key1,0) == -1, "NULL pointer");
	succeed_if (keyIsBelow(0,0) == -1, "NULL pointer");
	succeed_if (keyIsBelow(0,key1) == -1, "NULL pointer");

	succeed_if (keyIsDirectBelow(key1,0) == -1, "NULL pointer");
	succeed_if (keyIsDirectBelow(0,0) == -1, "NULL pointer");
	succeed_if (keyIsDirectBelow(0,key1) == -1, "NULL pointer");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid");
	succeed_if (!keyIsBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/");
	succeed_if (!keyIsBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/below");
	succeed_if (keyIsBelow(key1,key2), "Key should be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/b");
	succeed_if (keyIsBelow(key1,key2), "Key should be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/b");
	succeed_if (keyIsBelow(key1,key2), "Key should be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/b/e");
	succeed_if (keyIsBelow(key1,key2), "Key should be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valide");
	keySetName(key2,"user/valid/e");
	succeed_if (!keyIsBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid/b");
	keySetName(key2,"user/valid/e");
	succeed_if (!keyIsBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valide");
	keySetName(key2,"user/valid/valide");
	succeed_if (!keyIsBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/valide");
	succeed_if (keyIsDirectBelow(key1,key2), "Key should be below");
	succeed_if (!keyIsDirectBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/non/valid");
	succeed_if (!keyIsDirectBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsDirectBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid/a");
	keySetName(key2,"user/valid/b");
	succeed_if (!keyIsDirectBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsDirectBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid");
	succeed_if (!keyIsDirectBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsDirectBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid/a/b");
	keySetName(key2,"user/valid");
	succeed_if (!keyIsDirectBelow(key1,key2), "Key should not be below");
	succeed_if (!keyIsDirectBelow(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid/a");
	keySetName(key2,"user/valid");
	succeed_if (!keyIsDirectBelow(key1,key2), "Key should not be below");
	succeed_if (keyIsDirectBelow(key2,key1), "Key should be below");


	keyDel (key1);
	keyDel (key2);
}

void test_keyDup()
{
	Key     *orig, *copy;

	printf("Test key duplication\n");

	// Create test key
	orig = keyNew("user:yl/foo/bar",
			KEY_BINARY,
			KEY_SIZE, 6,
			KEY_VALUE, "foobar",
			KEY_COMMENT, "mycomment", 
			KEY_UID, 123,
			KEY_GID, 456,
			KEY_MODE, 0644,
			KEY_END);



	// Dup the key
	succeed_if( (copy = keyDup(orig)) != 0, "keyDup failed");
	succeed_if (keyGetATime(orig) == keyGetATime(copy), "ATime should be same");
	succeed_if (keyGetCTime(orig) == keyGetCTime(copy), "CTime should be same");
	succeed_if (keyGetMTime(orig) == keyGetMTime(copy), "MTime should be same");
	succeed_if (keyGetRef(orig) == 0, "orig ref counter should be 0");
	succeed_if (keyGetRef(copy) == 0, "copy ref counter should be 0");
	keyDel(orig); // everything independent from original!

	// Check the duplication
	succeed_if( strcmp(keyName(copy), "user/foo/bar") == 0, "keyDup: key name copy error");
	succeed_if( strcmp(keyOwner(copy), "yl") == 0, "keyDup: key name owner copy error");
	succeed_if( strncmp(keyValue(copy), "foobar", 6) == 0, "keyDup: key value copy error");
	succeed_if( strcmp(keyComment(copy), "mycomment") == 0, "keyDup: key comment copy error");
	succeed_if( keyGetUID(copy) == 123, "keyDup: key UID copy error");
	succeed_if( keyGetGID(copy) == 456, "keyDup: key GID copy error");
	succeed_if( keyGetMode(copy) == 0644, "keyDup: key mode copy error");
	succeed_if (keyIsBinary(copy), "keyDup: key type copy error");

	keyDel(copy);

	orig = keyNew (0);
	keySetName (orig, "invalid");

	succeed_if ( (copy = keyDup(orig)) != 0, "keyDup failed");
	succeed_if ( strcmp(keyName(orig), "") == 0, "orig name should be empty");
	succeed_if ( strcmp(keyName(copy), "") == 0, "copy name should be empty");
	succeed_if (keyGetNameSize(orig) == 1, "orig name size");
	succeed_if (keyGetNameSize(copy) == 1, "orig name size");
	succeed_if ( strcmp(keyOwner(orig), "") == 0, "orig name should be empty");
	succeed_if ( strcmp(keyOwner(copy), "") == 0, "copy name should be empty");
	succeed_if (keyGetOwnerSize(orig) == 1, "orig name size");
	succeed_if (keyGetOwnerSize(copy) == 1, "copy name size");
	succeed_if (keyGetATime(orig) == keyGetATime(copy), "ATime should be same");
	succeed_if (keyGetCTime(orig) == keyGetCTime(copy), "CTime should be same");
	succeed_if (keyGetMTime(orig) == keyGetMTime(copy), "MTime should be same");
	succeed_if (keyGetRef(orig) == 0, "orig ref counter should be 0");
	succeed_if (keyGetRef(copy) == 0, "copy ref counter should be 0");

	keyDel (orig);
	keyDel (copy);
}

void test_keyCopy()
{
	Key     *orig, *copy;

	printf("Test key copy\n");

	// Create test key
	orig = keyNew("user:yl/foo/bar",
			KEY_BINARY,
			KEY_SIZE, 6,
			KEY_VALUE, "foobar",
			KEY_COMMENT, "mycomment", 
			KEY_UID, 123,
			KEY_GID, 456,
			KEY_MODE, 0644,
			KEY_END);



	// Copy the key
	copy = keyNew(0);
	succeed_if( keyCopy(copy, orig) == 1, "keyCopy failed");
	succeed_if (keyGetATime(orig) == keyGetATime(copy), "ATime should be same");
	succeed_if (keyGetCTime(orig) == keyGetCTime(copy), "CTime should be same");
	succeed_if (keyGetMTime(orig) == keyGetMTime(copy), "MTime should be same");
	succeed_if (keyGetRef(orig) == 0, "orig ref counter should be 0");
	succeed_if (keyGetRef(copy) == 0, "copy ref counter should be 0");
	keyDel(orig); // everything independent from original!

	// Check the duplication
	succeed_if( strcmp(keyName(copy), "user/foo/bar") == 0, "keyCopy: key name copy error");
	succeed_if( strcmp(keyOwner(copy), "yl") == 0, "keyCopy: key name owner copy error");
	succeed_if( strncmp(keyValue(copy), "foobar", 6) == 0, "keyCopy: key value copy error");
	succeed_if( strcmp(keyComment(copy), "mycomment") == 0, "keyCopy: key comment copy error");
	succeed_if( keyGetUID(copy) == 123, "keyCopy: key UID copy error");
	succeed_if( keyGetGID(copy) == 456, "keyCopy: key GID copy error");
	succeed_if( keyGetMode(copy) == 0644, "keyCopy: key mode copy error");

	orig = keyNew(0);
	succeed_if (keyCopy(copy, 0) == 0, "make the key copy fresh");

	compare_key (orig, copy);
	keyDel (orig);

	keyDel(copy);

	orig = keyNew (0);
	keySetName (orig, "invalid");

	copy = keyNew(0);
	succeed_if( keyCopy(copy, orig) == 1, "keyCopy failed");
	succeed_if ( strcmp(keyName(orig), "") == 0, "orig name should be empty");
	succeed_if ( strcmp(keyName(copy), "") == 0, "copy name should be empty");
	succeed_if (keyGetNameSize(orig) == 1, "orig name size");
	succeed_if (keyGetNameSize(copy) == 1, "orig name size");
	succeed_if ( strcmp(keyOwner(orig), "") == 0, "orig name should be empty");
	succeed_if ( strcmp(keyOwner(copy), "") == 0, "copy name should be empty");
	succeed_if (keyGetOwnerSize(orig) == 1, "orig name size");
	succeed_if (keyGetOwnerSize(copy) == 1, "copy name size");
	succeed_if (keyGetATime(orig) == keyGetATime(copy), "ATime should be same");
	succeed_if (keyGetCTime(orig) == keyGetCTime(copy), "CTime should be same");
	succeed_if (keyGetMTime(orig) == keyGetMTime(copy), "MTime should be same");
	succeed_if (keyGetRef(orig) == 0, "orig ref counter should be 0");
	succeed_if (keyGetRef(copy) == 0, "copy ref counter should be 0");

	keyDel (orig);
	keyDel (copy);
}

void test_keyDir (void)
{
	mode_t i;
	Key * key = keyNew ("user", KEY_END);

	printf ("Test directory keys\n");

	succeed_if (keyGetMode(key) == 0664, "new key not 0664 by default");
	succeed_if (keyIsDir (key) == 0, "new key should not be directory by default");

	succeed_if (keySetMode(key, 0644) == 0, "could not set to 0644");
	succeed_if (keyIsDir (key) == 0, "0644 should not be directory");
	succeed_if (keyGetMode(key) == 0644, "key is not 0644, but was set");

	succeed_if (keySetDir (key) == 0, "could not set directory key");
	succeed_if (keyIsDir (key) == 1, "should be directory after keySetDir");
	// succeed_if (keyGetMode(key) == 0755, "key is not 0644, but was set");

	for (i = 0; i <= 0777; i++)
	{
		succeed_if (keySetMode(key, i) == 0, "could not set to 0000 <= i <= 0777");
		succeed_if (keyGetMode(key) == i, "key is not correct 0000 <= i <= 0777");

		if (/*getuid() == keyGetUID (key) &&*/ (keyGetMode (key) & 0100))
		{
			succeed_if (keyIsDir (key) == 1, "should be directory because of executable and uid match");
		} else if (/*getuid() != keyGetUID (key) && getgid() == keyGetGID (key) &&*/ (keyGetMode (key) & 0010))
		{
			succeed_if (keyIsDir (key) == 1, "should be directory because of executable and gid match");
		} else if (/*getuid() != keyGetUID (key) && getgid() != keyGetGID (key) &&*/ (keyGetMode (key) & 0001))
		{
			succeed_if (keyIsDir (key) == 1, "should be directory because of executable and other match");
		} else {
			succeed_if (keyIsDir (key) == 0, "should not be directory");
		}
	
		succeed_if (keySetDir (key) == 0, "could not set directory key");
		succeed_if (keyIsDir (key) == 1, "should be directory after keySetDir");
	}
	keyDel (key);

	key = keyNew ("user", KEY_DIR, KEY_END);
	succeed_if (keyGetMode(key) == 0775, "new key with KEY_DIR not 0775 by default");
	succeed_if (keyIsDir (key) == 1, "new key with KEY_DIR should be directory by default");

	succeed_if (keySetMode(key, 0644) == 0, "could not set to 0644");
	succeed_if (keyIsDir (key) == 0, "0644 should not be directory");
	succeed_if (keyGetMode(key) == 0644, "key is not 0644, but was set");

	succeed_if (keySetDir (key) == 0, "could not set directory key");
	succeed_if (keyIsDir (key) == 1, "should be directory after keySetDir");
	// succeed_if (keyGetMode(key) == 0755, "key is not 0644, but was set");
	keyDel (key);

	key = keyNew ("user/s", KEY_DIR, KEY_MODE, 0444, KEY_END);
	succeed_if (keyGetMode(key) == 0444, "0444 set by keyNew");
	succeed_if (keyIsDir (key) == 0, "0444 should be directory");
	keyDel (key);
	
	key = keyNew ("user/s", KEY_MODE, 0444, KEY_DIR, KEY_END);
	// succeed_if (keyGetMode(key) == 0555, "0555 set by keyNew");
	succeed_if (keyIsDir (key) == 1, "0555 should be directory");
	keyDel (key);
}

void test_keyTime()
{
	Key * key = keyNew (KEY_END);
	time_t now = time(0);
	time_t past= now - 60*60*24*356 * 10;
	time_t future = now + 60*60*24*356 * 10;
	/*
	time_t far_future = now + 60L*60L*24L*356L * 100L;
	*/

	printf ("Test key time\n");

	succeed_if (keyGetATime(0) == (time_t)-1, "null pointer check");
	succeed_if (keyGetMTime(0) == (time_t)-1, "null pointer check");
	succeed_if (keyGetCTime(0) == (time_t)-1, "null pointer check");

	succeed_if (keySetATime(0,0) == -1, "null pointer check");
	succeed_if (keySetMTime(0,0) == -1, "null pointer check");
	succeed_if (keySetCTime(0,0) == -1, "null pointer check");

	succeed_if (keyGetATime(key) == 0, "new initialized atime not 0");
	succeed_if (keyGetMTime(key) == 0, "new initialized mtime not 0");
	succeed_if (keyGetCTime(key) == 0, "new initialized ctime not 0");

	succeed_if (keySetATime (key, now) == 0, "could not set atime");
	succeed_if (keySetMTime (key, now) == 0, "could not set mtime");
	succeed_if (keySetCTime (key, now) == 0, "could not set ctime");

	succeed_if (keyGetATime(key) == now, "new initialized atime not 0");
	succeed_if (keyGetMTime(key) == now, "new initialized mtime not 0");
	succeed_if (keyGetCTime(key) == now, "new initialized ctime not 0");


	succeed_if (keySetATime (key, past) == 0, "could not set atime");
	succeed_if (keySetMTime (key, past) == 0, "could not set mtime");
	succeed_if (keySetCTime (key, past) == 0, "could not set ctime");

	succeed_if (keyGetATime(key) == past, "new initialized atime not 0");
	succeed_if (keyGetMTime(key) == past, "new initialized mtime not 0");
	succeed_if (keyGetCTime(key) == past, "new initialized ctime not 0");


	succeed_if (keySetATime (key, future) == 0, "could not set atime");
	succeed_if (keySetMTime (key, future) == 0, "could not set mtime");
	succeed_if (keySetCTime (key, future) == 0, "could not set ctime");

	succeed_if (keyGetATime(key) == future, "new initialized atime not 0");
	succeed_if (keyGetMTime(key) == future, "new initialized mtime not 0");
	succeed_if (keyGetCTime(key) == future, "new initialized ctime not 0");

/*
	succeed_if (keySetATime (key, far_future) == 0, "could not set atime");
	succeed_if (keySetMTime (key, far_future) == 0, "could not set mtime");
	succeed_if (keySetCTime (key, far_future) == 0, "could not set ctime");

	succeed_if (keyGetATime(key) == far_future, "new initialized atime not 0");
	succeed_if (keyGetMTime(key) == far_future, "new initialized mtime not 0");
	succeed_if (keyGetCTime(key) == far_future, "new initialized ctime not 0");

	warn_if_fail (keyGetATime(key) > 0, "time_t not 64 bit, 2038 problem");
	warn_if_fail (keyGetMTime(key) > 0, "time_t not 64 bit, 2038 problem");
	warn_if_fail (keyGetCTime(key) > 0, "time_t not 64 bit, 2038 problem");
*/

	keyDel (key);
}

void test_keyMeta(void)
{
	Key *key=0;

	succeed_if(keyGetUID(key) == (uid_t)-1, "uid null pointer");
	succeed_if(keyGetGID(key) == (gid_t)-1, "gid null pointer");
	succeed_if(keyGetMode(key) == (mode_t)-1, "mode null pointer");

	key=0;
	succeed_if (keyNeedSync(key) == -1, "key needs sync");
	key = keyNew (0);
	succeed_if (keyNeedSync(key) == 0, "fresh key needs sync");
	keyDel (key);

	key = keyNew ("user/remove", KEY_END);
	succeed_if (keyNeedSync(key) == 1, "need sync");
	keyDel (key);

	succeed_if (keyNeedSync(0) == -1, "keyNeedSync(0)");

	key = keyNew(0);
	succeed_if (keyNeedSync(key) == 0, "keyNew(0) should not need sync");
	succeed_if (keySetName(key,"invalid") == -1, "invalid name should fail");
	succeed_if (keyNeedSync(key) == 0, "keyNew(0) should not need sync");
	keyDel (key);

	key = keyNew(0);
	succeed_if (keyNeedSync(key) == 0, "should not need sync");
	keySetUID(key,20);
	succeed_if (keyNeedSync(key) == 1, "should need sync");
	keyDel (key);

	key = keyNew(0);
	succeed_if(keyGetUID(key) == (uid_t)-1, "uid not set to nobody");
	succeed_if(keyGetGID(key) == (gid_t)-1, "gid not set to nobody");

	succeed_if(keySetUID(key,20) == 0, "could not set uid");
	succeed_if(keySetGID(key,21) == 0, "could not set uid");

	succeed_if(keyGetUID(key) == 20, "uid not set to 20");
	succeed_if(keyGetGID(key) == 21, "gid not set to 21");

	succeed_if(keySetUID(key,(uid_t)-1) == 0, "could not set uid");
	succeed_if(keySetGID(key,(gid_t)-1) == 0, "could not set uid");

	succeed_if(keyGetUID(key) == (uid_t)-1, "uid not set to nobody");
	succeed_if(keyGetGID(key) == (gid_t)-1, "gid not set to nobody");

	succeed_if(keySetUID(key,0) == 0, "could not set uid");
	succeed_if(keySetGID(key,0) == 0, "could not set uid");

	succeed_if(keyGetUID(key) == 0, "uid not set to 20");
	succeed_if(keyGetGID(key) == 0, "gid not set to 21");
	keyDel (key);

	key = keyNew(0);
	succeed_if(keyGetMode(key) == KDB_FILE_MODE, "new key does not have default mode");
	succeed_if(keySetDir(key) == 0, "could not set dir");
	succeed_if(keyGetMode(key) == (KDB_FILE_MODE | KDB_DIR_MODE), "directory key");
	keyDel (key);

	key = keyNew("user/dir", KEY_DIR, KEY_END);
	succeed_if(keyGetMode(key) == (KDB_FILE_MODE | KDB_DIR_MODE), "directory key");
	succeed_if(keySetDir(key) == 0, "could not set dir");
	succeed_if(keyGetMode(key) == (KDB_FILE_MODE | KDB_DIR_MODE), "directory key");
	keyDel (key);
}

void test_keyHelpers()
{
	char *name="user/abc/defghi/jkl";
	char *p;
	size_t size=0;
	int level=0;
	char buffer[20];
	
	Key *key=keyNew("system/parent/base",KEY_END);
	char *parentName;
	size_t parentSize;
	Key *k1, *k2;

	printf ("Test key helpers\n");

	/* copied out of example from keyNameGetOneLevel
	 Lets define a key name with a lot of repeating '/' and escaped '/'
	 char *keyName="user////abc/def\\/ghi////jkl///";*/

	p=name;
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		level++;

		strncpy(buffer,p,size);
		buffer[size]=0;

		/* printf("Level %d name: \"%s\"\n",level,buffer);*/
		switch (level)
		{
			case 1: succeed_if (strcmp (buffer, "user") == 0, "keyNameGetOneLevel not correct"); break;
			case 2: succeed_if (strcmp (buffer, "abc") == 0, "keyNameGetOneLevel not correct"); break;
			case 3: succeed_if (strcmp (buffer, "defghi") == 0, "keyNameGetOneLevel not correct"); break;
			case 4: succeed_if (strcmp (buffer, "jkl") == 0, "keyNameGetOneLevel not correct"); break;
			default: succeed_if (0, "should not reach case statement");
		}
	}

	/* with escaped sequence:*/
	name="user////abc/def\\/ghi////jkl///";
	size=0;
	level=0;

	p=name;
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		level++;

		strncpy(buffer,p,size);
		buffer[size]=0;

		/* printf("Level %d name: \"%s\"\n",level,buffer);*/
		switch (level)
		{
			case 1: succeed_if (strcmp (buffer, "user") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 4, "wrong size returned"); break;
			case 2: succeed_if (strcmp (buffer, "abc") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 3, "wrong size returned"); break;
			case 3: succeed_if (strcmp (buffer, "def\\/ghi") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 8, "wrong size returned"); break;
			case 4: succeed_if (strcmp (buffer, "jkl") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 3, "wrong size returned"); break;
			default: succeed_if (0, "should not reach case statement");
		}
	}

	/* with escaped sequence at the end:*/
	name="user////abc/def\\/ghi////jkl\\/\\/";
	size=0;
	level=0;

	p=name;
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		level++;

		strncpy(buffer,p,size);
		buffer[size]=0;

		/* printf("Level %d name: \"%s\"\n",level,buffer);*/
		switch (level)
		{
			case 1: succeed_if (strcmp (buffer, "user") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 4, "wrong size returned"); break;
			case 2: succeed_if (strcmp (buffer, "abc") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 3, "wrong size returned"); break;
			case 3: succeed_if (strcmp (buffer, "def\\/ghi") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 8, "wrong size returned"); break;
			case 4: succeed_if (strcmp (buffer, "jkl\\/\\/") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 7, "wrong size returned"); break;
			default: succeed_if (0, "should not reach case statement");
		}
	}

	/* with escaped sequence at the begin:*/
	name="user////\\/abc/\\/def\\/ghi////jkl\\/\\/";
	size=0;
	level=0;

	p=name;
	while (*(p=keyNameGetOneLevel(p+size,&size))) {
		level++;

		strncpy(buffer,p,size);
		buffer[size]=0;

		/* printf("Level %d name: \"%s\"\n",level,buffer);*/
		switch (level)
		{
			case 1: succeed_if (strcmp (buffer, "user") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 4, "wrong size returned"); break;
			case 2: succeed_if (strcmp (buffer, "\\/abc") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 5, "wrong size returned"); break;
			case 3: succeed_if (strcmp (buffer, "\\/def\\/ghi") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 10, "wrong size returned"); break;
			case 4: succeed_if (strcmp (buffer, "jkl\\/\\/") == 0, "keyNameGetOneLevel not correct");
				succeed_if (size == 7, "wrong size returned"); break;
			default: succeed_if (0, "should not reach case statement");
		}
	}
	

	parentSize=keyGetParentNameSize(key);
	parentName=malloc(parentSize);
	keyGetParentName(key,parentName,parentSize);
	succeed_if (strcmp (parentName, "system/parent") == 0, "parentName error");
	free (parentName);
	keyDel (key);

	succeed_if (keyAddBaseName (0, "s") == -1, "null pointer saftey");

	k1 = keyNew ("user/dir1/dir2", KEY_END);
	succeed_if (keyAddBaseName (k1, 0) == 15, "Could not add nothing to basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2") == 0, "added basename not correct");
	succeed_if (keyAddBaseName (k1, "") == 15, "Could not add nothing to basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2") == 0, "added basename not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == 21, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 21, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == 27, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 27, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "a") == 29, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	keyDel (k1);

	k1 = keyNew ("user/dir1/dir2", KEY_END);
	succeed_if (keyAddBaseName (k1, "mykey/mykey/a") == 29, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	keyDel (k1);

	k1 = keyNew ("user/////dir1//////dir2", KEY_END);
	succeed_if (keyAddBaseName (k1, "mykey/////mykey////a") == 29, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	keyDel (k1);

	k1 = keyNew ("user/dir1/////dir2////", KEY_END);
	succeed_if (keyAddBaseName (k1, "////mykey////mykey/a") == 29, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	keyDel (k1);

	k1 = keyNew ("user/dir1/dir2", KEY_END);
	succeed_if (keyAddBaseName (k1, "mykey/mykey////a///") == 29, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	keyDel (k1);

	k2 = keyNew (KEY_END);
	succeed_if (keyAddBaseName (k2, "no") == -1, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 1, "Name size not correct");
	keyDel (k2);

	k2 = keyNew (KEY_END);
	succeed_if (keyAddBaseName (k2, "user") == 5, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 5, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/dir2/mykey/mykey/a", KEY_END);
	succeed_if (keySetBaseName (k2, "mykey") == 33, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/dir2/mykey/mykey/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 33, "Name size not correct");
	succeed_if (keySetBaseName (k2, "einva") == 33, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/dir2/mykey/mykey/einva") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 33, "Name size not correct");
	succeed_if (keySetBaseName (k2, "chang") == 33, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/dir2/mykey/mykey/chang") == 0, "added basename not correct");
	succeed_if (keySetBaseName (k2, "change") == 34, "Could not add basename");
	succeed_if (keyGetNameSize(k2) == 34, "Name size not correct");
	succeed_if (strcmp (keyName(k2), "user/dir1/dir2/mykey/mykey/change") == 0, "added basename not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "") == 10, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 10, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "some/more") == 20, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/some/more") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 20, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "some////more") == 20, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/some/more") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 20, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "some////more///") == 20, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/some/more") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 20, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "///some////more") == 20, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/some/more") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 20, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user", KEY_END);
	succeed_if (keySetBaseName (k2, "user") == 5, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 5, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("system", KEY_END);
	succeed_if (keySetBaseName (k2, "user") == 5, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 5, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user", KEY_END);
	succeed_if (keySetBaseName (k2, "system") == 7, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "system") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 7, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("system", KEY_END);
	succeed_if (keySetBaseName (k2, "system") == 7, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "system") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 7, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user", KEY_END);
	succeed_if (keySetBaseName (k2, "no") == -1, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 1, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("system", KEY_END);
	succeed_if (keySetBaseName (k2, "no") == -1, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 1, "Name size not correct");
	keyDel (k2);
}


void test_keyNamespace()
{
	Key *key;

	printf ("Test namespaces\n");

	key = keyNew (KEY_END);
	succeed_if (keyNameGetNamespace (keyName(key)) == 0, "empty namespace not 0");
	succeed_if (keyGetNamespace (key) == 0, "empty namespace not 0");
	succeed_if (keyNameIsSystem (keyName(key)) == 0, "empty name is not system");
	succeed_if (keyIsSystem (key) == 0, "empty key is not system");
	succeed_if (keyNameIsUser (keyName(key)) == 0, "empty name is not user");
	succeed_if (keyIsUser (key) == 0, "empty key is not user");
	keyDel (key);

	key = keyNew ("user", KEY_END);
	succeed_if (keyNameGetNamespace (keyName(key)) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyNameIsSystem (keyName(key)) == 0, "user name is not system");
	succeed_if (keyIsSystem (key) == 0, "user key is not system");
	succeed_if (keyNameIsUser (keyName(key)) == 1, "user name is not user");
	succeed_if (keyIsUser (key) == 1, "user key is not user");
	keyDel (key);

	key = keyNew ("user/key", KEY_END);
	succeed_if (keyNameGetNamespace (keyName(key)) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyNameIsSystem (keyName(key)) == 0, "user name is not system");
	succeed_if (keyIsSystem (key) == 0, "user key is not system");
	succeed_if (keyNameIsUser (keyName(key)) == 1, "user name is not user");
	succeed_if (keyIsUser (key) == 1, "user key is not user");
	keyDel (key);

	key = keyNew ("user:owner/key", KEY_END);
	succeed_if (keyNameGetNamespace (keyName(key)) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyGetNamespace (key) == KEY_NS_USER, "user namespace not KEY_NS_USER");
	succeed_if (keyNameIsSystem (keyName(key)) == 0, "user name is not system");
	succeed_if (keyIsSystem (key) == 0, "user key is not system");
	succeed_if (keyNameIsUser (keyName(key)) == 1, "user name is not user");
	succeed_if (keyIsUser (key) == 1, "user key is not user");
	keyDel (key);

	key = keyNew ("system", KEY_END);
	succeed_if (keyNameGetNamespace (keyName(key)) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	succeed_if (keyGetNamespace (key) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	succeed_if (keyNameIsSystem (keyName(key)) == 1, "system name is not system");
	succeed_if (keyIsSystem (key) == 1, "system key is not system");
	succeed_if (keyNameIsUser (keyName(key)) == 0, "system name is not system");
	succeed_if (keyIsUser (key) == 0, "system key is not system");
	keyDel (key);

	key = keyNew ("system/key", KEY_END);
	succeed_if (keyNameGetNamespace (keyName(key)) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	succeed_if (keyGetNamespace (key) == KEY_NS_SYSTEM, "system namespace not KEY_NS_SYSTEM");
	succeed_if (keyNameIsSystem (keyName(key)) == 1, "system name is not system");
	succeed_if (keyIsSystem (key) == 1, "system key is not system");
	succeed_if (keyNameIsUser (keyName(key)) == 0, "system name is not system");
	succeed_if (keyIsUser (key) == 0, "system key is not system");
	keyDel (key);
}

typedef void (*fun_t) ();
void fun()
{}

void test_binary()
{
	printf ("Test binary values\n");

	Key *k = 0;

	int i = 20;
	int *p = &i;

	k = keyNew(0);
	succeed_if (keySetBinary(k, &p, sizeof(p)) == sizeof(p),
			"could not set binary");

	int *q;
	succeed_if (keyGetBinary(k, &q, sizeof(q)) == sizeof(q),
			"could not get binary");
	succeed_if (p == q, "pointers to int are not equal");
	succeed_if (*p == *q, "values are not equal");
	succeed_if (*q == 20, "values are not equal");
	succeed_if (*p == 20, "values are not equal");

	keyDel (k);



	union {void (*f)(); void* v;} conversation;

	k = keyNew(0);
	conversation.f = fun;
	succeed_if (keySetBinary(k, &conversation.v, sizeof(conversation)) == sizeof(conversation),
			"could not set binary");

	conversation.v = 0;
	conversation.f = 0;
	void (*g) () = 0;
	succeed_if (keyGetBinary(k, &conversation.v, sizeof(conversation)) == sizeof(conversation),
			"could not get binary");
	g = conversation.f;
	succeed_if (g == fun, "pointers to functions are not equal");

	keyDel (k);



	conversation.f = fun;
	k = keyNew("system/symbols/fun",
			KEY_BINARY,
			KEY_SIZE, sizeof (conversation),
			KEY_VALUE, &conversation.v,
			KEY_END);

	conversation.v = 0;
	conversation.f = 0;
	succeed_if (keyGetBinary(k, &conversation.v, sizeof(conversation)) == sizeof(conversation),
			"could not get binary");
	g = conversation.f;
	succeed_if (g == fun, "pointers to functions are not equal");

	keyDel (k);




	Plugin *plug = (Plugin *) 1222243;

	k = 
		keyNew("system/name",
			KEY_BINARY,
			KEY_SIZE, sizeof (plug),
			KEY_VALUE, &plug,
			KEY_END);
	Plugin *xlug = *(Plugin**)keyValue(k);

	succeed_if (xlug == plug, "should point to the same");
	succeed_if (plug == (Plugin *) 1222243, "should point to that");
	succeed_if (xlug == (Plugin *) 1222243, "should point to that too");

	keyDel (k);


	fun_t tmp = fun;

	k = keyNew ("system/symbol/fun",
			KEY_BINARY,
			KEY_SIZE, sizeof (fun_t),
			KEY_VALUE, &tmp,
			KEY_END);

	fun_t myfun = 0;
	succeed_if (keyGetBinary(k, &myfun, sizeof(fun_t)) == sizeof(fun_t),
			"could not get binary");

	succeed_if (fun == myfun, "pointers not equal");

	keyDel (k);


	k = keyNew ("system/symbol/cool",
			KEY_FUNC, fun,
			KEY_END);

	succeed_if (keyGetBinary(k, &myfun, sizeof(fun_t)) == sizeof(fun_t),
			"could not get binary");

	succeed_if (fun == myfun, "pointers not equal");

	keyDel (k);

	char data[10];
	k = keyNew ("system/empty_binary", KEY_END);
	succeed_if (keySetBinary(k, 0, 0) == 0, "could not set binary will null pointer");
	succeed_if (keyIsBinary(k), "key is not binary");
	succeed_if (keyGetBinary(k, data, 1) == 0, "could not get empty binary");
	succeed_if (keyValue(k) == 0, "did not get back null pointer");

	succeed_if (keySetBinary(k, 0, 1) == 0, "could not set binary will null pointer");
	succeed_if (keyIsBinary(k), "key is not binary");
	succeed_if (keyGetBinary(k, data, 1) == 0, "could not get empty binary");
	succeed_if (keyValue(k) == 0, "did not get back null pointer");

	succeed_if (keySetBinary(k, 0, 5) == 0, "could not set binary will null pointer");
	succeed_if (keyIsBinary(k), "key is not binary");
	succeed_if (keyGetBinary(k, data, 1) == 0, "could not get empty binary");
	succeed_if (keyValue(k) == 0, "did not get back null pointer");

	succeed_if (keySetBinary(k, 0, -1) == -1, "misusage: this will fail");
	succeed_if (keyIsBinary(k), "key is not binary (should be from previous calls)");
	succeed_if (keyGetBinary(k, data, 1) == 0, "could not get empty binary");
	succeed_if (keyValue(k) == 0, "did not get back null pointer");
	keyDel (k);
}

void test_keyBelowOrSame()
{
	Key * key1 = keyNew(0);
	Key * key2 = keyNew(0);

	printf("Test of keyBelowOrSame\n");

	succeed_if (keyIsBelowOrSame(key1,0) == -1, "NULL pointer");
	succeed_if (keyIsBelowOrSame(0,0) == -1, "NULL pointer");
	succeed_if (keyIsBelowOrSame(0,key1) == -1, "NULL pointer");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid");
	succeed_if (keyIsBelowOrSame(key1,key2), "Key should be below");
	succeed_if (keyIsBelowOrSame(key2,key1), "Key should be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/");
	succeed_if (keyIsBelowOrSame(key1,key2), "Key should be below");
	succeed_if (keyIsBelowOrSame(key2,key1), "Key should be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/below");
	succeed_if (keyIsBelowOrSame(key1,key2), "Key should be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/b");
	succeed_if (keyIsBelowOrSame(key1,key2), "Key should be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/b");
	succeed_if (keyIsBelowOrSame(key1,key2), "Key should be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid");
	keySetName(key2,"user/valid/b/e");
	succeed_if (keyIsBelowOrSame(key1,key2), "Key should be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/valide");
	keySetName(key2,"user/valid/e");
	succeed_if (!keyIsBelowOrSame(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/valid/b");
	keySetName(key2,"user/valid/e");
	succeed_if (!keyIsBelowOrSame(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/export/a");
	keySetName(key2,"user/export-backup/b");
	succeed_if (!keyIsBelowOrSame(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keySetName(key1,"user/export");
	keySetName(key2,"user/export-backup-2/x");
	succeed_if (!keyIsBelowOrSame(key1,key2), "Key should not be below");
	succeed_if (!keyIsBelowOrSame(key2,key1), "Key should not be below");

	keyDel (key1);
	keyDel (key2);
}

void test_keyNameSpecial()
{
	printf ("Test special keynames");
	Key *k = keyNew (KEY_END);
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");

	succeed_if (keySetName (k, "system"), "could not set key name with system");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, 0) == 0, "could not set key name with 0");
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");

	succeed_if (keySetName (k, "system"), "could not set key name with system");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "") == 0, "could not set key name with empty string");
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");

	succeed_if (keySetName (k, "system"), "could not set key name with system");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "invalid") == -1, "could not set key name invalid");
	succeed_if (!strcmp (keyName(k), ""), "name should be empty after initialization");



	succeed_if (keySetName (k, "system/something/.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/something/.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/something/../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/something/../../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/something/../../../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/something/../../../../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");



	succeed_if (keySetName (k, "system/../something"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/something"), "name wrong");

	succeed_if (keySetName (k, "system/../../something"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/something"), "name wrong");

	succeed_if (keySetName (k, "system/../../../something"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/something"), "name wrong");

	succeed_if (keySetName (k, "system/../../../../something"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/something"), "name wrong");

	succeed_if (keySetName (k, "system/../../../../../something"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/something"), "name wrong");



	succeed_if (keySetName (k, "system/a/b/c/.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a/b"), "name wrong");

	succeed_if (keySetName (k, "system/a/b/c/../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a"), "name wrong");

	succeed_if (keySetName (k, "system/a/b/c/../../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/a/b/c/../../../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");

	succeed_if (keySetName (k, "system/a/b/c/../../../../.."), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system"), "name wrong");



	succeed_if (keySetName (k, "system/../a/b/c"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a/b/c"), "name wrong");

	succeed_if (keySetName (k, "system/../../a/b/c"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a/b/c"), "name wrong");

	succeed_if (keySetName (k, "system/../../../a/b/c"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a/b/c"), "name wrong");

	succeed_if (keySetName (k, "system/../../../../a/b/c"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a/b/c"), "name wrong");

	succeed_if (keySetName (k, "system/../../../../../a/b/c"), "could not set key name with ..");
	succeed_if (!strcmp (keyName(k), "system/a/b/c"), "name wrong");


	keyDel (k);
}

void test_keyClear()
{
	printf ("Test clear of key\n");

	Key *k1 = keyNew("system/abc", KEY_END);
	succeed_if (!strcmp (keyName(k1), "system/abc"), "name wrong");

	succeed_if (keyGetRef(k1) == 0, "New key reference");
	keyIncRef(k1);
	succeed_if (keyGetRef(k1) == 1, "Incremented key reference");
	Key *k2 = k1; // create an alias for k1
	succeed_if (!strcmp (keyName(k2), "system/abc"), "name wrong");
	succeed_if (keyGetRef(k1) == 1, "Incremented key reference");
	succeed_if (keyGetRef(k2) == 1, "Incremented key reference");

	keyIncRef(k1);
	Key *k3 = k1; // create an alias for k1
	succeed_if (!strcmp (keyName(k3), "system/abc"), "name wrong");
	succeed_if (keyGetRef(k1) == 2, "Incremented key reference");
	succeed_if (keyGetRef(k2) == 2, "Incremented key reference");
	succeed_if (keyGetRef(k3) == 2, "Incremented key reference");

	keyClear(k1);
	succeed_if (!strcmp (keyName(k1), ""), "name wrong after clear");
	succeed_if (!strcmp (keyName(k2), ""), "name wrong after clear");
	succeed_if (!strcmp (keyName(k3), ""), "name wrong after clear");

	keySetMeta(k1, "test_meta", "test_value");
	succeed_if (!strcmp (keyValue(keyGetMeta(k1, "test_meta")), "test_value"), "meta wrong");
	succeed_if (!strcmp (keyValue(keyGetMeta(k2, "test_meta")), "test_value"), "meta wrong");
	succeed_if (!strcmp (keyValue(keyGetMeta(k3, "test_meta")), "test_value"), "meta wrong");

	keyClear(k2);
	succeed_if (keyGetMeta(k1, "test_meta") == 0, "there should be no meta after keyClear");
	succeed_if (keyGetMeta(k2, "test_meta") == 0, "there should be no meta after keyClear");
	succeed_if (keyGetMeta(k3, "test_meta") == 0, "there should be no meta after keyClear");

	keySetString(k1, "mystring");
	succeed_if (!strcmp (keyValue(k1), "mystring"), "value wrong after clear");
	succeed_if (!strcmp (keyValue(k2), "mystring"), "value wrong after clear");
	succeed_if (!strcmp (keyValue(k3), "mystring"), "value wrong after clear");

	keyClear(k3);
	succeed_if (!strcmp (keyValue(k1), ""), "value wrong");
	succeed_if (!strcmp (keyValue(k2), ""), "value wrong");
	succeed_if (!strcmp (keyValue(k3), ""), "value wrong");

	succeed_if (keyGetRef(k1) == 2, "Incremented key reference");
	succeed_if (keyGetRef(k2) == 2, "Incremented key reference");
	succeed_if (keyGetRef(k3) == 2, "Incremented key reference");

	keyDel(k3); // does nothing
	keyDecRef(k3);
	k3 = 0; // remove alias
	succeed_if (keyGetRef(k1) == 1, "Incremented key reference");
	succeed_if (keyGetRef(k2) == 1, "Incremented key reference");

	keyDel(k2); // does nothing
	keyDecRef(k2);
	k2 = 0; // remove alias
	succeed_if (keyGetRef(k1) == 0, "Incremented key reference");

	keyDel(k1);
}

void test_keyBaseName()
{
	// TODO: Bug, does not work at the moment!
	printf ("Test add and set basename");

	Key *k = keyNew (KEY_END);

	keySetName (k, "system/valid");
	succeed_if (keySetBaseName (k, "..") == -1, "outbreak should not be allowed");
	output_key(k);

	keySetName (k, "system/valid");
	keyAddBaseName (k, "..");
	succeed_if (keyAddBaseName (k, "..") == -1, "outbreak should not be allowed");
	output_key(k);

	keySetName (k, "system/valid");
	succeed_if (!strcmp(keyBaseName(k), "valid"), "invalid base name");

	keySetName (k, "system");
	succeed_if (!strcmp(keyBaseName(k), "system"), "invalid base name for system");

	keySetName (k, "user");
	succeed_if (!strcmp(keyBaseName(k), "user"), "invalid base name for user");

	keyDel (k);
}


int main(int argc, char** argv)
{
	printf("    KEY  TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_keyComparing();
	test_keyNewSpecial();
	test_keyNewSystem();
	test_keyNewUser();
	test_keyReference();
	test_keyName();
	test_keyValue();
	test_keyBinary();
	test_keyComment();
	test_keyOwner();
	test_keyInactive();
	test_keyBelow();
	test_keyDup();
	test_keyCopy();
	test_keyDir();
	test_keyTime();
	test_keyMeta();
	test_keyHelpers();
	test_keyNamespace();
	test_binary();
	test_keyBelowOrSame();
	test_keyNameSpecial();
	test_keyClear();

	// test_keyBaseName(); // TODO: Bug, does not work at the moment

	printf("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
