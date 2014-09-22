#include <tests_internal.h>

#ifdef HAVE_TIME_H
#include <time.h>
#endif

static void test_keyRefcounter()
{
	Key *key = keyNew(0);
	key->ksReference = 5;
	succeed_if (key->ksReference == 5, "wrong ref");
	succeed_if (keyGetRef(key) == 5, "wrong ref");
	while (keyGetRef(key) > 0) keyDecRef(key);
	succeed_if (key->ksReference == 0, "wrong ref after dec");
	succeed_if (keyGetRef(key) == 0, "reference counter");
	succeed_if (keyDecRef(key) == 0, "should stay at minimum");
	succeed_if (keyGetRef(key) == 0, "reference counter");
	succeed_if (keyDecRef(key) == 0, "should stay at minimum");
	keyDel (key);
}

static void test_keyHelpers()
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
	succeed_if (keyAddBaseName (k1, "") == 17, "Could not add nothing to basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%") == 0, "added basename not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == 23, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 23, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == sizeof("user/dir1/dir2/%/mykey/mykey"), "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%/mykey/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "a") == 31, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 31, "Name size not correct");
	keyDel (k1);

{
	k2 = keyNew ("user/dir1/dir2", KEY_END);
	char c[] = "user/dir1/dir2/mykey\\/mykey\\/a";
	succeed_if (keyAddBaseName (k2, "mykey/mykey/a") == sizeof(c), "Could not add basename");
	succeed_if_same_string (keyName(k2), c);
	succeed_if (keyGetNameSize(k2) == sizeof(c), "Name size not correct");
	keyDel (k2);
}

{
	k2 = keyNew ("user/dir1/dir2", KEY_END);
	char c[] = "user/dir1/dir2/mykey\\/\\/\\/\\/a";
	succeed_if (keyAddBaseName (k2, "mykey////a") == sizeof(c), "Could not add basename");
	succeed_if_same_string (keyName(k2), c);
	succeed_if (keyGetNameSize(k2) == sizeof(c), "Name size not correct");
	keyDel (k2);
}

{
	k2 = keyNew ("user/dir1/dir2", KEY_END);
	char c[] = "user/dir1/dir2/mykey\\/\\/\\/\\/";
	succeed_if (keyAddBaseName (k2, "mykey////") == sizeof(c), "Could not add basename");
	succeed_if_same_string (keyName(k2), c);
	succeed_if (keyGetNameSize(k2) == sizeof(c), "Name size not correct");
	keyDel (k2);
}


	k2 = keyNew (0);
	succeed_if (keyAddBaseName (k2, "no") == -1, "Could add basename on empty name");
	succeed_if (strcmp (keyName(k2), "") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 1, "Name size not correct");
	keyDel (k2);

	k2 = keyNew (0);
	succeed_if (keyAddBaseName (k2, "user") == -1, "Could add basename on empty name");
	succeed_if (strcmp (keyName(k2), "") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 1, "Name size not correct");
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
	succeed_if (keySetBaseName (k2, 0) == 10, "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 10, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "some/more") == sizeof("user/dir1/some\\/more"), "Could not add basename");
	succeed_if (strcmp (keyName(k2), "user/dir1/some\\/more") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == sizeof("user/dir1/some\\/more"), "Name size not correct");
	keyDel (k2);

{
	k2 = keyNew ("user/dir1/a", KEY_END);
	char c[] = "user/dir1/some\\/\\/\\/\\/more";
	succeed_if (keySetBaseName (k2, "some////more") == sizeof(c), "Could not add basename");
	succeed_if_same_string (keyName(k2), c);
	succeed_if (keyGetNameSize(k2) == sizeof(c), "Name size not correct");
	keyDel (k2);
}

{
	k2 = keyNew ("user/dir1/a", KEY_END);
	char c[] = "user/dir1/\\/\\/\\/\\/more";
	succeed_if (keySetBaseName (k2, "////more") == sizeof(c), "Could not add basename");
	succeed_if_same_string (keyName(k2), c);
	succeed_if (keyGetNameSize(k2) == sizeof(c), "Name size not correct");
	keyDel (k2);
}

	k2 = keyNew ("user", KEY_END);
	succeed_if (keySetBaseName (k2, "user") == -1, "Could add basename, but there is none");
	succeed_if (strcmp (keyName(k2), "user") == 0, "basename not correct");
	succeed_if (keyGetNameSize(k2) == 5, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("system", KEY_END);
	succeed_if (keySetBaseName (k2, "system") == -1, "Could add basename, but there is none");
	succeed_if (strcmp (keyName(k2), "system") == 0, "basename not correct");
	succeed_if (keyGetNameSize(k2) == 7, "Name size not correct");
	keyDel (k2);
}

static void test_keyPlugin()
{
	Plugin *plug = (Plugin *) 1222243;

	Key *k = 
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
}

static void test_keyUnescaped()
{
	printf ("test unescaped\n");

	Key *k = keyNew("user/something", KEY_END);
	succeed_if (!memcmp(keyUnescapedName(k), "user\0something", sizeof("user/something")), "unescaped name wrong");

	keySetName(k, "system/something/else");
	succeed_if (!memcmp(keyUnescapedName(k), "system\0something\0else", sizeof("system/something/else")), "unescaped name wrong");

	keyAddBaseName(k, "more");
	succeed_if (!memcmp(keyUnescapedName(k), "system\0something\0else\0more", sizeof("system/something/else/more")), "unescaped name wrong");

	keySetBaseName(k, "else");
	succeed_if (!memcmp(keyUnescapedName(k), "system\0something\0else\0else", sizeof("system/something/else/else")), "unescaped name wrong");

	keySetBaseName(k, "");
	succeed_if (!memcmp(keyUnescapedName(k), "system\0something\0else\0\0", sizeof("system/something/else/")), "unescaped name wrong");

	keySetBaseName(k, "%");
	succeed_if (!memcmp(keyUnescapedName(k), "system\0something\0else\0%", sizeof("system/something/else/%")), "unescaped name wrong");

	/* print memory of keyUnescapedName
	for (size_t i = 0; i<sizeof("system/something/else/\\%"); ++i)
	{
		printf ("%c %d\n", (char)((char*)keyUnescapedName(k))[i], (int)((char*)keyUnescapedName(k))[i]);
	}
	*/

	keyDel(k);
}

static void test_keyCompare()
{
	printf ("test keyCompare\n");
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

static void test_keyNewExtensions()
{
	printf ("test keyNewExtensions\n");

	Key *key = 0;
	succeed_if (keyIsUser(key) == -1, "empty user key");
	succeed_if (keyIsSystem(key) == -1, "empty user key?");

	key = keyNew("", KEY_END);
	succeed_if (keyIsUser(key) == 0, "empty user key");
	succeed_if (keyIsSystem(key) == 0, "empty user key?");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + mode");

	// Key with name + UID/GID
	key = keyNew("system/sw/test",
			KEY_UID, 123,
			KEY_GID, 456,
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + UID + GID");
	succeed_if(keyGetUID(key) == 123, "keyNew: UID no set correctly");
	succeed_if(keyGetGID(key) == 456, "keyNew: GID not set correctly");
	succeed_if(keyIsUser(key) == 0, "not user");
	succeed_if(keyIsSystem(key) == 1, "is system");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + UID + GID");

	// Key with name + MODE
	key = keyNew("system/sw/test",
			KEY_MODE, 0644,
			KEY_END);
	succeed_if(key != NULL, "keyNew: Unable to create a key with name + mode");
	succeed_if(keyGetMode(key) == 0644, "keyNew: mode no set correctly");
	succeed_if(keyDel(key) == 0, "keyDel: Unable to delete key with name + mode");
}

static void test_owner()
{
	printf ("test owner\n");

	Key *key = 0;
	char fullroot [KDB_MAX_PATH_LENGTH];
	char array[] = "here is some data stored";
	char testOwner[] = "max";

	char * getBack;
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
	succeed_if (keyIsUser(key) == 1, "is user");
	succeed_if (keyIsSystem(key) == 0, "not system");
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

	char	ret [1000];
	size_t i = 0;
	key = keyNew ("user:max/name", KEY_END);
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
}

static void test_keyComment()
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

static void test_keyOwner()
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

static void test_keyDir (void)
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

static void test_keyTime()
{
	Key * key = keyNew (0);
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

static void test_keyMeta(void)
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

static void test_keyNamespace()
{
	Key *key;

	printf ("Test namespaces\n");

	key = keyNew (0);
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

static void test_elektraKeySetName()
{
	printf ("test elektraKeySetName\n");

	Key *key = keyNew("", KEY_END);
	succeed_if(elektraKeySetName(key, "/", KDB_O_CASCADING_NAME) != -1, "could not set cascading name");
	succeed_if_same_string(keyName(key), "/");
	elektraKeySetName(key, "/cascading", KDB_O_CASCADING_NAME);
	succeed_if_same_string(keyName(key), "/cascading");
	elektraKeySetName(key, "/cascading/s/deep/below", KDB_O_CASCADING_NAME);
	succeed_if_same_string(keyName(key), "/cascading/s/deep/below");
	elektraKeySetName(key, "user/cascading/s/deep/below", KDB_O_CASCADING_NAME);
	succeed_if_same_string(keyName(key), "user/cascading/s/deep/below");
	elektraKeySetName(key, "system/cascading/s/deep/below", KDB_O_CASCADING_NAME);
	succeed_if_same_string(keyName(key), "system/cascading/s/deep/below");

	elektraKeySetName(key, "order", KDB_O_META_NAME);
	succeed_if_same_string(keyName(key), "order");
	elektraKeySetName(key, "check/type", KDB_O_META_NAME);
	succeed_if_same_string(keyName(key), "check/type");

	elektraKeySetName(key, "", KDB_O_EMPTY_NAME);
	succeed_if_same_string(keyName(key), "");
	succeed_if(key->key != 0, "null pointer?");
	succeed_if_same_string(keyName(key), "");

	elektraKeySetName(key, "", KDB_O_META_NAME | KDB_O_EMPTY_NAME);
	succeed_if_same_string(keyName(key), "");
	succeed_if(key->key != 0, "null pointer?");
	succeed_if_same_string(keyName(key), "");

	keySetName(key, 0);
	succeed_if_same_string(keyName(key), "");
	succeed_if(key->key != 0, "null pointer?");
	succeed_if_same_string(keyName(key), "");

	elektraKeySetName(key, "", KDB_O_META_NAME | KDB_O_CASCADING_NAME);
	succeed_if_same_string(keyName(key), "");
	succeed_if(key->key != 0, "null pointer?");
	succeed_if_same_string(keyName(key), "");

	keyDel(key);
}

static void test_keyLock()
{
	printf ("Test locking\n");

	Key *key = keyNew("", KEY_LOCK_NAME, KEY_END);
	Key *key2 = keyNew("", KEY_LOCK_NAME, KEY_END);

	succeed_if (keySetName(key, "user") == -1, "read only name, not allowed to set");

	keyDel (key);
	key = keyNew("", KEY_LOCK_VALUE, KEY_END);

	succeed_if (keySetString(key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary(key, "a", 2) == -1, "read only string, not allowed to set");

	keyDel (key);
	key = keyNew("", KEY_LOCK_META, KEY_END);

	succeed_if (keySetMeta(key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyMeta(key, key2,  "meta") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyAllMeta(key, key2) == -1, "read only meta, not allowed to set");

	keyDel (key);
	key = keyNew(0);

	keyLock(key, KEY_LOCK_NAME);
	succeed_if (keySetName(key, "user") == -1, "read only name, not allowed to set");
	succeed_if (keyAddName(key, "a") == -1, "read only name, not allowed to set");
	succeed_if (keySetBaseName(key, "a") == -1, "read only name, not allowed to set");
	succeed_if (keyAddBaseName(key, "a") == -1, "read only name, not allowed to set");

	keyDel (key);
	key = keyNew(0);

	keyLock(key, KEY_LOCK_VALUE);

	succeed_if (keySetString(key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary(key, "a", 2) == -1, "read only string, not allowed to set");

	keyDel (key);
	key = keyNew(0);

	keyLock(key, KEY_LOCK_META);

	succeed_if (keySetMeta(key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyMeta(key, key2,  "meta") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyAllMeta(key, key2) == -1, "read only meta, not allowed to set");

	keyDel (key);
	keyDel (key2);
}

static void test_keyAddName()
{
	Key *k = keyNew("user", KEY_END);
	keyAddName(k, "something");
	succeed_if_same_string(keyName(k), "user/something");

	keyAddName(k, "with/slash");
	succeed_if_same_string(keyName(k), "user/something/with/slash");
	keyDel(k);

	k = keyNew("system/elektra/mountpoints/_t_error/config", KEY_END);
	keyAddName(k, "on_open/error");
	succeed_if_same_string(keyName(k), "system/elektra/mountpoints/_t_error/config/on_open/error");
	keyDel(k);

	k = keyNew("user", KEY_END);
	succeed_if (keyAddName(k, "///sw/../sw//././MyApp")==sizeof("user/sw/MyApp"), "could not add name");
	succeed_if_same_string(keyName(k), "user/sw/MyApp");
	keyDel(k);

	k = keyNew("user", KEY_END);
	succeed_if (keyAddName(k, "bar\\/foo_bar\\/")==sizeof("user/bar\\/foo_bar\\/"), "could not add name");
	succeed_if_same_string(keyName(k), "user/bar\\/foo_bar\\/");
	keyDel(k);

	k = keyNew("user", KEY_END);
	succeed_if (keyAddName(k, "ba\\\\/foo_bar\\/")==sizeof("user/ba\\\\/foo_bar\\/"), "could not add name");
	succeed_if_same_string(keyName(k), "user/ba\\\\/foo_bar\\/");
	keyDel(k);

	k = keyNew("user", KEY_END);
	succeed_if (keyAddName(k, "ba\\\\/foo_bar\\//%")==sizeof("user/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string(keyName(k), "user/ba\\\\/foo_bar\\//%");
	keyDel(k);

	k = keyNew("user:yl", KEY_END);
	succeed_if (keyAddName(k, "ba\\\\/foo_bar\\//%")==sizeof("user/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string(keyName(k), "user/ba\\\\/foo_bar\\//%");
	keyDel(k);

	k = keyNew("user:yl", KEY_END);
	succeed_if (keyAddName(k, "ba\\\\foo_bar\\//%")==sizeof("user/ba\\\\foo_bar\\//%"), "could not add name");
	succeed_if_same_string(keyName(k), "user/ba\\\\foo_bar\\//%");
	keyDel(k);

	k = keyNew("system", KEY_END);
	succeed_if (keyAddName(k, "ba\\\\/foo_bar\\//%")==sizeof("system/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string(keyName(k), "system/ba\\\\/foo_bar\\//%");
	keyDel(k);
}

int main(int argc, char** argv)
{
	printf("KEY      TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_keyRefcounter();
	test_keyHelpers();
	test_keyPlugin();
	test_keyUnescaped();
	test_keyCompare();
	test_keyNewExtensions();
	test_keyComment();
	test_keyOwner();
	test_keyComment();
	test_keyOwner();
	test_keyDir();
	test_keyTime();
	test_keyMeta();
	test_keyNamespace();
	test_owner();
	test_elektraKeySetName();
	test_keyLock();
	test_keyAddName();

	printf("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
