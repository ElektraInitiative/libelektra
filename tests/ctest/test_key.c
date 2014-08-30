#include <tests_internal.h>

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

	// TODO: the disabled tests wont work anymore with the new behaviour of keyAddBasename

	k1 = keyNew ("user/dir1/dir2", KEY_END);
	succeed_if (keyAddBaseName (k1, 0) == 15, "Could not add nothing to basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2") == 0, "added basename not correct");
	succeed_if (keyAddBaseName (k1, "") == 17, "Could not add nothing to basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%") == 0, "added basename not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == 23, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 23, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == 29, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%/mykey/mykey") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 29, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "a") == 31, "Could not add basename");
	succeed_if (strcmp (keyName(k1), "user/dir1/dir2/%/mykey/mykey/a") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k1) == 31, "Name size not correct");
	keyDel (k1);

	/*
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

	*/

	k2 = keyNew (KEY_END);
	succeed_if (keyAddBaseName (k2, "no") == -1, "Could add basename on empty name");
	succeed_if (strcmp (keyName(k2), "") == 0, "added basename not correct");
	succeed_if (keyGetNameSize(k2) == 1, "Name size not correct");
	keyDel (k2);

	k2 = keyNew (KEY_END);
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
	succeed_if (keyGetNameSize(k2) == 5, "Name size not correct");
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

int main(int argc, char** argv)
{
	printf("KEY      TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_keyRefcounter();
	test_keyHelpers();
	test_keyPlugin();

	printf("\ntest_key RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
