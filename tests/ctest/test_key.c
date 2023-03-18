/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/kdbprivate.h>
#include <tests_internal.h>

#ifdef HAVE_TIME_H
#include <time.h>
#endif

static void test_keyRefcounter (void)
{
	Key * key = keyNew ("/", KEY_END);
	key->refs = 5;
	succeed_if (key->refs == 5, "wrong ref");
	succeed_if (keyGetRef (key) == 5, "wrong ref");
	while (keyGetRef (key) > 0)
		keyDecRef (key);
	succeed_if (key->refs == 0, "wrong ref after dec");
	succeed_if (keyGetRef (key) == 0, "reference counter");
	succeed_if (keyDecRef (key) == 0, "should stay at minimum");
	succeed_if (keyGetRef (key) == 0, "reference counter");
	succeed_if (keyDecRef (key) == 0, "should stay at minimum");
	keyDel (key);
}

static void test_keyHelpers (void)
{
	Key *k1, *k2;

	succeed_if (keyAddBaseName (0, "s") == -1, "null pointer saftey");

	k1 = keyNew ("user:/dir1/dir2", KEY_END);
	succeed_if (keyAddBaseName (k1, 0) == -1, "Could add null basename");
	succeed_if_same_string (keyName (k1), "user:/dir1/dir2");
	succeed_if (keyAddBaseName (k1, "") == 18, "Could not add nothing to basename");
	succeed_if_same_string (keyName (k1), "user:/dir1/dir2/%");
	succeed_if (keyAddBaseName (k1, "mykey") == 24, "Could not add basename");
	succeed_if_same_string (keyName (k1), "user:/dir1/dir2/%/mykey");
	succeed_if (keyGetNameSize (k1) == 24, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "mykey") == sizeof ("user:/dir1/dir2/%/mykey/mykey"), "Could not add basename");
	succeed_if_same_string (keyName (k1), "user:/dir1/dir2/%/mykey/mykey");
	succeed_if (keyGetNameSize (k1) == 30, "Name size not correct");
	succeed_if (keyAddBaseName (k1, "a") == 32, "Could not add basename");
	succeed_if_same_string (keyName (k1), "user:/dir1/dir2/%/mykey/mykey/a");
	succeed_if (keyGetNameSize (k1) == 32, "Name size not correct");
	keyDel (k1);

	{
		k2 = keyNew ("user:/dir1/dir2", KEY_END);
		char c[] = "user:/dir1/dir2/mykey\\/mykey\\/a";
		succeed_if (keyAddBaseName (k2, "mykey/mykey/a") == sizeof (c), "Could not add basename");
		succeed_if_same_string (keyName (k2), c);
		succeed_if (keyGetNameSize (k2) == sizeof (c), "Name size not correct");
		keyDel (k2);
	}

	{
		k2 = keyNew ("user:/dir1/dir2", KEY_END);
		char c[] = "user:/dir1/dir2/mykey\\/\\/\\/\\/a";
		succeed_if (keyAddBaseName (k2, "mykey////a") == sizeof (c), "Could not add basename");
		succeed_if_same_string (keyName (k2), c);
		succeed_if (keyGetNameSize (k2) == sizeof (c), "Name size not correct");
		keyDel (k2);
	}

	{
		k2 = keyNew ("user:/dir1/dir2", KEY_END);
		char c[] = "user:/dir1/dir2/mykey\\/\\/\\/\\/";
		succeed_if (keyAddBaseName (k2, "mykey////") == sizeof (c), "Could not add basename");
		succeed_if_same_string (keyName (k2), c);
		succeed_if (keyGetNameSize (k2) == sizeof (c), "Name size not correct");
		keyDel (k2);
	}

	k2 = keyNew ("/", KEY_END);
	succeed_if (keyAddBaseName (k2, "user") == 6, "Could not add basename on /");
	succeed_if_same_string (keyName (k2), "/user");
	succeed_if (keyGetNameSize (k2) == 6, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user:/dir1/dir2/mykey/mykey/a", KEY_END);
	succeed_if (keySetBaseName (k2, "mykey") == 34, "Could not add basename");
	succeed_if_same_string (keyName (k2), "user:/dir1/dir2/mykey/mykey/mykey");
	succeed_if (keyGetNameSize (k2) == 34, "Name size not correct");
	succeed_if (keySetBaseName (k2, "einva") == 34, "Could not add basename");
	succeed_if_same_string (keyName (k2), "user:/dir1/dir2/mykey/mykey/einva");
	succeed_if (keyGetNameSize (k2) == 34, "Name size not correct");
	succeed_if (keySetBaseName (k2, "chang") == 34, "Could not add basename");
	succeed_if_same_string (keyName (k2), "user:/dir1/dir2/mykey/mykey/chang");
	succeed_if (keySetBaseName (k2, "change") == 35, "Could not add basename");
	succeed_if (keyGetNameSize (k2) == 35, "Name size not correct");
	succeed_if_same_string (keyName (k2), "user:/dir1/dir2/mykey/mykey/change");
	keyDel (k2);

	k2 = keyNew ("user:/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, 0) == 11, "Could not add basename");
	succeed_if_same_string (keyName (k2), "user:/dir1");
	succeed_if (keyGetNameSize (k2) == 11, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("user:/dir1/a", KEY_END);
	succeed_if (keySetBaseName (k2, "some/more") == sizeof ("user:/dir1/some\\/more"), "Could not add basename");
	succeed_if_same_string (keyName (k2), "user:/dir1/some\\/more");
	succeed_if (keyGetNameSize (k2) == sizeof ("user:/dir1/some\\/more"), "Name size not correct");
	keyDel (k2);

	{
		k2 = keyNew ("user:/dir1/a", KEY_END);
		char c[] = "user:/dir1/some\\/\\/\\/\\/more";
		succeed_if (keySetBaseName (k2, "some////more") == sizeof (c), "Could not add basename");
		succeed_if_same_string (keyName (k2), c);
		succeed_if (keyGetNameSize (k2) == sizeof (c), "Name size not correct");
		keyDel (k2);
	}

	{
		k2 = keyNew ("user:/dir1/a", KEY_END);
		char c[] = "user:/dir1/\\/\\/\\/\\/more";
		succeed_if (keySetBaseName (k2, "////more") == sizeof (c), "Could not add basename");
		succeed_if_same_string (keyName (k2), c);
		succeed_if (keyGetNameSize (k2) == sizeof (c), "Name size not correct");
		keyDel (k2);
	}

	k2 = keyNew ("user:/", KEY_END);
	succeed_if (keySetBaseName (k2, "user") == -1, "Could add basename, but there is none");
	succeed_if_same_string (keyName (k2), "user:/");
	succeed_if (keyGetNameSize (k2) == 7, "Name size not correct");
	keyDel (k2);

	k2 = keyNew ("system:/", KEY_END);
	succeed_if (keySetBaseName (k2, "system") == -1, "Could add basename, but there is none");
	succeed_if_same_string (keyName (k2), "system:/");
	succeed_if (keyGetNameSize (k2) == 9, "Name size not correct");
	keyDel (k2);
}

static void test_keyPlugin (void)
{
	Plugin * plug = (Plugin *) 1222243;

	Key * k = keyNew ("system:/name", KEY_BINARY, KEY_SIZE, sizeof (plug), KEY_VALUE, &plug, KEY_END);
	Plugin * xlug = *(Plugin **) keyValue (k);

	succeed_if (xlug == plug, "should point to the same");
	succeed_if (plug == (Plugin *) 1222243, "should point to that");
	succeed_if (xlug == (Plugin *) 1222243, "should point to that too");

	keyDel (k);
}

static void test_keyNameUnescape (void)
{
	char buf[1024];
	char * buffer = buf;

	printf ("test elektraKeyNameUnescape\n");
	{
		char a[] = "/\\\\a";
		char s[] = "\0\0\\a";
		s[0] = KEY_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "/a\\/test";
		char s[] = "\0\0a/test";
		s[0] = KEY_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "/a\\\\\\/test";
		char s[] = "\0\0a\\/test";
		s[0] = KEY_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "/a\\\\\\\\\\/test";
		char s[] = "\0\0a\\\\/test";
		s[0] = KEY_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}


	printf ("test elektraKeyNameUnescape (with namespace)\n");
	{
		char a[] = "user:/a/test";
		char s[] = "\0\0a\0test";
		s[0] = KEY_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "user:/a\\/test";
		char s[] = "\0\0a/test";
		s[0] = KEY_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "user:/a\\\\/test";
		char s[] = "\0\0a\\\0test";
		s[0] = KEY_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "user:/\\\\/test";
		char s[] = "\0\0\\\0test";
		s[0] = KEY_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}
}

static void test_keyNewExtensions (void)
{
	printf ("test keyNewExtensions\n");

	Key * key;

	key = keyNew ("/", KEY_END);
	succeed_if (keyIsUser (key) == 0, "empty user key");
	succeed_if (keyIsSystem (key) == 0, "empty user key?");
	succeed_if (keyDel (key) == 0, "keyDel: Unable to delete key with name + mode");
}

static void test_keyComment (void)
{
	Key * key;
	char ret[1000];
	size_t i;
	char testComment[] = "testcomment";

	printf ("Test comment of key\n");

	succeed_if (key = keyNew ("/", KEY_END), "could not create new key");
	succeed_if (keyGetCommentSize (key) == 1, "empty comment size");
	succeed_if (keySetComment (key, "perfectcomment") == 15, "could not set comment");
	succeed_if (keyGetCommentSize (key) == 15, "comment size not correct");
	succeed_if_same_string (keyComment (key), "perfectcomment");
	succeed_if (keySetComment (key, "perfectcomment") == 15, "could not re-set same comment");
	succeed_if_same_string (keyComment (key), "perfectcomment");
	succeed_if (keySetComment (key, "nearperfectcomment") == 19, "could not re-set other comment");
	succeed_if (keyGetCommentSize (key) == 19, "comment size not correct");
	succeed_if_same_string (keyComment (key), "nearperfectcomment");
	succeed_if (keyGetComment (key, ret, keyGetCommentSize (key) >= 999 ? 999 : keyGetCommentSize (key)) == 19,
		    "could not get comment");
	succeed_if_same_string (ret, "nearperfectcomment");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("/", KEY_END), "could not create new key");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keySetComment (key, "") == 1, "could not set comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("/", KEY_END), "could not create new key");
	for (i = 1; i < 256; i++)
	{
		ret[0] = i;
		ret[1] = i;
		ret[2] = 0;
		succeed_if (keySetComment (key, ret) == 3, "could not set comment");
		// output_key (key);
		succeed_if_same_string (keyComment (key), ret);
	}
	succeed_if (keyDel (key) == 0, "could not delete key");


	printf ("Test comment of key 2\n");

	succeed_if (keyComment (0) == 0, "null pointer");
	succeed_if (keyGetCommentSize (0) == -1, "null pointer");
	succeed_if (keySetComment (0, "") == -1, "null pointer");

	key = keyNew ("/", KEY_END);
	succeed_if (keyGetCommentSize (key) == 1, "empty comment size");

	keySetComment (key, testComment);
	succeed_if (keyGetComment (0, ret, 100) == -1, "null pointer");
	succeed_if (keyGetComment (key, 0, 100) == -1, "comment null pointer");
	succeed_if (keyGetComment (key, ret, 0) == -1, "length checking");

	for (i = 1; i < sizeof (testComment); i++)
	{
		succeed_if (keyGetComment (key, ret, i) == -1, "length checking too short");
	}
	for (i = sizeof (testComment); i < sizeof (testComment) * 2; i++)
	{
		succeed_if (keyGetComment (key, ret, i) == sizeof (testComment), "length checking longer");
	}
	succeed_if (keyGetComment (key, ret, (size_t) -1) == -1, "maxSize exceeded");

	succeed_if (keySetComment (key, 0) == 1, "delete comment");
	succeed_if (keyGetComment (key, ret, i) == 1, "length checking deleting");
	succeed_if_same_string (ret, "");

	succeed_if (keySetComment (key, testComment) == sizeof (testComment), "set comment");
	succeed_if (keyGetComment (key, ret, i) == sizeof (testComment), "length checking working");
	succeed_if_same_string (ret, testComment);

	succeed_if (keySetComment (key, "") == 1, "delete comment");
	succeed_if (keyGetComment (key, ret, i) == 1, "length checking deleting");
	succeed_if_same_string (ret, "");

	succeed_if (keySetComment (key, testComment) == sizeof (testComment), "set comment");
	succeed_if (keyGetComment (key, ret, i) == sizeof (testComment), "length checking working");
	succeed_if_same_string (ret, testComment);

	succeed_if (keyGetCommentSize (key) == sizeof (testComment), "testComment comment size");
	succeed_if (strncmp (keyComment (key), testComment, sizeof (testComment)) == 0, "testComment not same");
	keyDel (key);
}

static void test_keySetName (void)
{
	printf ("test keySetName\n");

	Key * key = keyNew ("/", KEY_END);
	Key * dup = 0;

	succeed_if (keySetName (key, "/") != -1, "could not set cascading name");
	succeed_if_same_string (keyName (key), "/");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "/");
	keyDel (dup);

	keySetName (key, "/c");
	succeed_if_same_string (keyName (key), "/c");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "/c");
	keyDel (dup);

	succeed_if (keySetName (key, "/") != -1, "could not set cascading name");
	succeed_if_same_string (keyName (key), "/");
	keySetName (key, "/cascading");
	succeed_if_same_string (keyName (key), "/cascading");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "/cascading");
	keyDel (dup);

	keySetName (key, "/cascading/s/deep/below");
	succeed_if_same_string (keyName (key), "/cascading/s/deep/below");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "/cascading/s/deep/below");
	keyDel (dup);

	keySetName (key, "user:/cascading/s/deep/below");
	succeed_if_same_string (keyName (key), "user:/cascading/s/deep/below");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "user:/cascading/s/deep/below");
	keyDel (dup);

	keySetName (key, "system:/cascading/s/deep/below");
	succeed_if_same_string (keyName (key), "system:/cascading/s/deep/below");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "system:/cascading/s/deep/below");
	keyDel (dup);

	keySetName (key, "meta:/order");
	succeed_if_same_string (keyName (key), "meta:/order");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "meta:/order");
	keyDel (dup);

	keySetName (key, "meta:/check/type");
	succeed_if_same_string (keyName (key), "meta:/check/type");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "meta:/check/type");
	keyDel (dup);

	keySetName (key, "meta:/a");
	succeed_if_same_string (keyName (key), "meta:/a");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "meta:/a");
	keyDel (dup);

	succeed_if (keySetName (key, "") == -1, "setting emtpy name should fail");
	succeed_if_same_string (keyName (key), "meta:/a");

	succeed_if (keySetName (key, 0) == -1, "setting null name should fail");
	succeed_if_same_string (keyName (key), "meta:/a");

	keySetName (key, "/cascading");
	succeed_if_same_string (keyName (key), "/cascading");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "/cascading");
	keyDel (dup);

	keySetName (key, "meta:/");
	succeed_if_same_string (keyName (key), "meta:/");
	succeed_if (key->keyName->key != 0, "null pointer?");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "meta:/");
	keyDel (dup);

	keySetName (key, "meta:/other");
	succeed_if_same_string (keyName (key), "meta:/other");
	succeed_if (key->keyName->key != 0, "null pointer?");
	dup = keyDup (key, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "meta:/other");
	keyDel (dup);

	for (int i = 0; i < 8; ++i)
	{
		keySetName (key, "spec:/test");
		succeed_if_same_string (keyName (key), "spec:/test");
		dup = keyDup (key, KEY_CP_ALL);
		succeed_if_same_string (keyName (dup), "spec:/test");
		keyDel (dup);

		keySetName (key, "proc:/test");
		succeed_if_same_string (keyName (key), "proc:/test");
		dup = keyDup (key, KEY_CP_ALL);
		succeed_if_same_string (keyName (dup), "proc:/test");
		keyDel (dup);

		keySetName (key, "dir:/test");
		succeed_if_same_string (keyName (key), "dir:/test");
		dup = keyDup (key, KEY_CP_ALL);
		succeed_if_same_string (keyName (dup), "dir:/test");
		keyDel (dup);

		keySetName (key, "user:/test");
		succeed_if_same_string (keyName (key), "user:/test");
		dup = keyDup (key, KEY_CP_ALL);
		succeed_if_same_string (keyName (dup), "user:/test");
		keyDel (dup);

		keySetName (key, "system:/test");
		succeed_if_same_string (keyName (key), "system:/test");
		dup = keyDup (key, KEY_CP_ALL);
		succeed_if_same_string (keyName (dup), "system:/test");
		keyDel (dup);
	}

	keyDel (key);
}

static void test_keyLock (void)
{
	printf ("Test locking\n");

	succeed_if (keyLock (0, KEY_LOCK_NAME) == -1, "no error on locking NULL key");
	succeed_if (keyLock (0, KEY_LOCK_VALUE) == -1, "no error on locking NULL key");
	succeed_if (keyLock (0, KEY_LOCK_META) == -1, "no error on locking NULL key");

	succeed_if (keyIsLocked (0, KEY_LOCK_NAME) == -1, "no error on NULL Key");
	succeed_if (keyIsLocked (0, KEY_LOCK_VALUE) == -1, "no error on NULL Key");
	succeed_if (keyIsLocked (0, KEY_LOCK_META) == -1, "no error on NULL Key");

	Key * key = keyNew ("/", KEY_LOCK_NAME, KEY_END);
	Key * key2 = keyNew ("/", KEY_LOCK_NAME, KEY_END);

	succeed_if (keySetName (key, "user:/") == -1, "read only name, not allowed to set");

	keyDel (key);
	key = keyNew ("/", KEY_LOCK_VALUE, KEY_END);

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	keyDel (key);
	key = keyNew ("/", KEY_LOCK_META, KEY_END);

	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyMeta (key, key2, "meta") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyAllMeta (key, key2) == -1, "read only meta, not allowed to set");

	keyDel (key);
	key = keyNew ("/", KEY_END);

	succeed_if (keyIsLocked (key, KEY_LOCK_NAME) == 0, "can lock name");
	keyLock (key, KEY_LOCK_NAME);
	succeed_if (keyIsLocked (key, KEY_LOCK_NAME) == KEY_LOCK_NAME, "name is locked");

	succeed_if (keySetName (key, "user:/") == -1, "read only name, not allowed to set");
	succeed_if (keyAddName (key, "a") == -1, "read only name, not allowed to set");
	succeed_if (keySetBaseName (key, "a") == -1, "read only name, not allowed to set");
	succeed_if (keyAddBaseName (key, "a") == -1, "read only name, not allowed to set");

	keyDel (key);
	key = keyNew ("/", KEY_END);

	succeed_if (keyIsLocked (key, KEY_LOCK_VALUE | KEY_LOCK_META) == 0, "can lock name");
	keyLock (key, KEY_LOCK_VALUE);
	succeed_if (keyIsLocked (key, KEY_LOCK_VALUE | KEY_LOCK_META) == KEY_LOCK_VALUE, "value is locked");

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	keyDel (key);
	key = keyNew ("/", KEY_END);

	succeed_if (keyIsLocked (key, KEY_LOCK_META) == 0, "can lock meta");
	keyLock (key, KEY_LOCK_META);
	succeed_if (keyIsLocked (key, KEY_LOCK_META) == KEY_LOCK_META, "meta is locked");

	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyMeta (key, key2, "meta") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyAllMeta (key, key2) == -1, "read only meta, not allowed to set");

	keyDel (key);
	keyDel (key2);
}


static void test_keyAddName (void)
{
	Key * k = keyNew ("user:/", KEY_END);
	keyAddName (k, "something");
	succeed_if_same_string (keyName (k), "user:/something");

	keyAddName (k, "with/slash");
	succeed_if_same_string (keyName (k), "user:/something/with/slash");
	keyDel (k);

#define TEST_ADD_NAME(base, toadd, result)                                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		k = keyNew (base, KEY_END);                                                                                                \
		succeed_if (keyAddName (k, toadd) == sizeof (result), "could not add name");                                               \
		succeed_if_same_string (keyName (k), result);                                                                              \
		keyDel (k);                                                                                                                \
	} while (0)

#define TEST_ADD_NAME_ERROR(base, toadd)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		k = keyNew (base, KEY_END);                                                                                                \
		succeed_if (keyAddName (k, toadd) == -1, "shouldn't be able to add name");                                                 \
		succeed_if_same_string (keyName (k), base);                                                                                \
		keyDel (k);                                                                                                                \
	} while (0)

	TEST_ADD_NAME ("spec:/", "something", "spec:/something");
	TEST_ADD_NAME ("proc:/", "something", "proc:/something");
	TEST_ADD_NAME ("dir:/", "something", "dir:/something");
	TEST_ADD_NAME ("user:/", "something", "user:/something");
	TEST_ADD_NAME ("system:/", "something", "system:/something");

	TEST_ADD_NAME ("meta:/", "something", "meta:/something");
	TEST_ADD_NAME ("meta://", "something", "meta:/something");

	TEST_ADD_NAME ("meta:/", "something/", "meta:/something");
	TEST_ADD_NAME ("meta:/", "something//", "meta:/something");

	TEST_ADD_NAME ("meta:/", "/something", "meta:/something");
	TEST_ADD_NAME ("meta:/", "//something", "meta:/something");

	TEST_ADD_NAME ("user:/", "./user", "user:/user");
	TEST_ADD_NAME ("user:/", "/./user", "user:/user");
	TEST_ADD_NAME ("user:/", "/////./user", "user:/user");
	TEST_ADD_NAME ("user:/", "../user", "user:/user");

	TEST_ADD_NAME ("user:/verylongstringtoremove", "../x", "user:/x");
	TEST_ADD_NAME ("user:/huhu", "../x", "user:/x");
	TEST_ADD_NAME ("user:/rem", "../x", "user:/x");
	TEST_ADD_NAME ("user:/more/level", "../../x", "user:/x");

	TEST_ADD_NAME ("user:/something", "../user", "user:/user");

	TEST_ADD_NAME ("/something", "user", "/something/user");
	TEST_ADD_NAME ("/", "user", "/user");
	TEST_ADD_NAME ("/s", "user", "/s/user");
	TEST_ADD_NAME ("/s", "/user", "/s/user");
	TEST_ADD_NAME ("/s", "../user", "/user");
	TEST_ADD_NAME ("/s", "..//user", "/user");
	TEST_ADD_NAME ("/more/level", "../..//user", "/user");
	TEST_ADD_NAME ("/much/more/level/1/2/3", "../../../../../..//user", "/user");

	TEST_ADD_NAME ("/s", ".../user", "/s/.../user");
	TEST_ADD_NAME ("/s", "..a/user", "/s/..a/user");
	TEST_ADD_NAME ("/s", "..../user", "/s/..../user");

	TEST_ADD_NAME ("user:/", "///sw/../sw//././MyApp", "user:/sw/MyApp");
	TEST_ADD_NAME ("user:/", "sw/../sw", "user:/sw");

	TEST_ADD_NAME_ERROR ("/", 0);
	TEST_ADD_NAME ("/", "", "/");
	TEST_ADD_NAME ("/", "/", "/");
	TEST_ADD_NAME ("/", "//", "/");
	TEST_ADD_NAME ("//", "/", "/");
	TEST_ADD_NAME ("//", "//", "/");
	TEST_ADD_NAME ("///", "//", "/");
	TEST_ADD_NAME ("//", "///", "/");
	TEST_ADD_NAME ("///", "///", "/");
	TEST_ADD_NAME ("///.", "///", "/");
	TEST_ADD_NAME ("///.", "///.", "/");
	TEST_ADD_NAME ("///.", "///./", "/");
	TEST_ADD_NAME ("///./", "///.", "/");
	TEST_ADD_NAME ("///./", "///./", "/");

	k = keyNew ("system:/elektra/mountpoints/_t_error/config", KEY_END);
	keyAddName (k, "on_open/error");
	succeed_if_same_string (keyName (k), "system:/elektra/mountpoints/_t_error/config/on_open/error");
	keyDel (k);

	k = keyNew ("user:/", KEY_END);
	succeed_if (keyAddName (k, "bar\\/foo_bar\\/") == sizeof ("user:/bar\\/foo_bar\\/"), "could not add name");
	succeed_if_same_string (keyName (k), "user:/bar\\/foo_bar\\/");
	keyDel (k);

	k = keyNew ("user:/", KEY_END);
	succeed_if (keyAddName (k, "ba\\\\/foo_bar\\/") == sizeof ("user:/ba\\\\/foo_bar\\/"), "could not add name");
	succeed_if_same_string (keyName (k), "user:/ba\\\\/foo_bar\\/");
	keyDel (k);

	k = keyNew ("user:/", KEY_END);
	succeed_if (keyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("user:/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (keyName (k), "user:/ba\\\\/foo_bar\\//%");
	keyDel (k);

	k = keyNew ("system:/", KEY_END);
	succeed_if (keyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("system:/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (keyName (k), "system:/ba\\\\/foo_bar\\//%");
	keyDel (k);

	k = keyNew ("meta:/", KEY_END);
	succeed_if (keyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("meta:/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (keyName (k), "meta:/ba\\\\/foo_bar\\//%");
	keyDel (k);

	k = keyNew ("/", KEY_END);
	succeed_if (keyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (keyName (k), "/ba\\\\/foo_bar\\//%");
	keyDel (k);

	k = keyNew ("/", KEY_END);
	succeed_if (keyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (keyName (k), "/ba\\\\/foo_bar\\//%");
	keyDel (k);

	k = keyNew ("/", KEY_END);
	succeed_if (keyAddName (k, "/\\\\/foo_bar\\//%") == sizeof ("/\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (keyName (k), "/\\\\/foo_bar\\//%");
	keyDel (k);
}

static void test_keyNeedSync (void)
{
	printf ("Test key need sync\n");

	succeed_if (keyNeedSync (0) == -1, "No error on NULL Key");

	Key * k = keyNew ("/", KEY_END);
	succeed_if (keyNeedSync (k), "fresh key should need sync");

	k->needsSync = true;
	succeed_if (keyNeedSync (k), "sync bit was set");
	k->needsSync = false;
	succeed_if (!keyNeedSync (k), "sync bit was cleared");

	keySetName (k, "/");
	succeed_if (keyNeedSync (k), "nothing done, but synced (impl-dep, could be optimized)");

	k->needsSync = false;
	keySetName (k, "user:/abc");
	succeed_if (keyNeedSync (k), "new name, should definitely need sync");

	k->needsSync = false;
	keySetString (k, "a str");
	succeed_if (keyNeedSync (k), "new string, should definitely need sync");

	k->needsSync = false;
	keySetBinary (k, "a str", 4);
	succeed_if (keyNeedSync (k), "new binary, should definitely need sync");

	k->needsSync = false;
	keySetMeta (k, "metakey", "metaval");
	succeed_if (keyNeedSync (k), "new meta, should definitely need sync");

	k->needsSync = false;
	Key * d = keyDup (k, KEY_CP_ALL);
	succeed_if (keyNeedSync (d), "dup key, should definitely need sync");

	k->needsSync = false;
	d->needsSync = false;
	succeed_if (keyCopy (d, k, 0) != NULL, "copy not successful");
	succeed_if (keyNeedSync (d), "copy key, should definitely need sync");
	succeed_if (!keyNeedSync (k), "sources sync flag should not be affected");
	keyDel (d);

	keyIncRef (k);
	succeed_if (!keyNeedSync (k), "ref counter should not affect sync");
	keyDecRef (k);
	succeed_if (!keyNeedSync (k), "ref counter should not affect sync");


	keySetName (k, "");
	k->needsSync = false;

	succeed_if (keySetBaseName (k, "") != -1, "could not set base name");
	succeed_if (keyNeedSync (k), "name set, sync should be there");

	keySetName (k, "user:/abc");
	succeed_if (keyNeedSync (k), "name set, sync should be there");

	k->needsSync = false;
	succeed_if (keySetBaseName (k, "xynz") != -1, "could not set base name");
	succeed_if (keyNeedSync (k), "base name changed, sync should be there");

	k->needsSync = false;
	succeed_if (keyAddBaseName (k, "foo") != -1, "could not add base name");
	succeed_if (keyNeedSync (k), "base name changed, sync should be there");

	k->needsSync = false;
	succeed_if (keyAddName (k, "bar") != -1, "could not add name");
	succeed_if (keyNeedSync (k), "base name changed, sync should be there");

	keyDel (k);
}

static void test_keyCopy (void)
{
	printf ("test copy key\n");
	Key * k = keyNew ("/", KEY_END);
	Key * c = keyNew ("user:/name", KEY_END);

	succeed_if (keyCopy (c, k, KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (keyName (k), "/");
	succeed_if_same_string (keyName (c), "/");

	succeed_if (keySetName (k, "/abc") != -1, "could not set cascading name");
	succeed_if (keyCopy (c, k, KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (keyName (k), "/abc");
	succeed_if_same_string (keyName (c), "/abc");

	succeed_if (keySetName (c, "spec:/test") != -1, "could not set name");
	succeed_if (keyCopy (k, c, KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (keyName (k), "spec:/test");
	succeed_if_same_string (keyName (c), "spec:/test");

	succeed_if (keySetName (c, "proc:/test") != -1, "could not set name");
	succeed_if (keyCopy (k, c, KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (keyName (k), "proc:/test");
	succeed_if_same_string (keyName (c), "proc:/test");

	succeed_if (keyCopy (k, c, KEY_CP_VALUE | KEY_CP_STRING) == NULL, "could copy despite of illegal flags");

	keyDel (k);
	keyDel (c);

	Key * keyLock = keyNew ("user:/foo", KEY_FLAGS, KEY_LOCK_NAME | KEY_LOCK_VALUE | KEY_LOCK_META, KEY_END);
	Key * keyNorm = keyNew ("user:/test", KEY_END);

	succeed_if (keyCopy (keyNorm, keyLock, KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (keyName (keyNorm), "user:/foo");
	succeed_if_same_string (keyName (keyLock), "user:/foo");

	succeed_if (keySetName (keyNorm, "user:/test") != -1, "could not set name");
	succeed_if (keyCopy (keyLock, keyNorm, KEY_CP_NAME) == NULL, "could copy locked key");
	succeed_if_same_string (keyName (keyNorm), "user:/test");
	succeed_if_same_string (keyName (keyLock), "user:/foo");

	succeed_if (keyCopy (NULL, keyNorm, KEY_CP_NAME) == NULL, "could copy to NULL");
	succeed_if (keyCopy (NULL, keyLock, KEY_CP_NAME) == NULL, "could copy to NULL");
	succeed_if_same_string (keyName (keyNorm), "user:/test");
	succeed_if_same_string (keyName (keyLock), "user:/foo");

	Key * keyBin = keyNew ("user:/binary/foo", KEY_FLAGS, KEY_BINARY, KEY_END);
	succeed_if (keyIsBinary (keyBin), "error creating binary key");

	keySetString (keyNorm, "This is a string");
	succeed_if_same_string (keyString (keyNorm), "This is a string");

	succeed_if (keyCopy (keyNorm, keyBin, KEY_CP_STRING) == NULL, "could copy string to binary key");
	succeed_if_same_string (keyName (keyNorm), "user:/test");
	succeed_if_same_string (keyString (keyNorm), "This is a string");
	succeed_if_same_string (keyName (keyBin), "user:/binary/foo");
	succeed_if (keyCopy (keyNorm, keyBin, KEY_CP_NAME) != NULL, "could not copy name to binary key");
	succeed_if_same_string (keyName (keyNorm), "user:/binary/foo");
	succeed_if_same_string (keyName (keyBin), "user:/binary/foo");

	keyDel (keyNorm);
	keyDel (keyLock);
	keyDel (keyBin);

	Key * keyValSource = keyNew ("user:/hello", KEY_VALUE, "hello", KEY_END);
	Key * keyValDest = keyNew ("user:/hi", KEY_END);
	keyValDest = keyCopy (keyValDest, keyValSource, KEY_CP_ALL);
	compare_key (keyValDest, keyValSource);

	keySetString (keyValSource, "This string has special chars \n \\ \t \a");
	keySetName (keyValDest, "user:/hi");
	keyValDest = keyCopy (keyValDest, keyValSource, KEY_CP_ALL);
	succeed_if_same_string (keyString (keyValDest), keyString (keyValSource));
	compare_key (keyValDest, keyValSource);

	keySetString (keyValSource, "This string has special \0 here");
	keySetName (keyValDest, "user:/hi");
	keyValDest = keyCopy (keyValDest, keyValSource, KEY_CP_ALL);
	succeed_if_same_string (keyString (keyValDest), keyString (keyValSource));
	compare_key (keyValDest, keyValSource);

	keyDel (keyValSource);
	keyDel (keyValDest);
}

static void test_keyFixedNew (void)
{
	printf ("test fixed new\n");
	Key * k1 = keyNew ("/", KEY_END);
	Key * k2 = keyNew ("/", KEY_SIZE, 0, KEY_VALUE, 0, KEY_END);
	compare_key (k1, k2);
	keyDel (k1);
	keyDel (k2);

	k1 = keyNew ("user:/hello", KEY_END);
	k2 = keyNew ("user:/hello", KEY_SIZE, 0, KEY_VALUE, 0, KEY_END);
	compare_key (k1, k2);
	keyDel (k1);
	keyDel (k2);

	k1 = keyNew ("user:/hello", KEY_VALUE, "hello", KEY_END);
	k2 = keyNew ("user:/hello", KEY_SIZE, 6, KEY_VALUE, "hello", KEY_END);
	compare_key (k1, k2);
	keyDel (k1);
	keyDel (k2);
}

static void test_keyFlags (void)
{
	printf ("Test KEY_FLAGS\n");

	Key * key = keyNew ("user:/foo", KEY_FLAGS, KEY_BINARY | KEY_LOCK_NAME | KEY_LOCK_VALUE | KEY_LOCK_META, KEY_END);
	Key * key2 = NULL;

	succeed_if (keyIsBinary (key), "Could not set type to binary");

	succeed_if (keySetName (key, "system:/") == -1, "read only name, not allowed to set");
	succeed_if (keyAddName (key, "bar") == -1, "read only name, not allowed to set");
	succeed_if (keyAddBaseName (key, "bar") == -1, "read only name, not allowed to set");

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyMeta (key, key2, "meta") == -1, "read only meta, not allowed to set");
	succeed_if (keyCopyAllMeta (key, key2) == -1, "read only meta, not allowed to set");

	keyDel (key);
	keyDel (key2);
}

static void test_warnings (void)
{
	printf ("Test ADD_WARNING\n");

	Key * key = keyNew ("user:/bar", KEY_VALUE, "config", KEY_END);
	for (int i = 0; i < 200; i++)
	{
#define WITH_LINENO(code)                                                                                                                  \
	const char * lineno = ELEKTRA_STRINGIFY (__LINE__);                                                                                \
	code
		WITH_LINENO (ELEKTRA_ADD_INTERFACE_WARNING (key, "reason reason reason"));
#undef WITH_LINENO

		char index[ELEKTRA_MAX_ARRAY_SIZE];
		elektraWriteArrayNumber (index, i % 100);

		printf ("  -- warning %d -> %s\n", i, index);
		succeed_if_same_string (keyString (keyGetMeta (key, "meta:/warnings")), index);

		Key * k = keyNew ("meta:/warnings", KEY_END);
		keyAddBaseName (k, index);

		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))),
					"number description  module file line mountpoint configfile reason");

		keyAddBaseName (k, "number");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), "C01320");

		keySetBaseName (k, "description");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), "Interface");

		keySetBaseName (k, "module");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME));

		keySetBaseName (k, "file");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), __FILE__);

		keySetBaseName (k, "line");
		// must be line number of ELEKTRA_ADD_INTERFACE_WARNING
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), lineno);

		keySetBaseName (k, "mountpoint");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), "user:/bar");

		keySetBaseName (k, "configfile");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), "config");

		keySetBaseName (k, "reason");
		succeed_if_same_string (keyString (keyGetMeta (key, keyName (k))), "reason reason reason");

		keyDel (k);
	}
	keyDel (key);
}

static void test_keyReplacePrefix (void)
{
	printf ("Test keyReplacePrefix\n");

	Key * key = keyNew ("user:/", KEY_END);
	Key * oldPrefix = keyNew ("user:/", KEY_END);
	Key * newPrefix = keyNew ("user:/", KEY_END);

	succeed_if (keyReplacePrefix (NULL, oldPrefix, newPrefix) == -1, "should not accept NULL argument");
	succeed_if (keyReplacePrefix (key, NULL, newPrefix) == -1, "should not accept NULL argument");
	succeed_if (keyReplacePrefix (key, oldPrefix, NULL) == -1, "should not accept NULL argument");

	key->hasReadOnlyName = true;
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == -1, "should not accept read-only key");
	key->hasReadOnlyName = false;

	keySetName (oldPrefix, "user:/foo");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys not below oldPrefix");
	succeed_if (keyGetNameSize (key) == 7, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 3, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "user:/");
	succeed_if_same_string (keyName (oldPrefix), "user:/foo");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0", 2) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "user:/bar");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys not below oldPrefix");
	succeed_if (keyGetNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 6, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "user:/bar");
	succeed_if_same_string (keyName (oldPrefix), "user:/foo");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0bar", 5) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys with different namespace than oldPrefix");
	succeed_if (keyGetNameSize (key) == 16, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "system:/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "user:/foo");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "/foo/bar");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys with different namespace than oldPrefix");
	succeed_if (keyGetNameSize (key) == 9, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "user:/foo");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_CASCADING, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	keySetName (oldPrefix, "/foo");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys with different namespace than oldPrefix");
	succeed_if (keyGetNameSize (key) == 16, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 5, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "system:/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "/foo");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	keySetName (oldPrefix, "system:/foo");
	keySetName (newPrefix, "system:/baz");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 16, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 12, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 12, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "system:/baz/bar");
	succeed_if_same_string (keyName (oldPrefix), "system:/foo");
	succeed_if_same_string (keyName (newPrefix), "system:/baz");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0baz\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	keySetName (oldPrefix, "system:/foo");
	keySetName (newPrefix, "user:/baz");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 14, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 12, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 10, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "user:/baz/bar");
	succeed_if_same_string (keyName (oldPrefix), "system:/foo");
	succeed_if_same_string (keyName (newPrefix), "user:/baz");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0baz\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	keySetName (oldPrefix, "system:/foo/bar");
	keySetName (newPrefix, "system:/baz");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 12, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 16, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 12, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 6, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "system:/baz");
	succeed_if_same_string (keyName (oldPrefix), "system:/foo/bar");
	succeed_if_same_string (keyName (newPrefix), "system:/baz");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0baz", 5) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo\0bar", 9) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	keySetName (oldPrefix, "system:/");
	keySetName (newPrefix, "user:/baz");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 18, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 9, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 10, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 14, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "user:/baz/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "system:/");
	succeed_if_same_string (keyName (newPrefix), "user:/baz");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0baz\0foo\0bar", 13) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/foo/bar");
	keySetName (oldPrefix, "system:/");
	keySetName (newPrefix, "user:/");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 14, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 9, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "user:/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "system:/");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "system:/");
	keySetName (oldPrefix, "system:/");
	keySetName (newPrefix, "user:/");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 7, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 9, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 3, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "user:/");
	succeed_if_same_string (keyName (oldPrefix), "system:/");
	succeed_if_same_string (keyName (newPrefix), "user:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0", 2) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "/foo/bar");
	keySetName (oldPrefix, "/foo");
	keySetName (newPrefix, "/baz");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 9, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 5, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 5, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "/baz/bar");
	succeed_if_same_string (keyName (oldPrefix), "/foo");
	succeed_if_same_string (keyName (newPrefix), "/baz");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_CASCADING, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_CASCADING, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0baz\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "/foo/bar");
	keySetName (oldPrefix, "/");
	keySetName (newPrefix, "system:/baz");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 20, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 2, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 12, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 14, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "system:/baz/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "/");
	succeed_if_same_string (keyName (newPrefix), "system:/baz");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0baz\0foo\0bar", 13) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	keySetName (key, "/foo/bar");
	keySetName (oldPrefix, "/");
	keySetName (newPrefix, "system:/");
	succeed_if (keyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (keyGetNameSize (key) == 16, "key size wrong");
	succeed_if (keyGetNameSize (oldPrefix) == 2, "oldPrefix size wrong");
	succeed_if (keyGetNameSize (newPrefix) == 9, "newPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (keyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (keyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (keyName (key), "system:/foo/bar");
	succeed_if_same_string (keyName (oldPrefix), "/");
	succeed_if_same_string (keyName (newPrefix), "system:/");
	succeed_if (*(char *) keyUnescapedName (key) == KEY_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (oldPrefix) == KEY_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) keyUnescapedName (newPrefix) == KEY_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) keyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) keyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	keyDel (key);
	keyDel (oldPrefix);
	keyDel (newPrefix);
}

int main (int argc, char ** argv)
{
	printf ("KEY      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_keyNameUnescape ();
	test_keySetName ();
	test_keyAddName ();

	test_keyRefcounter ();
	test_keyHelpers ();
	test_keyPlugin ();
	test_keyNewExtensions ();
	test_keyComment ();
	test_keyLock ();
	test_keyNeedSync ();
	test_keyCopy ();
	test_keyFixedNew ();
	test_keyFlags ();
	test_warnings ();
	test_keyReplacePrefix ();

	print_result ("test_key");
	return nbError;
}
