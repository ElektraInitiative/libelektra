/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbprivate.h"
#include <tests_internal.h>

#ifdef HAVE_TIME_H
#include <time.h>
#endif

static void test_keyRefcounter (void)
{
	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	key->refs = 5;
	succeed_if (key->refs == 5, "wrong ref");
	succeed_if (elektraKeyGetRef (key) == 5, "wrong ref");
	while (elektraKeyGetRef (key) > 0)
		elektraKeyDecRef (key);
	succeed_if (key->refs == 0, "wrong ref after dec");
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	succeed_if (elektraKeyDecRef (key) == 0, "should stay at minimum");
	succeed_if (elektraKeyGetRef (key) == 0, "reference counter");
	succeed_if (elektraKeyDecRef (key) == 0, "should stay at minimum");
	elektraKeyDel (key);
}

static void test_keyHelpers (void)
{
	ElektraKey *k1, *k2;

	succeed_if (elektraKeyAddBaseName (0, "s") == -1, "null pointer saftey");

	k1 = elektraKeyNew ("user:/dir1/dir2", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddBaseName (k1, 0) == -1, "Could add null basename");
	succeed_if_same_string (elektraKeyName (k1), "user:/dir1/dir2");
	succeed_if (elektraKeyAddBaseName (k1, "") == 18, "Could not add nothing to basename");
	succeed_if_same_string (elektraKeyName (k1), "user:/dir1/dir2/%");
	succeed_if (elektraKeyAddBaseName (k1, "mykey") == 24, "Could not add basename");
	succeed_if_same_string (elektraKeyName (k1), "user:/dir1/dir2/%/mykey");
	succeed_if (elektraKeyGetNameSize (k1) == 24, "Name size not correct");
	succeed_if (elektraKeyAddBaseName (k1, "mykey") == sizeof ("user:/dir1/dir2/%/mykey/mykey"), "Could not add basename");
	succeed_if_same_string (elektraKeyName (k1), "user:/dir1/dir2/%/mykey/mykey");
	succeed_if (elektraKeyGetNameSize (k1) == 30, "Name size not correct");
	succeed_if (elektraKeyAddBaseName (k1, "a") == 32, "Could not add basename");
	succeed_if_same_string (elektraKeyName (k1), "user:/dir1/dir2/%/mykey/mykey/a");
	succeed_if (elektraKeyGetNameSize (k1) == 32, "Name size not correct");
	elektraKeyDel (k1);

	{
		k2 = elektraKeyNew ("user:/dir1/dir2", ELEKTRA_KEY_END);
		char c[] = "user:/dir1/dir2/mykey\\/mykey\\/a";
		succeed_if (elektraKeyAddBaseName (k2, "mykey/mykey/a") == sizeof (c), "Could not add basename");
		succeed_if_same_string (elektraKeyName (k2), c);
		succeed_if (elektraKeyGetNameSize (k2) == sizeof (c), "Name size not correct");
		elektraKeyDel (k2);
	}

	{
		k2 = elektraKeyNew ("user:/dir1/dir2", ELEKTRA_KEY_END);
		char c[] = "user:/dir1/dir2/mykey\\/\\/\\/\\/a";
		succeed_if (elektraKeyAddBaseName (k2, "mykey////a") == sizeof (c), "Could not add basename");
		succeed_if_same_string (elektraKeyName (k2), c);
		succeed_if (elektraKeyGetNameSize (k2) == sizeof (c), "Name size not correct");
		elektraKeyDel (k2);
	}

	{
		k2 = elektraKeyNew ("user:/dir1/dir2", ELEKTRA_KEY_END);
		char c[] = "user:/dir1/dir2/mykey\\/\\/\\/\\/";
		succeed_if (elektraKeyAddBaseName (k2, "mykey////") == sizeof (c), "Could not add basename");
		succeed_if_same_string (elektraKeyName (k2), c);
		succeed_if (elektraKeyGetNameSize (k2) == sizeof (c), "Name size not correct");
		elektraKeyDel (k2);
	}

	k2 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddBaseName (k2, "user") == 6, "Could not add basename on /");
	succeed_if_same_string (elektraKeyName (k2), "/user");
	succeed_if (elektraKeyGetNameSize (k2) == 6, "Name size not correct");
	elektraKeyDel (k2);

	k2 = elektraKeyNew ("user:/dir1/dir2/mykey/mykey/a", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBaseName (k2, "mykey") == 34, "Could not add basename");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir1/dir2/mykey/mykey/mykey");
	succeed_if (elektraKeyGetNameSize (k2) == 34, "Name size not correct");
	succeed_if (elektraKeySetBaseName (k2, "einva") == 34, "Could not add basename");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir1/dir2/mykey/mykey/einva");
	succeed_if (elektraKeyGetNameSize (k2) == 34, "Name size not correct");
	succeed_if (elektraKeySetBaseName (k2, "chang") == 34, "Could not add basename");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir1/dir2/mykey/mykey/chang");
	succeed_if (elektraKeySetBaseName (k2, "change") == 35, "Could not add basename");
	succeed_if (elektraKeyGetNameSize (k2) == 35, "Name size not correct");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir1/dir2/mykey/mykey/change");
	elektraKeyDel (k2);

	k2 = elektraKeyNew ("user:/dir1/a", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBaseName (k2, 0) == 11, "Could not add basename");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir1");
	succeed_if (elektraKeyGetNameSize (k2) == 11, "Name size not correct");
	elektraKeyDel (k2);

	k2 = elektraKeyNew ("user:/dir1/a", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBaseName (k2, "some/more") == sizeof ("user:/dir1/some\\/more"), "Could not add basename");
	succeed_if_same_string (elektraKeyName (k2), "user:/dir1/some\\/more");
	succeed_if (elektraKeyGetNameSize (k2) == sizeof ("user:/dir1/some\\/more"), "Name size not correct");
	elektraKeyDel (k2);

	{
		k2 = elektraKeyNew ("user:/dir1/a", ELEKTRA_KEY_END);
		char c[] = "user:/dir1/some\\/\\/\\/\\/more";
		succeed_if (elektraKeySetBaseName (k2, "some////more") == sizeof (c), "Could not add basename");
		succeed_if_same_string (elektraKeyName (k2), c);
		succeed_if (elektraKeyGetNameSize (k2) == sizeof (c), "Name size not correct");
		elektraKeyDel (k2);
	}

	{
		k2 = elektraKeyNew ("user:/dir1/a", ELEKTRA_KEY_END);
		char c[] = "user:/dir1/\\/\\/\\/\\/more";
		succeed_if (elektraKeySetBaseName (k2, "////more") == sizeof (c), "Could not add basename");
		succeed_if_same_string (elektraKeyName (k2), c);
		succeed_if (elektraKeyGetNameSize (k2) == sizeof (c), "Name size not correct");
		elektraKeyDel (k2);
	}

	k2 = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBaseName (k2, "user") == -1, "Could add basename, but there is none");
	succeed_if_same_string (elektraKeyName (k2), "user:/");
	succeed_if (elektraKeyGetNameSize (k2) == 7, "Name size not correct");
	elektraKeyDel (k2);

	k2 = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeySetBaseName (k2, "system") == -1, "Could add basename, but there is none");
	succeed_if_same_string (elektraKeyName (k2), "system:/");
	succeed_if (elektraKeyGetNameSize (k2) == 9, "Name size not correct");
	elektraKeyDel (k2);
}

static void test_keyPlugin (void)
{
	Plugin * plug = (Plugin *) 1222243;

	ElektraKey * k = elektraKeyNew ("system:/name", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (plug), ELEKTRA_KEY_VALUE, &plug, ELEKTRA_KEY_END);
	Plugin * xlug = *(Plugin **) elektraKeyValue (k);

	succeed_if (xlug == plug, "should point to the same");
	succeed_if (plug == (Plugin *) 1222243, "should point to that");
	succeed_if (xlug == (Plugin *) 1222243, "should point to that too");

	elektraKeyDel (k);
}

static void test_keyNameUnescape (void)
{
	char buf[1024];
	char * buffer = buf;

	printf ("test elektraKeyNameUnescape\n");
	{
		char a[] = "/\\\\a";
		char s[] = "\0\0\\a";
		s[0] = ELEKTRA_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "/a\\/test";
		char s[] = "\0\0a/test";
		s[0] = ELEKTRA_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "/a\\\\\\/test";
		char s[] = "\0\0a\\/test";
		s[0] = ELEKTRA_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "/a\\\\\\\\\\/test";
		char s[] = "\0\0a\\\\/test";
		s[0] = ELEKTRA_NS_CASCADING;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}


	printf ("test elektraKeyNameUnescape (with namespace)\n");
	{
		char a[] = "user:/a/test";
		char s[] = "\0\0a\0test";
		s[0] = ELEKTRA_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "user:/a\\/test";
		char s[] = "\0\0a/test";
		s[0] = ELEKTRA_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "user:/a\\\\/test";
		char s[] = "\0\0a\\\0test";
		s[0] = ELEKTRA_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}

	{
		char a[] = "user:/\\\\/test";
		char s[] = "\0\0\\\0test";
		s[0] = ELEKTRA_NS_USER;
		elektraKeyNameUnescape (a, buffer);
		succeed_if (!memcmp (buffer, s, sizeof (s)), "unescaped name wrong");
	}
}

static void test_keyCompare (void)
{
	printf ("test keyCompare\n");
	ElektraKey * key1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * key2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCompare (key1, key2) == 0, "the keys don't differ of course");

	elektraKeySetName (key1, "user:/myname");
	succeed_if_same_string (elektraKeyName (key1), "user:/myname");
	succeed_if (elektraKeyCompare (key1, key2) == ELEKTRA_KEY_NAME, "the keys should differ in name");
	elektraKeySetName (key2, "user:/myname");
	succeed_if (elektraKeyCompare (key1, key2) == 0, "the keys should not differ in name");

	elektraKeySetString (key1, "myvalue");
	succeed_if (elektraKeyCompare (key1, key2) == ELEKTRA_KEY_VALUE, "the keys should differ in value");
	elektraKeySetString (key2, "myvalue");
	succeed_if (elektraKeyCompare (key1, key2) == 0, "the keys should not differ in value");

	keySetComment (key1, "mycomment");
	succeed_if (elektraKeyCompare (key1, key2) == (ELEKTRA_KEY_COMMENT | ELEKTRA_KEY_META), "the keys should differ in comment");
	keySetComment (key2, "mycomment");
	succeed_if (elektraKeyCompare (key1, key2) == 0, "the keys should not differ in comment");

	elektraKeyDel (key1);
	elektraKeyDel (key2);
}

static void test_keyNewExtensions (void)
{
	printf ("test keyNewExtensions\n");

	ElektraKey * key;

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyIsUser (key) == 0, "empty user key");
	succeed_if (elektraKeyIsSystem (key) == 0, "empty user key?");
	succeed_if (elektraKeyDel (key) == 0, "keyDel: Unable to delete key with name + mode");
}

static void test_keyComment (void)
{
	ElektraKey * key;
	char ret[1000];
	size_t i;
	char testComment[] = "testcomment";

	printf ("Test comment of key\n");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
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
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keySetComment (key, "") == 1, "could not set comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	for (i = 1; i < 256; i++)
	{
		ret[0] = i;
		ret[1] = i;
		ret[2] = 0;
		succeed_if (keySetComment (key, ret) == 3, "could not set comment");
		// output_key (key);
		succeed_if_same_string (keyComment (key), ret);
	}
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");


	printf ("Test comment of key 2\n");

	succeed_if (keyComment (0) == 0, "null pointer");
	succeed_if (keyGetCommentSize (0) == -1, "null pointer");
	succeed_if (keySetComment (0, "") == -1, "null pointer");

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
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
	elektraKeyDel (key);
}

static void test_keySetName (void)
{
	printf ("test keySetName\n");

	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * dup = 0;

	succeed_if (elektraKeySetName (key, "/") != -1, "could not set cascading name");
	succeed_if_same_string (elektraKeyName (key), "/");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "/");
	elektraKeyDel (dup);

	elektraKeySetName (key, "/c");
	succeed_if_same_string (elektraKeyName (key), "/c");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "/c");
	elektraKeyDel (dup);

	succeed_if (elektraKeySetName (key, "/") != -1, "could not set cascading name");
	succeed_if_same_string (elektraKeyName (key), "/");
	elektraKeySetName (key, "/cascading");
	succeed_if_same_string (elektraKeyName (key), "/cascading");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "/cascading");
	elektraKeyDel (dup);

	elektraKeySetName (key, "/cascading/s/deep/below");
	succeed_if_same_string (elektraKeyName (key), "/cascading/s/deep/below");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "/cascading/s/deep/below");
	elektraKeyDel (dup);

	elektraKeySetName (key, "user:/cascading/s/deep/below");
	succeed_if_same_string (elektraKeyName (key), "user:/cascading/s/deep/below");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "user:/cascading/s/deep/below");
	elektraKeyDel (dup);

	elektraKeySetName (key, "system:/cascading/s/deep/below");
	succeed_if_same_string (elektraKeyName (key), "system:/cascading/s/deep/below");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "system:/cascading/s/deep/below");
	elektraKeyDel (dup);

	elektraKeySetName (key, "meta:/order");
	succeed_if_same_string (elektraKeyName (key), "meta:/order");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "meta:/order");
	elektraKeyDel (dup);

	elektraKeySetName (key, "meta:/check/type");
	succeed_if_same_string (elektraKeyName (key), "meta:/check/type");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "meta:/check/type");
	elektraKeyDel (dup);

	elektraKeySetName (key, "meta:/a");
	succeed_if_same_string (elektraKeyName (key), "meta:/a");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "meta:/a");
	elektraKeyDel (dup);

	succeed_if (elektraKeySetName (key, "") == -1, "setting emtpy name should fail");
	succeed_if_same_string (elektraKeyName (key), "meta:/a");

	succeed_if (elektraKeySetName (key, 0) == -1, "setting null name should fail");
	succeed_if_same_string (elektraKeyName (key), "meta:/a");

	elektraKeySetName (key, "/cascading");
	succeed_if_same_string (elektraKeyName (key), "/cascading");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "/cascading");
	elektraKeyDel (dup);

	elektraKeySetName (key, "meta:/");
	succeed_if_same_string (elektraKeyName (key), "meta:/");
	succeed_if (key->key != 0, "null pointer?");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "meta:/");
	elektraKeyDel (dup);

	elektraKeySetName (key, "meta:/other");
	succeed_if_same_string (elektraKeyName (key), "meta:/other");
	succeed_if (key->key != 0, "null pointer?");
	dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyName (dup), "meta:/other");
	elektraKeyDel (dup);

	for (int i = 0; i < 8; ++i)
	{
		elektraKeySetName (key, "spec:/test");
		succeed_if_same_string (elektraKeyName (key), "spec:/test");
		dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		succeed_if_same_string (elektraKeyName (dup), "spec:/test");
		elektraKeyDel (dup);

		elektraKeySetName (key, "proc:/test");
		succeed_if_same_string (elektraKeyName (key), "proc:/test");
		dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		succeed_if_same_string (elektraKeyName (dup), "proc:/test");
		elektraKeyDel (dup);

		elektraKeySetName (key, "dir:/test");
		succeed_if_same_string (elektraKeyName (key), "dir:/test");
		dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		succeed_if_same_string (elektraKeyName (dup), "dir:/test");
		elektraKeyDel (dup);

		elektraKeySetName (key, "user:/test");
		succeed_if_same_string (elektraKeyName (key), "user:/test");
		dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		succeed_if_same_string (elektraKeyName (dup), "user:/test");
		elektraKeyDel (dup);

		elektraKeySetName (key, "system:/test");
		succeed_if_same_string (elektraKeyName (key), "system:/test");
		dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		succeed_if_same_string (elektraKeyName (dup), "system:/test");
		elektraKeyDel (dup);
	}

	elektraKeyDel (key);
}

static void test_keyLock (void)
{
	printf ("Test locking\n");

	succeed_if (elektraKeyLock (0, ELEKTRA_KEY_LOCK_NAME) == -1, "no error on locking NULL key");
	succeed_if (elektraKeyLock (0, ELEKTRA_KEY_LOCK_VALUE) == -1, "no error on locking NULL key");
	succeed_if (elektraKeyLock (0, ELEKTRA_KEY_LOCK_META) == -1, "no error on locking NULL key");

	succeed_if (elektraKeyIsLocked (0, ELEKTRA_KEY_LOCK_NAME) == -1, "no error on NULL Key");
	succeed_if (elektraKeyIsLocked (0, ELEKTRA_KEY_LOCK_VALUE) == -1, "no error on NULL Key");
	succeed_if (elektraKeyIsLocked (0, ELEKTRA_KEY_LOCK_META) == -1, "no error on NULL Key");

	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_LOCK_NAME, ELEKTRA_KEY_END);
	ElektraKey * key2 = elektraKeyNew ("/", ELEKTRA_KEY_LOCK_NAME, ELEKTRA_KEY_END);

	succeed_if (elektraKeySetName (key, "user:/") == -1, "read only name, not allowed to set");

	elektraKeyDel (key);
	key = elektraKeyNew ("/", ELEKTRA_KEY_LOCK_VALUE, ELEKTRA_KEY_END);

	succeed_if (elektraKeySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (elektraKeySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	elektraKeyDel (key);
	key = elektraKeyNew ("/", ELEKTRA_KEY_LOCK_META, ELEKTRA_KEY_END);

	succeed_if (elektraKeySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (elektraKeyCopyMeta (key, key2, "meta") == -1, "read only meta, not allowed to set");
	succeed_if (elektraKeyCopyAllMeta (key, key2) == -1, "read only meta, not allowed to set");

	elektraKeyDel (key);
	key = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyIsLocked (key, ELEKTRA_KEY_LOCK_NAME) == 0, "can lock name");
	elektraKeyLock (key, ELEKTRA_KEY_LOCK_NAME);
	succeed_if (elektraKeyIsLocked (key, ELEKTRA_KEY_LOCK_NAME) == ELEKTRA_KEY_LOCK_NAME, "name is locked");

	succeed_if (elektraKeySetName (key, "user:/") == -1, "read only name, not allowed to set");
	succeed_if (elektraKeyAddName (key, "a") == -1, "read only name, not allowed to set");
	succeed_if (elektraKeySetBaseName (key, "a") == -1, "read only name, not allowed to set");
	succeed_if (elektraKeyAddBaseName (key, "a") == -1, "read only name, not allowed to set");

	elektraKeyDel (key);
	key = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyIsLocked (key, ELEKTRA_KEY_LOCK_VALUE | ELEKTRA_KEY_LOCK_META) == 0, "can lock name");
	elektraKeyLock (key, ELEKTRA_KEY_LOCK_VALUE);
	succeed_if (elektraKeyIsLocked (key, ELEKTRA_KEY_LOCK_VALUE | ELEKTRA_KEY_LOCK_META) == ELEKTRA_KEY_LOCK_VALUE, "value is locked");

	succeed_if (elektraKeySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (elektraKeySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	elektraKeyDel (key);
	key = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyIsLocked (key, ELEKTRA_KEY_LOCK_META) == 0, "can lock meta");
	elektraKeyLock (key, ELEKTRA_KEY_LOCK_META);
	succeed_if (elektraKeyIsLocked (key, ELEKTRA_KEY_LOCK_META) == ELEKTRA_KEY_LOCK_META, "meta is locked");

	succeed_if (elektraKeySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (elektraKeyCopyMeta (key, key2, "meta") == -1, "read only meta, not allowed to set");
	succeed_if (elektraKeyCopyAllMeta (key, key2) == -1, "read only meta, not allowed to set");

	elektraKeyDel (key);
	elektraKeyDel (key2);
}


static void test_keyAddName (void)
{
	ElektraKey * k = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	elektraKeyAddName (k, "something");
	succeed_if_same_string (elektraKeyName (k), "user:/something");

	elektraKeyAddName (k, "with/slash");
	succeed_if_same_string (elektraKeyName (k), "user:/something/with/slash");
	elektraKeyDel (k);

#define TEST_ADD_NAME(base, toadd, result)                                                                                                 \
	do                                                                                                                                 \
	{                                                                                                                                  \
		k = keyNew (base, ELEKTRA_KEY_END);                                                                                                \
		succeed_if (keyAddName (k, toadd) == sizeof (result), "could not add name");                                               \
		succeed_if_same_string (keyName (k), result);                                                                              \
		keyDel (k);                                                                                                                \
	} while (0)

#define TEST_ADD_NAME_ERROR(base, toadd)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		k = keyNew (base, ELEKTRA_KEY_END);                                                                                                \
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

	k = elektraKeyNew ("system:/elektra/mountpoints/_t_error/config", ELEKTRA_KEY_END);
	elektraKeyAddName (k, "on_open/error");
	succeed_if_same_string (elektraKeyName (k), "system:/elektra/mountpoints/_t_error/config/on_open/error");
	elektraKeyDel (k);

	k = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "bar\\/foo_bar\\/") == sizeof ("user:/bar\\/foo_bar\\/"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "user:/bar\\/foo_bar\\/");
	elektraKeyDel (k);

	k = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "ba\\\\/foo_bar\\/") == sizeof ("user:/ba\\\\/foo_bar\\/"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "user:/ba\\\\/foo_bar\\/");
	elektraKeyDel (k);

	k = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("user:/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "user:/ba\\\\/foo_bar\\//%");
	elektraKeyDel (k);

	k = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("system:/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "system:/ba\\\\/foo_bar\\//%");
	elektraKeyDel (k);

	k = elektraKeyNew ("meta:/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("meta:/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "meta:/ba\\\\/foo_bar\\//%");
	elektraKeyDel (k);

	k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "/ba\\\\/foo_bar\\//%");
	elektraKeyDel (k);

	k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "ba\\\\/foo_bar\\//%") == sizeof ("/ba\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "/ba\\\\/foo_bar\\//%");
	elektraKeyDel (k);

	k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyAddName (k, "/\\\\/foo_bar\\//%") == sizeof ("/\\\\/foo_bar\\//%"), "could not add name");
	succeed_if_same_string (elektraKeyName (k), "/\\\\/foo_bar\\//%");
	elektraKeyDel (k);
}

static void test_keyNeedSync (void)
{
	printf ("Test key need sync\n");

	succeed_if (elektraKeyNeedSync (0) == -1, "No error on NULL Key");

	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	succeed_if (elektraKeyNeedSync (k), "fresh key should need sync");

	set_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	succeed_if (elektraKeyNeedSync (k), "sync bit was set");
	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	succeed_if (!elektraKeyNeedSync (k), "sync bit was cleared");

	elektraKeySetName (k, "/");
	succeed_if (elektraKeyNeedSync (k), "nothing done, but synced (impl-dep, could be optimized)");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	elektraKeySetName (k, "user:/abc");
	succeed_if (elektraKeyNeedSync (k), "new name, should definitely need sync");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	elektraKeySetString (k, "a str");
	succeed_if (elektraKeyNeedSync (k), "new string, should definitely need sync");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	elektraKeySetBinary (k, "a str", 4);
	succeed_if (elektraKeyNeedSync (k), "new binary, should definitely need sync");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	elektraKeySetMeta (k, "metakey", "metaval");
	succeed_if (elektraKeyNeedSync (k), "new meta, should definitely need sync");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	ElektraKey * d = elektraKeyDup (k, ELEKTRA_KEY_CP_ALL);
	succeed_if (elektraKeyNeedSync (d), "dup key, should definitely need sync");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	clear_bit (d->flags, ELEKTRA_KEY_FLAG_SYNC);
	succeed_if (elektraKeyCopy (d, k, 0) != NULL, "copy not successful");
	succeed_if (elektraKeyNeedSync (d), "copy key, should definitely need sync");
	succeed_if (!elektraKeyNeedSync (k), "sources sync flag should not be affected");
	elektraKeyDel (d);

	elektraKeyIncRef (k);
	succeed_if (!elektraKeyNeedSync (k), "ref counter should not affect sync");
	elektraKeyDecRef (k);
	succeed_if (!elektraKeyNeedSync (k), "ref counter should not affect sync");


	elektraKeySetName (k, "");
	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);

	succeed_if (elektraKeySetBaseName (k, "") != -1, "could not set base name");
	succeed_if (elektraKeyNeedSync (k), "name set, sync should be there");

	elektraKeySetName (k, "user:/abc");
	succeed_if (elektraKeyNeedSync (k), "name set, sync should be there");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	succeed_if (elektraKeySetBaseName (k, "xynz") != -1, "could not set base name");
	succeed_if (elektraKeyNeedSync (k), "base name changed, sync should be there");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	succeed_if (elektraKeyAddBaseName (k, "foo") != -1, "could not add base name");
	succeed_if (elektraKeyNeedSync (k), "base name changed, sync should be there");

	clear_bit (k->flags, ELEKTRA_KEY_FLAG_SYNC);
	succeed_if (elektraKeyAddName (k, "bar") != -1, "could not add name");
	succeed_if (elektraKeyNeedSync (k), "base name changed, sync should be there");

	elektraKeyDel (k);
}

static void test_keyCopy (void)
{
	printf ("test copy key\n");
	ElektraKey * k = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * c = elektraKeyNew ("user:/name", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCopy (c, k, ELEKTRA_KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (elektraKeyName (k), "/");
	succeed_if_same_string (elektraKeyName (c), "/");

	succeed_if (elektraKeySetName (k, "/abc") != -1, "could not set cascading name");
	succeed_if (elektraKeyCopy (c, k, ELEKTRA_KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (elektraKeyName (k), "/abc");
	succeed_if_same_string (elektraKeyName (c), "/abc");

	succeed_if (elektraKeySetName (c, "spec:/test") != -1, "could not set name");
	succeed_if (elektraKeyCopy (k, c, ELEKTRA_KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (elektraKeyName (k), "spec:/test");
	succeed_if_same_string (elektraKeyName (c), "spec:/test");

	succeed_if (elektraKeySetName (c, "proc:/test") != -1, "could not set name");
	succeed_if (elektraKeyCopy (k, c, ELEKTRA_KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (elektraKeyName (k), "proc:/test");
	succeed_if_same_string (elektraKeyName (c), "proc:/test");

	succeed_if (elektraKeyCopy (k, c, ELEKTRA_KEY_CP_VALUE | ELEKTRA_KEY_CP_STRING) == NULL, "could copy despite of illegal flags");

	elektraKeyDel (k);
	elektraKeyDel (c);

	ElektraKey * elektraKeyLock = elektraKeyNew ("user:/foo", ELEKTRA_KEY_FLAGS, ELEKTRA_KEY_LOCK_NAME | ELEKTRA_KEY_LOCK_VALUE | ELEKTRA_KEY_LOCK_META, ELEKTRA_KEY_END);
	ElektraKey * keyNorm = elektraKeyNew ("user:/test", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCopy (keyNorm, elektraKeyLock, ELEKTRA_KEY_CP_NAME) != NULL, "could not copy");
	succeed_if_same_string (elektraKeyName (keyNorm), "user:/foo");
	succeed_if_same_string (elektraKeyName (elektraKeyLock), "user:/foo");

	succeed_if (elektraKeySetName (keyNorm, "user:/test") != -1, "could not set name");
	succeed_if (elektraKeyCopy (elektraKeyLock, keyNorm, ELEKTRA_KEY_CP_NAME) == NULL, "could copy locked key");
	succeed_if_same_string (elektraKeyName (keyNorm), "user:/test");
	succeed_if_same_string (elektraKeyName (elektraKeyLock), "user:/foo");

	succeed_if (elektraKeyCopy (NULL, keyNorm, ELEKTRA_KEY_CP_NAME) == NULL, "could copy to NULL");
	succeed_if (elektraKeyCopy (NULL, elektraKeyLock, ELEKTRA_KEY_CP_NAME) == NULL, "could copy to NULL");
	succeed_if_same_string (elektraKeyName (keyNorm), "user:/test");
	succeed_if_same_string (elektraKeyName (elektraKeyLock), "user:/foo");

	ElektraKey * keyBin = elektraKeyNew ("user:/binary/foo", ELEKTRA_KEY_FLAGS, ELEKTRA_KEY_BINARY, ELEKTRA_KEY_END);
	succeed_if (elektraKeyIsBinary (keyBin), "error creating binary key");

	elektraKeySetString (keyNorm, "This is a string");
	succeed_if_same_string (elektraKeyString (keyNorm), "This is a string");

	succeed_if (elektraKeyCopy (keyNorm, keyBin, ELEKTRA_KEY_CP_STRING) == NULL, "could copy string to binary key");
	succeed_if_same_string (elektraKeyName (keyNorm), "user:/test");
	succeed_if_same_string (elektraKeyString (keyNorm), "This is a string");
	succeed_if_same_string (elektraKeyName (keyBin), "user:/binary/foo");
	succeed_if (elektraKeyCopy (keyNorm, keyBin, ELEKTRA_KEY_CP_NAME) != NULL, "could not copy name to binary key");
	succeed_if_same_string (elektraKeyName (keyNorm), "user:/binary/foo");
	succeed_if_same_string (elektraKeyName (keyBin), "user:/binary/foo");

	elektraKeyDel (keyNorm);
	elektraKeyDel (elektraKeyLock);
	elektraKeyDel (keyBin);

	ElektraKey * keyValSource = elektraKeyNew ("user:/hello", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END);
	ElektraKey * keyValDest = elektraKeyNew ("user:/hi", ELEKTRA_KEY_END);
	keyValDest = elektraKeyCopy (keyValDest, keyValSource, ELEKTRA_KEY_CP_ALL);
	compare_key (keyValDest, keyValSource);

	elektraKeySetString (keyValSource, "This string has special chars \n \\ \t \a");
	elektraKeySetName (keyValDest, "user:/hi");
	keyValDest = elektraKeyCopy (keyValDest, keyValSource, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyString (keyValDest), elektraKeyString (keyValSource));
	compare_key (keyValDest, keyValSource);

	elektraKeySetString (keyValSource, "This string has special \0 here");
	elektraKeySetName (keyValDest, "user:/hi");
	keyValDest = elektraKeyCopy (keyValDest, keyValSource, ELEKTRA_KEY_CP_ALL);
	succeed_if_same_string (elektraKeyString (keyValDest), elektraKeyString (keyValSource));
	compare_key (keyValDest, keyValSource);

	elektraKeyDel (keyValSource);
	elektraKeyDel (keyValDest);
}

static void test_keyFixedNew (void)
{
	printf ("test fixed new\n");
	ElektraKey * k1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("/", ELEKTRA_KEY_SIZE, 0, ELEKTRA_KEY_VALUE, 0, ELEKTRA_KEY_END);
	compare_key (k1, k2);
	elektraKeyDel (k1);
	elektraKeyDel (k2);

	k1 = elektraKeyNew ("user:/hello", ELEKTRA_KEY_END);
	k2 = elektraKeyNew ("user:/hello", ELEKTRA_KEY_SIZE, 0, ELEKTRA_KEY_VALUE, 0, ELEKTRA_KEY_END);
	compare_key (k1, k2);
	elektraKeyDel (k1);
	elektraKeyDel (k2);

	k1 = elektraKeyNew ("user:/hello", ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END);
	k2 = elektraKeyNew ("user:/hello", ELEKTRA_KEY_SIZE, 6, ELEKTRA_KEY_VALUE, "hello", ELEKTRA_KEY_END);
	compare_key (k1, k2);
	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

static void test_keyFlags (void)
{
	printf ("Test KEY_FLAGS\n");

	ElektraKey * key = elektraKeyNew ("user:/foo", ELEKTRA_KEY_FLAGS, ELEKTRA_KEY_BINARY | ELEKTRA_KEY_LOCK_NAME | ELEKTRA_KEY_LOCK_VALUE | ELEKTRA_KEY_LOCK_META, ELEKTRA_KEY_END);
	ElektraKey * key2 = NULL;

	succeed_if (elektraKeyIsBinary (key), "Could not set type to binary");

	succeed_if (elektraKeySetName (key, "system:/") == -1, "read only name, not allowed to set");
	succeed_if (elektraKeyAddName (key, "bar") == -1, "read only name, not allowed to set");
	succeed_if (elektraKeyAddBaseName (key, "bar") == -1, "read only name, not allowed to set");

	succeed_if (elektraKeySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (elektraKeySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	succeed_if (elektraKeySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");
	succeed_if (elektraKeyCopyMeta (key, key2, "meta") == -1, "read only meta, not allowed to set");
	succeed_if (elektraKeyCopyAllMeta (key, key2) == -1, "read only meta, not allowed to set");

	elektraKeyDel (key);
	elektraKeyDel (key2);
}

static void test_warnings (void)
{
	printf ("Test ADD_WARNING\n");

	ElektraKey * key = elektraKeyNew ("user:/bar", ELEKTRA_KEY_VALUE, "config", ELEKTRA_KEY_END);
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
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, "meta:/warnings")), index);

		ElektraKey * k = elektraKeyNew ("meta:/warnings", ELEKTRA_KEY_END);
		elektraKeyAddBaseName (k, index);

		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))),
					"number description  module file line mountpoint configfile reason");

		elektraKeyAddBaseName (k, "number");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), "C01320");

		elektraKeySetBaseName (k, "description");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), "Interface");

		elektraKeySetBaseName (k, "module");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME));

		elektraKeySetBaseName (k, "file");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), __FILE__);

		elektraKeySetBaseName (k, "line");
		// must be line number of ELEKTRA_ADD_INTERFACE_WARNING
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), lineno);

		elektraKeySetBaseName (k, "mountpoint");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), "user:/bar");

		elektraKeySetBaseName (k, "configfile");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), "config");

		elektraKeySetBaseName (k, "reason");
		succeed_if_same_string (elektraKeyString (elektraKeyGetMeta (key, elektraKeyName (k))), "reason reason reason");

		elektraKeyDel (k);
	}
	elektraKeyDel (key);
}

static void test_keyReplacePrefix (void)
{
	printf ("Test keyReplacePrefix\n");

	ElektraKey * key = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	ElektraKey * oldPrefix = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
	ElektraKey * newPrefix = elektraKeyNew ("user:/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyReplacePrefix (NULL, oldPrefix, newPrefix) == -1, "should not accept NULL argument");
	succeed_if (elektraKeyReplacePrefix (key, NULL, newPrefix) == -1, "should not accept NULL argument");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, NULL) == -1, "should not accept NULL argument");

	set_bit (key->flags, ELEKTRA_KEY_FLAG_RO_NAME);
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == -1, "should not accept read-only key");
	clear_bit (key->flags, ELEKTRA_KEY_FLAG_RO_NAME);

	elektraKeySetName (oldPrefix, "user:/foo");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys not below oldPrefix");
	succeed_if (elektraKeyGetNameSize (key) == 7, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 3, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if_same_string (elektraKeyName (oldPrefix), "user:/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0", 2) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "user:/bar");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys not below oldPrefix");
	succeed_if (elektraKeyGetNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 6, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "user:/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "user:/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0bar", 5) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys with different namespace than oldPrefix");
	succeed_if (elektraKeyGetNameSize (key) == 16, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "system:/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "user:/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "/foo/bar");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys with different namespace than oldPrefix");
	succeed_if (elektraKeyGetNameSize (key) == 9, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "user:/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_CASCADING, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_USER, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	elektraKeySetName (oldPrefix, "/foo");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 0, "shouldn't touch keys with different namespace than oldPrefix");
	succeed_if (elektraKeyGetNameSize (key) == 16, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 5, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "system:/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	elektraKeySetName (oldPrefix, "system:/foo");
	elektraKeySetName (newPrefix, "system:/baz");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 16, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 12, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 12, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "system:/baz/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "system:/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "system:/baz");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0baz\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	elektraKeySetName (oldPrefix, "system:/foo");
	elektraKeySetName (newPrefix, "user:/baz");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 14, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 12, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 10, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "user:/baz/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "system:/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/baz");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0baz\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	elektraKeySetName (oldPrefix, "system:/foo/bar");
	elektraKeySetName (newPrefix, "system:/baz");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 12, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 16, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 12, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 6, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 10, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "system:/baz");
	succeed_if_same_string (elektraKeyName (oldPrefix), "system:/foo/bar");
	succeed_if_same_string (elektraKeyName (newPrefix), "system:/baz");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0baz", 5) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo\0bar", 9) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	elektraKeySetName (oldPrefix, "system:/");
	elektraKeySetName (newPrefix, "user:/baz");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 18, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 9, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 10, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 14, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "user:/baz/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "system:/");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/baz");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0baz\0foo\0bar", 13) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/foo/bar");
	elektraKeySetName (oldPrefix, "system:/");
	elektraKeySetName (newPrefix, "user:/");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 14, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 9, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "user:/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "system:/");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "system:/");
	elektraKeySetName (oldPrefix, "system:/");
	elektraKeySetName (newPrefix, "user:/");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 7, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 9, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 7, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 3, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "user:/");
	succeed_if_same_string (elektraKeyName (oldPrefix), "system:/");
	succeed_if_same_string (elektraKeyName (newPrefix), "user:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_USER, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_SYSTEM, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_USER, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0", 2) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "/foo/bar");
	elektraKeySetName (oldPrefix, "/foo");
	elektraKeySetName (newPrefix, "/baz");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 9, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 5, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 5, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 6, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "/baz/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "/foo");
	succeed_if_same_string (elektraKeyName (newPrefix), "/baz");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_CASCADING, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_CASCADING, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0baz\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0foo", 5) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "/foo/bar");
	elektraKeySetName (oldPrefix, "/");
	elektraKeySetName (newPrefix, "system:/baz");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 20, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 2, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 12, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 14, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 6, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "system:/baz/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "/");
	succeed_if_same_string (elektraKeyName (newPrefix), "system:/baz");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0baz\0foo\0bar", 13) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0baz", 5) == 0, "newPrefix has wrong unescaped name");

	elektraKeySetName (key, "/foo/bar");
	elektraKeySetName (oldPrefix, "/");
	elektraKeySetName (newPrefix, "system:/");
	succeed_if (elektraKeyReplacePrefix (key, oldPrefix, newPrefix) == 1, "didn't correctly replace");
	succeed_if (elektraKeyGetNameSize (key) == 16, "key size wrong");
	succeed_if (elektraKeyGetNameSize (oldPrefix) == 2, "oldPrefix size wrong");
	succeed_if (elektraKeyGetNameSize (newPrefix) == 9, "newPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (key) == 10, "key size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (oldPrefix) == 3, "oldPrefix size wrong");
	succeed_if (elektraKeyGetUnescapedNameSize (newPrefix) == 3, "newPrefix size wrong");
	succeed_if_same_string (elektraKeyName (key), "system:/foo/bar");
	succeed_if_same_string (elektraKeyName (oldPrefix), "/");
	succeed_if_same_string (elektraKeyName (newPrefix), "system:/");
	succeed_if (*(char *) elektraKeyUnescapedName (key) == ELEKTRA_NS_SYSTEM, "key has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (oldPrefix) == ELEKTRA_NS_CASCADING, "oldPrefix has wrong namespace");
	succeed_if (*(char *) elektraKeyUnescapedName (newPrefix) == ELEKTRA_NS_SYSTEM, "newPrefix has wrong namespace");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (key) + 1, "\0foo\0bar", 9) == 0, "key has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (oldPrefix) + 1, "\0", 2) == 0, "oldPrefix has wrong unescaped name");
	succeed_if (memcmp ((char *) elektraKeyUnescapedName (newPrefix) + 1, "\0", 2) == 0, "newPrefix has wrong unescaped name");

	elektraKeyDel (key);
	elektraKeyDel (oldPrefix);
	elektraKeyDel (newPrefix);
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
	test_keyCompare ();
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
