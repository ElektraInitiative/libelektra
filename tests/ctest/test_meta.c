/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <kdbproposal.h>
#include <tests_internal.h>

static void test_ro ()
{
	Key * key;

	key = keyNew (0);
	key->flags |= KEY_FLAG_RO_VALUE;

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	key->flags |= KEY_FLAG_RO_NAME;
	succeed_if (keySetName (key, "user") == -1, "read only name, not allowed to set");

	key->flags |= KEY_FLAG_RO_META;
	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");

	keyDel (key);
}

static void test_uid ()
{
	Key * key;

	key = keyNew ("user/uid", KEY_UID, 100, KEY_END);
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "100");
	succeed_if (keyGetUID (key) == 100, "uid was not set correctly");

	succeed_if (keySetUID (key, 101) == 0, "could not set uid");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "101");
	succeed_if (keyGetUID (key) == 101, "uid was not set correctly");

	succeed_if (keySetUID (key, 0) == 0, "could not set uid");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "0");
	succeed_if (keyGetUID (key) == 0, "uid was not set correctly");

	succeed_if (keySetUID (key, (uid_t)-1) == 0, "could not set uid");
	warn_if_fail (!strcmp (keyValue (keyGetMeta (key, "uid")), "-1"),
		      "this is for 64bit, other platforms might have other results here");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "102") == sizeof ("102"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "102");
	succeed_if (keyGetUID (key) == 102, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "x") == sizeof ("x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "x");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "x1") == sizeof ("x1"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "x1");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "2000000") == sizeof ("2000000"), "could not set large uid");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "2000000");
	succeed_if (keyGetUID (key) == 2000000, "large uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "1x") == sizeof ("1x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "1x");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	succeed_if (keySetMeta (key, "uid", "50x") == sizeof ("50x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "uid")), "50x");
	succeed_if (keyGetUID (key) == (uid_t)-1, "uid was not set correctly");

	keyDel (key);

	key = keyNew ("user/uid", KEY_END);
	succeed_if (keyValue (keyGetMeta (key, "uid")) == 0, "got value, but uid was not set up to now");
	succeed_if (keyGetUID (key) == (uid_t)-1, "got value, but uid was not set up to now");

	keyDel (key);
}


static void test_comment ()
{
	Key * key;
	char ret[10];

	succeed_if (key = keyNew (0), "could not create new key");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyValue (keyGetMeta (key, "comment")) == 0, "No comment up to now");

	succeed_if (keySetComment (key, 0) == 1, "could not remove comment");
	succeed_if (keyValue (keyGetMeta (key, "comment")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "") == 1, "could not remove comment");
	succeed_if (keyValue (keyGetMeta (key, "comment")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "mycom") == sizeof ("mycom"), "could not set comment");
	succeed_if_same_string (keyValue (keyGetMeta (key, "comment")), "mycom");
	succeed_if_same_string (keyComment (key), "mycom");
	succeed_if (keyGetCommentSize (key) == sizeof ("mycom"), "My comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, 1) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, sizeof ("mycom")) == sizeof ("mycom"), "Could not get my comment");
	succeed_if_same_string (ret, "mycom");
	succeed_if (keyDel (key) == 0, "could not delete key");
}

static void test_owner ()
{
	Key * key;

	succeed_if (key = keyNew (0), "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("system/key", KEY_END), "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key");

	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key");
	succeed_if (keySetOwner (key, "markus") == sizeof ("markus"), "could not set owner markus");
	succeed_if_same_string (keyValue (keyGetMeta (key, "owner")), "markus");
	succeed_if_same_string (keyOwner (key), "markus");
	succeed_if (keyDel (key) == 0, "could not delete key");


	succeed_if (key = keyNew ("user:markus/key", KEY_END), "could not create new key");
	succeed_if (keySetOwner (key, "markus") == sizeof ("markus"), "could not set owner markus");
	succeed_if_same_string (keyValue (keyGetMeta (key, "owner")), "markus");
	succeed_if_same_string (keyOwner (key), "markus");
	succeed_if (keyDel (key) == 0, "could not delete key");

	setenv ("USER", "markus", 1);
	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key with env");
	succeed_if (keyValue (keyGetMeta (key, "owner")) == 0, "owner set for empty key with env");
	succeed_if_same_string (keyOwner (key), "");
	succeed_if (keyDel (key) == 0, "could not delete key with env");

	succeed_if (key = keyNew ("user/key", KEY_END), "could not create new key with env");
	succeed_if (keySetMeta (key, "owner", "myowner") == 8, "owner set for empty key with env");
	succeed_if_same_string (keyString (keyGetMeta (key, "owner")), "myowner");
	succeed_if (keyDel (key) == 0, "could not delete key with env");
}

static void test_mode ()
{
	Key * key;

	key = keyNew ("user/mode", KEY_MODE, 0100, KEY_END);
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "100");
	succeed_if (keyGetMode (key) == 0100, "mode was not set correctly");

	succeed_if (keySetMode (key, 0101) == 0, "could not set mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "101");
	succeed_if (keyGetMode (key) == 0101, "mode was not set correctly");

	succeed_if (keySetMode (key, 0) == 0, "could not set mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0");
	succeed_if (keyGetMode (key) == 0, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "102") == sizeof ("102"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "102");
	succeed_if (keyGetMode (key) == 0102, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "0103") == sizeof ("0103"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "0103");
	succeed_if (keyGetMode (key) == 0103, "mode was not set correctly with leading octal 0");

	succeed_if (keySetMeta (key, "mode", "x") == sizeof ("x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "x");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "x1") == sizeof ("x1"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "x1");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

#if SIZEOF_MODE_T > 2
	succeed_if (keySetMeta (key, "mode", "2000000") == sizeof ("2000000"), "could not set large mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "2000000");
	succeed_if (keyGetMode (key) == 02000000, "large mode was not set correctly");
#endif

	succeed_if (keySetMeta (key, "mode", "1x") == sizeof ("1x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "1x");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

	succeed_if (keySetMeta (key, "mode", "50x") == sizeof ("50x"), "could not set meta");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "50x");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "mode was not set correctly");

	keyDel (key);

	key = keyNew ("user/mode", KEY_END);
	succeed_if (keyValue (keyGetMeta (key, "mode")) == 0, "got value, but mode was not set up to now");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "KDB_FILE_MODE not default on new key");

	succeed_if (keySetMeta (key, "mode", "") == sizeof (""), "could not set large mode");
	succeed_if_same_string (keyValue (keyGetMeta (key, "mode")), "");
	succeed_if (keyGetMode (key) == KDB_FILE_MODE, "empty mode should also yield default");

	keyDel (key);
}

static void test_metaKeySet ()
{
	Key * key = keyNew ("user/test", KEY_END);
	keySetMeta (key, "meta/test1", "value1");
	keySetMeta (key, "meta/test2", "value2");
	keySetMeta (key, "meta/test3", "value3");

	KeySet * metaKeys = elektraKeyGetMetaKeySet (key);

	/* test whether the metakeyset contains all keys */
	Key * metaKey = ksLookupByName (metaKeys, "meta/test1", KDB_O_NONE);
	exit_if_fail (metaKey, "the first metakey was not found in the metakeyset");
	succeed_if (!strcmp (keyString (metaKey), "value1"), "the first metakey in the metakeyset has a wrong value");

	metaKey = ksLookupByName (metaKeys, "meta/test2", KDB_O_NONE);
	exit_if_fail (metaKey, "the second metakey was not found in the metakeyset");
	succeed_if (!strcmp (keyString (metaKey), "value2"), "the second metakey in the metakeyset has a wrong value");

	metaKey = ksLookupByName (metaKeys, "meta/test3", KDB_O_NONE);
	exit_if_fail (metaKey, "the third metakey was not found in the metakeyset");
	succeed_if (!strcmp (keyString (metaKey), "value3"), "the third metakey in the metakeyset has a wrong value");

	/* test whether the metakeyset is affected by deletions */
	ksPop (metaKeys);

	const Key * deletedKey = keyGetMeta (key, "meta/test3");
	exit_if_fail (deletedKey, "key deleted from the metakeyset is not present on the original key anymore");
	succeed_if (!strcmp (keyString (deletedKey), "value3"), "key deleted from the metakeyset has a wrong value afterwards");

	ksDel (metaKeys);
	metaKeys = elektraKeyGetMetaKeySet (key);
	ksRewind (metaKeys);
	metaKey = ksNext (metaKeys);
	keySetString (metaKey, "newvalue");

	const Key * modifiedKey = keyGetMeta (key, "meta/test1");
	succeed_if (!strcmp (keyString (modifiedKey), "value1"),
		    "metakey has incorrect value after a key from the metakeyset was modified");

	ksDel (metaKeys);
	keyDel (key);
}

static void test_metaArrayToKS ()
{
	Key * test = keyNew ("/a", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END);
	KeySet * ks = elektraMetaArrayToKS (test, "dep");
	Key * cur;
	cur = ksNext (ks);
	succeed_if (!strcmp (keyName (cur), "dep"), "failed!");
	cur = ksNext (ks);
	succeed_if (!strcmp (keyName (cur), "dep/#0"), "failed!");
	cur = ksNext (ks);
	succeed_if (!strcmp (keyName (cur), "dep/#1"), "failed!");
	keyDel (test);
	ksDel (ks);
}
static void checkTopArray (Key ** array, unsigned int size)
{
	unsigned int i;
	Key * cur;
	KeySet * done = ksNew (size, KS_END);
	for (i = 0; i < size; ++i)
	{
		cur = array[i];
		KeySet * deps = elektraMetaArrayToKS (cur, "dep");
		Key * dep;
		ksRewind (deps);
		ksNext (deps);
		ksRewind (done);
		while ((dep = ksNext (deps)) != NULL)
		{
			if (!strcmp (keyName (cur), keyString (dep))) continue;
			Key * ret = ksLookupByName (done, keyString (dep), KDB_O_NONE);
			succeed_if (ret != NULL, "Failed, dependency not resolved correctly\n");
		}
		ksDel (deps);
		ksAppendKey (done, cur);
	}
	ksDel (done);
}

#define succeed_if_top(i, s)                                                                                                               \
	{                                                                                                                                  \
		if (strcmp (keyName (array[i]), s))                                                                                        \
		{                                                                                                                          \
			fprintf (stderr, "Failed, %s should be on position %d\n", s, i);                                                   \
			succeed_if (0, "");                                                                                                \
		}                                                                                                                          \
		else                                                                                                                       \
			succeed_if (1, "");                                                                                                \
	}


static void checkTopOrder1 (Key ** array)
{
	succeed_if_top (0, "/d");
	succeed_if_top (1, "/c");
	succeed_if_top (2, "/b");
	succeed_if_top (3, "/a");
}

static void checkTopOrder2 (Key ** array)
{
	succeed_if_top (0, "/d");
	succeed_if_top (1, "/c");
	succeed_if_top (2, "/b");
	succeed_if_top (3, "/a");
}

static void checkTopOrder3 (Key ** array)
{
	succeed_if_top (0, "/b");
	succeed_if_top (1, "/d");
	succeed_if_top (2, "/c");
	succeed_if_top (3, "/a");
}
uint64_t rdtsc ()
{
	unsigned int lo, hi;
	__asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
	return ((uint64_t)hi << 32) | lo;
}
static void test_top ()
{
	KeySet * test0 = ksNew (10, keyNew ("/a", KEY_VALUE, "whatever", KEY_END), KS_END);
	KeySet * test1 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/b", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_END), KS_END);
	KeySet * test2 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, d", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/d", KEY_END),
		keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_END),
		keyNew ("/d", KEY_VALUE, "c, e", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/c", KEY_META, "dep/#1", "/e", KEY_END),
		keyNew ("/e", KEY_VALUE, "f", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/f", KEY_END),
		keyNew ("/f", KEY_VALUE, "h", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/h", KEY_END),
		keyNew ("/g", KEY_VALUE, "h", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/h", KEY_END),
		keyNew ("/h", KEY_VALUE, "-", KEY_END), KS_END);
	KeySet * test3 = ksNew (
		10, keyNew ("/5", KEY_VALUE, "11", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/11", KEY_END),
		keyNew ("/7", KEY_VALUE, "8, 11", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/8", KEY_META, "dep/#1", "/11", KEY_END),
		keyNew ("/3", KEY_VALUE, "8, 10", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/8", KEY_META, "dep/#1", "/10", KEY_END),
		keyNew ("/11", KEY_VALUE, "2, 9, 10", KEY_META, "dep", "#2", KEY_META, "dep/#0", "/2", KEY_META, "dep/#1", "/9", KEY_META,
			"dep/#2", "/10", KEY_END),
		keyNew ("/8", KEY_VALUE, "9", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/9", KEY_END),
		keyNew ("/2", KEY_VALUE, "-", KEY_END), keyNew ("/9", KEY_VALUE, "-", KEY_END), keyNew ("/10", KEY_VALUE, "-", KEY_END),
		KS_END);
	Key ** array = elektraMalloc (ksGetSize (test1) * sizeof (Key *));
	memset (array, 0, ksGetSize (test1) * sizeof (Key *));
	elektraSortTopology (test1, array);

	checkTopArray (array, ksGetSize (test1));

	elektraRealloc ((void **)&array, ksGetSize (test2) * sizeof (Key *));
	memset (array, 0, ksGetSize (test2) * sizeof (Key *));
	elektraSortTopology (test2, array);

	checkTopArray (array, ksGetSize (test2));

	elektraRealloc ((void **)&array, ksGetSize (test3) * sizeof (Key *));
	memset (array, 0, ksGetSize (test3) * sizeof (Key *));
	elektraSortTopology (test3, array);

	checkTopArray (array, ksGetSize (test3));

	elektraRealloc ((void **)&array, ksGetSize (test0) * sizeof (Key *));
	memset (array, 0, ksGetSize (test0) * sizeof (Key *));
	elektraSortTopology (test0, array);

	checkTopArray (array, ksGetSize (test0));

	KeySet * testCycle = ksNew (10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_END),
				    keyNew ("/b", KEY_VALUE, "a", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/a", KEY_END), KS_END);
	KeySet * testCycle2 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, d", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/d", KEY_END),
		keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_END),
		keyNew ("/d", KEY_VALUE, "c, e", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/c", KEY_META, "dep/#1", "/e", KEY_END),
		keyNew ("/e", KEY_VALUE, "f", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/f", KEY_END),
		keyNew ("/f", KEY_VALUE, "h", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/h", KEY_END),
		keyNew ("/g", KEY_VALUE, "h", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/h", KEY_END),
		keyNew ("/h", KEY_VALUE, "e", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/e", KEY_END), KS_END);

	KeySet * testCycle3 = ksNew (10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_END),
				     keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_END),
				     keyNew ("/c", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_END),
				     keyNew ("/d", KEY_VALUE, "e", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/e", KEY_END),
				     keyNew ("/e", KEY_VALUE, "f", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/f", KEY_END),
				     keyNew ("/f", KEY_VALUE, "g", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/g", KEY_END),
				     keyNew ("/g", KEY_VALUE, "a", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/a", KEY_END), KS_END);
	KeySet * testCycle4 =
		ksNew (10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_END),
		       keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_END),
		       keyNew ("/c", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_END),
		       keyNew ("/d", KEY_VALUE, "e", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/e", KEY_META, "dep/#1", "/g", KEY_END),
		       keyNew ("/e", KEY_VALUE, "f", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/f", KEY_END),
		       keyNew ("/f", KEY_VALUE, "g", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/g", KEY_END),
		       keyNew ("/g", KEY_VALUE, "a", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/a", KEY_END), KS_END);


	elektraRealloc ((void **)&array, ksGetSize (testCycle) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **)&array, ksGetSize (testCycle2) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **)&array, ksGetSize (testCycle3) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle3, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **)&array, ksGetSize (testCycle4) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle4, array) == 0, "Cycle detection failed\n");

	KeySet * orderTest1 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/b", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_META, "order", "#1", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#0", KEY_END), KS_END);
	elektraRealloc ((void **)&array, ksGetSize (orderTest1) * sizeof (Key *));
	memset (array, 0, ksGetSize (orderTest1) * sizeof (Key *));
	elektraSortTopology (orderTest1, array);
	checkTopOrder1 (array);


	KeySet * orderTest2 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/b", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_META,
			"order", "#0", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_META, "order", "#3", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#1", KEY_END), KS_END);
	memset (array, 0, ksGetSize (orderTest2) * sizeof (Key *));
	elektraSortTopology (orderTest2, array);
	checkTopOrder2 (array);


	KeySet * orderTest3 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/a", KEY_META, "dep/#1", "/b", KEY_META,
			    "order", "#2", KEY_END),
		keyNew ("/b", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_META, "order", "#0", KEY_END),
		keyNew ("/c", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#1", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#5", KEY_END), KS_END);
	memset (array, 0, ksGetSize (orderTest3) * sizeof (Key *));
	elektraSortTopology (orderTest3, array);
	checkTopOrder3 (array);


	KeySet * testCycleOrder1 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_META, "order", "1", KEY_END),
		keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_END),
		keyNew ("/c", KEY_VALUE, "a", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/a", KEY_META, "order", "#0", KEY_END), KS_END);

	KeySet * testCycleOrder2 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_META, "order", "#2", KEY_END),
		keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_META, "order", "#0", KEY_END),
		keyNew ("/c", KEY_VALUE, "a", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/a", KEY_META, "order", "#1", KEY_END), KS_END);

	KeySet * testCycleOrder3 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_META, "order", "#2", KEY_END),
		keyNew ("/b", KEY_VALUE, "c", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/c", KEY_META, "order", "#0", KEY_END),
		keyNew ("/c", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#1", KEY_END),
		keyNew ("/d", KEY_VALUE, "e", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/e", KEY_META, "order", "#3", KEY_END),
		keyNew ("/e", KEY_VALUE, "f", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/f", KEY_META, "order", "#5", KEY_END),
		keyNew ("/f", KEY_VALUE, "g", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/g", KEY_META, "order", "#4", KEY_END),
		keyNew ("/g", KEY_VALUE, "a", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/a", KEY_META, "order", "#6", KEY_END), KS_END);

	elektraRealloc ((void **)&array, ksGetSize (testCycleOrder1) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycleOrder1, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **)&array, ksGetSize (testCycleOrder2) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycleOrder2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **)&array, ksGetSize (testCycleOrder3) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycleOrder3, array) == 0, "Cycle detection failed\n");

	ksDel (test0);
	ksDel (test1);
	ksDel (test2);
	ksDel (test3);
	ksDel (testCycle);
	ksDel (testCycle2);
	ksDel (testCycle3);
	ksDel (testCycle4);
	ksDel (orderTest1);
	ksDel (orderTest2);
	ksDel (orderTest3);
	ksDel (testCycleOrder1);
	ksDel (testCycleOrder2);
	ksDel (testCycleOrder3);
	elektraFree (array);
}
int main (int argc, char ** argv)
{
	printf ("KEY META     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	test_ro ();

	test_uid ();
	test_comment ();
	test_owner ();
	test_mode ();
	test_metaKeySet ();

	test_metaArrayToKS ();
	test_top ();
	printf ("\ntest_meta RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
