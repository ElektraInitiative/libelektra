/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_ro (void)
{
	Key * key;

	key = keyNew ("/", KEY_END);
	key->hasReadOnlyValue = true;

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	key->hasReadOnlyName = true;
	succeed_if (keySetName (key, "user:/") == -1, "read only name, not allowed to set");

	key->hasReadOnlyMeta = true;
	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");

	keyDel (key);
}

static void test_comment (void)
{
	Key * key;
	char ret[10];

	succeed_if (key = keyNew ("/", KEY_END), "could not create new key");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyValue (keyGetMeta (key, "comment/#0")) == 0, "No comment up to now");

	succeed_if (keySetComment (key, 0) == 1, "could not remove comment");
	succeed_if (keyValue (keyGetMeta (key, "comment/#0")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "") == 1, "could not remove comment");
	succeed_if (keyValue (keyGetMeta (key, "comment/#0")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "mycom") == sizeof ("mycom"), "could not set comment");
	succeed_if_same_string (keyValue (keyGetMeta (key, "comment/#0")), "mycom");
	succeed_if_same_string (keyComment (key), "mycom");
	succeed_if (keyGetCommentSize (key) == sizeof ("mycom"), "My comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, 1) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, sizeof ("mycom")) == sizeof ("mycom"), "Could not get my comment");
	succeed_if_same_string (ret, "mycom");
	succeed_if (keyDel (key) == 0, "could not delete key");
}

static void test_metaArrayToKS (void)
{
	Key * test = keyNew ("/a", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END);
	KeySet * ks = elektraMetaArrayToKS (test, "dep");
	Key * cur;
	cur = ksAtCursor (ks, 0);
	succeed_if (cur && !strcmp (keyName (cur), "meta:/dep"), "failed!");
	cur = ksAtCursor (ks, 1);
	succeed_if (cur && !strcmp (keyName (cur), "meta:/dep/#0"), "failed!");
	cur = ksAtCursor (ks, 2);
	succeed_if (cur && !strcmp (keyName (cur), "meta:/dep/#1"), "failed!");
	keyDel (test);
	ksDel (ks);
}
static void checkTopArray (Key ** array, unsigned int size)
{
	unsigned int i;
	KeySet * done = ksNew (size, KS_END);
	for (i = 0; i < size; ++i)
	{
		Key * cur = array[i];
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
static void test_top (void)
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

	elektraRealloc ((void **) &array, ksGetSize (test2) * sizeof (Key *));
	memset (array, 0, ksGetSize (test2) * sizeof (Key *));
	elektraSortTopology (test2, array);

	checkTopArray (array, ksGetSize (test2));

	elektraRealloc ((void **) &array, ksGetSize (test3) * sizeof (Key *));
	memset (array, 0, ksGetSize (test3) * sizeof (Key *));
	elektraSortTopology (test3, array);

	checkTopArray (array, ksGetSize (test3));

	elektraRealloc ((void **) &array, ksGetSize (test0) * sizeof (Key *));
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


	elektraRealloc ((void **) &array, ksGetSize (testCycle) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycle2) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycle3) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle3, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycle4) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycle4, array) == 0, "Cycle detection failed\n");

	KeySet * orderTest1 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/b", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_META, "order", "#1", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#0", KEY_END), KS_END);
	elektraRealloc ((void **) &array, ksGetSize (orderTest1) * sizeof (Key *));
	memset (array, 0, ksGetSize (orderTest1) * sizeof (Key *));
	int ret = elektraSortTopology (orderTest1, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder1 (array);


	KeySet * orderTest2 = ksNew (
		10, keyNew ("/a", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_END),
		keyNew ("/b", KEY_VALUE, "b, c", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/b", KEY_META, "dep/#1", "/c", KEY_META,
			"order", "#0", KEY_END),
		keyNew ("/c", KEY_VALUE, "-", KEY_META, "order", "#3", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#1", KEY_END), KS_END);
	memset (array, 0, ksGetSize (orderTest2) * sizeof (Key *));
	ret = elektraSortTopology (orderTest2, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder2 (array);


	KeySet * orderTest3 = ksNew (
		10,
		keyNew ("/a", KEY_VALUE, "b", KEY_META, "dep", "#1", KEY_META, "dep/#0", "/a", KEY_META, "dep/#1", "/b", KEY_META, "order",
			"#2", KEY_END),
		keyNew ("/b", KEY_VALUE, "b", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/b", KEY_META, "order", "#0", KEY_END),
		keyNew ("/c", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#1", KEY_END),
		keyNew ("/d", KEY_VALUE, "d", KEY_META, "dep", "#0", KEY_META, "dep/#0", "/d", KEY_META, "order", "#5", KEY_END), KS_END);
	memset (array, 0, ksGetSize (orderTest3) * sizeof (Key *));
	ret = elektraSortTopology (orderTest3, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder3 (array);


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

	elektraRealloc ((void **) &array, ksGetSize (testCycleOrder1) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycleOrder1, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycleOrder2) * sizeof (Key *));
	succeed_if (elektraSortTopology (testCycleOrder2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycleOrder3) * sizeof (Key *));
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

	test_comment ();

	test_metaArrayToKS ();
	test_top ();
	printf ("\ntest_meta RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
