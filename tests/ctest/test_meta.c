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
	ElektraKey * key;

	key = keyNew ("/", ELEKTRA_KEY_END);
	key->flags |= ELEKTRA_KEY_FLAG_RO_VALUE;

	succeed_if (keySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (keySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	key->flags |= ELEKTRA_KEY_FLAG_RO_NAME;
	succeed_if (keySetName (key, "user:/") == -1, "read only name, not allowed to set");

	key->flags |= ELEKTRA_KEY_FLAG_RO_META;
	succeed_if (keySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");

	keyDel (key);
}

static void test_comment (void)
{
	ElektraKey * key;
	char ret[10];

	succeed_if (key = keyNew ("/", ELEKTRA_KEY_END), "could not create new key");
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

static void test_metaArrayToKS (void)
{
	ElektraKey * test = keyNew ("/a", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraMetaArrayToKS (test, "dep");
	ElektraKey * cur;
	cur = ksNext (ks);
	succeed_if (cur && !strcmp (keyName (cur), "meta:/dep"), "failed!");
	cur = ksNext (ks);
	succeed_if (cur && !strcmp (keyName (cur), "meta:/dep/#0"), "failed!");
	cur = ksNext (ks);
	succeed_if (cur && !strcmp (keyName (cur), "meta:/dep/#1"), "failed!");
	keyDel (test);
	ksDel (ks);
}
static void checkTopArray (ElektraKey ** array, unsigned int size)
{
	unsigned int i;
	ElektraKeyset * done = ksNew (size, ELEKTRA_KS_END);
	for (i = 0; i < size; ++i)
	{
		ElektraKey * cur = array[i];
		ElektraKeyset * deps = elektraMetaArrayToKS (cur, "dep");
		ElektraKey * dep;
		ksRewind (deps);
		ksNext (deps);
		ksRewind (done);
		while ((dep = ksNext (deps)) != NULL)
		{
			if (!strcmp (keyName (cur), keyString (dep))) continue;
			ElektraKey * ret = ksLookupByName (done, keyString (dep), ELEKTRA_KDB_O_NONE);
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


static void checkTopOrder1 (ElektraKey ** array)
{
	succeed_if_top (0, "/d");
	succeed_if_top (1, "/c");
	succeed_if_top (2, "/b");
	succeed_if_top (3, "/a");
}

static void checkTopOrder2 (ElektraKey ** array)
{
	succeed_if_top (0, "/d");
	succeed_if_top (1, "/c");
	succeed_if_top (2, "/b");
	succeed_if_top (3, "/a");
}

static void checkTopOrder3 (ElektraKey ** array)
{
	succeed_if_top (0, "/b");
	succeed_if_top (1, "/d");
	succeed_if_top (2, "/c");
	succeed_if_top (3, "/a");
}
static void test_top (void)
{
	ElektraKeyset * test0 = ksNew (10, keyNew ("/a", ELEKTRA_KEY_VALUE, "whatever", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * test1 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * test2 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b, d", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/d", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "c, e", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "dep/#1", "/e", ELEKTRA_KEY_END),
		keyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
		keyNew ("/f", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		keyNew ("/g", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		keyNew ("/h", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * test3 = ksNew (
		10, keyNew ("/5", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/11", ELEKTRA_KEY_END),
		keyNew ("/7", ELEKTRA_KEY_VALUE, "8, 11", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/8", ELEKTRA_KEY_META, "dep/#1", "/11", ELEKTRA_KEY_END),
		keyNew ("/3", ELEKTRA_KEY_VALUE, "8, 10", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/8", ELEKTRA_KEY_META, "dep/#1", "/10", ELEKTRA_KEY_END),
		keyNew ("/11", ELEKTRA_KEY_VALUE, "2, 9, 10", ELEKTRA_KEY_META, "dep", "#2", ELEKTRA_KEY_META, "dep/#0", "/2", ELEKTRA_KEY_META, "dep/#1", "/9", ELEKTRA_KEY_META,
			"dep/#2", "/10", ELEKTRA_KEY_END),
		keyNew ("/8", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/9", ELEKTRA_KEY_END),
		keyNew ("/2", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END), keyNew ("/9", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END), keyNew ("/10", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	ElektraKey ** array = elektraMalloc (ksGetSize (test1) * sizeof (ElektraKey *));
	memset (array, 0, ksGetSize (test1) * sizeof (ElektraKey *));
	elektraSortTopology (test1, array);

	checkTopArray (array, ksGetSize (test1));

	elektraRealloc ((void **) &array, ksGetSize (test2) * sizeof (ElektraKey *));
	memset (array, 0, ksGetSize (test2) * sizeof (ElektraKey *));
	elektraSortTopology (test2, array);

	checkTopArray (array, ksGetSize (test2));

	elektraRealloc ((void **) &array, ksGetSize (test3) * sizeof (ElektraKey *));
	memset (array, 0, ksGetSize (test3) * sizeof (ElektraKey *));
	elektraSortTopology (test3, array);

	checkTopArray (array, ksGetSize (test3));

	elektraRealloc ((void **) &array, ksGetSize (test0) * sizeof (ElektraKey *));
	memset (array, 0, ksGetSize (test0) * sizeof (ElektraKey *));
	elektraSortTopology (test0, array);

	checkTopArray (array, ksGetSize (test0));

	ElektraKeyset * testCycle = ksNew (10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_END),
				    keyNew ("/b", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * testCycle2 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b, d", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/d", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "c, e", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "dep/#1", "/e", ELEKTRA_KEY_END),
		keyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
		keyNew ("/f", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		keyNew ("/g", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		keyNew ("/h", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * testCycle3 = ksNew (10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_END),
				     keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
				     keyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_END),
				     keyNew ("/d", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_END),
				     keyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
				     keyNew ("/f", ELEKTRA_KEY_VALUE, "g", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/g", ELEKTRA_KEY_END),
				     keyNew ("/g", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * testCycle4 =
		ksNew (10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_END),
		       keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		       keyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_END),
		       keyNew ("/d", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_META, "dep/#1", "/g", ELEKTRA_KEY_END),
		       keyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
		       keyNew ("/f", ELEKTRA_KEY_VALUE, "g", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/g", ELEKTRA_KEY_END),
		       keyNew ("/g", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);


	elektraRealloc ((void **) &array, ksGetSize (testCycle) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycle2) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycle3) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle3, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycle4) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle4, array) == 0, "Cycle detection failed\n");

	ElektraKeyset * orderTest1 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraRealloc ((void **) &array, ksGetSize (orderTest1) * sizeof (ElektraKey *));
	memset (array, 0, ksGetSize (orderTest1) * sizeof (ElektraKey *));
	int ret = elektraSortTopology (orderTest1, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder1 (array);


	ElektraKeyset * orderTest2 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_META,
			"order", "#0", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_META, "order", "#3", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	memset (array, 0, ksGetSize (orderTest2) * sizeof (ElektraKey *));
	ret = elektraSortTopology (orderTest2, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder2 (array);


	ElektraKeyset * orderTest3 = ksNew (
		10,
		keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "dep/#1", "/b", ELEKTRA_KEY_META, "order",
			"#2", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#5", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	memset (array, 0, ksGetSize (orderTest3) * sizeof (ElektraKey *));
	ret = elektraSortTopology (orderTest3, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder3 (array);


	ElektraKeyset * testCycleOrder1 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "1", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * testCycleOrder2 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "#2", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * testCycleOrder3 = ksNew (
		10, keyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "#2", ELEKTRA_KEY_END),
		keyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END),
		keyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END),
		keyNew ("/d", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_META, "order", "#3", ELEKTRA_KEY_END),
		keyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_META, "order", "#5", ELEKTRA_KEY_END),
		keyNew ("/f", ELEKTRA_KEY_VALUE, "g", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/g", ELEKTRA_KEY_META, "order", "#4", ELEKTRA_KEY_END),
		keyNew ("/g", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "order", "#6", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	elektraRealloc ((void **) &array, ksGetSize (testCycleOrder1) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycleOrder1, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycleOrder2) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycleOrder2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, ksGetSize (testCycleOrder3) * sizeof (ElektraKey *));
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
