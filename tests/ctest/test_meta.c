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

	key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	key->flags |= ELEKTRA_KEY_FLAG_RO_VALUE;

	succeed_if (elektraKeySetString (key, "a") == -1, "read only string, not allowed to set");
	succeed_if (elektraKeySetBinary (key, "a", 2) == -1, "read only string, not allowed to set");

	key->flags |= ELEKTRA_KEY_FLAG_RO_NAME;
	succeed_if (elektraKeySetName (key, "user:/") == -1, "read only name, not allowed to set");

	key->flags |= ELEKTRA_KEY_FLAG_RO_META;
	succeed_if (elektraKeySetMeta (key, "meta", "value") == -1, "read only meta, not allowed to set");

	elektraKeyDel (key);
}

static void test_comment (void)
{
	ElektraKey * key;
	char ret[10];

	succeed_if (key = elektraKeyNew ("/", ELEKTRA_KEY_END), "could not create new key");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "comment")) == 0, "No comment up to now");

	succeed_if (keySetComment (key, 0) == 1, "could not remove comment");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "comment")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "") == 1, "could not remove comment");
	succeed_if (elektraKeyValue (elektraKeyGetMeta (key, "comment")) == 0, "There should be an no comment");
	succeed_if_same_string (keyComment (key), "");
	succeed_if (keyGetCommentSize (key) == 1, "Empty comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get empty comment");
	succeed_if (keyGetComment (key, ret, 1) == 1, "Could not get empty comment");
	succeed_if (ret[0] == 0, "keyGetComment did not return empty comment");

	succeed_if (keySetComment (key, "mycom") == sizeof ("mycom"), "could not set comment");
	succeed_if_same_string (elektraKeyValue (elektraKeyGetMeta (key, "comment")), "mycom");
	succeed_if_same_string (keyComment (key), "mycom");
	succeed_if (keyGetCommentSize (key) == sizeof ("mycom"), "My comment size problem");
	succeed_if (keyGetComment (key, ret, 0) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, 1) == -1, "Could not get my comment");
	succeed_if (keyGetComment (key, ret, sizeof ("mycom")) == sizeof ("mycom"), "Could not get my comment");
	succeed_if_same_string (ret, "mycom");
	succeed_if (elektraKeyDel (key) == 0, "could not delete key");
}

static void test_metaArrayToKS (void)
{
	ElektraKey * test = elektraKeyNew ("/a", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraMetaArrayToKS (test, "dep");
	ElektraKey * cur;
	cur = elektraKeysetNext (ks);
	succeed_if (cur && !strcmp (elektraKeyName (cur), "meta:/dep"), "failed!");
	cur = elektraKeysetNext (ks);
	succeed_if (cur && !strcmp (elektraKeyName (cur), "meta:/dep/#0"), "failed!");
	cur = elektraKeysetNext (ks);
	succeed_if (cur && !strcmp (elektraKeyName (cur), "meta:/dep/#1"), "failed!");
	elektraKeyDel (test);
	elektraKeysetDel (ks);
}
static void checkTopArray (ElektraKey ** array, unsigned int size)
{
	unsigned int i;
	ElektraKeyset * done = elektraKeysetNew (size, ELEKTRA_KS_END);
	for (i = 0; i < size; ++i)
	{
		ElektraKey * cur = array[i];
		ElektraKeyset * deps = elektraMetaArrayToKS (cur, "dep");
		ElektraKey * dep;
		elektraKeysetRewind (deps);
		elektraKeysetNext (deps);
		elektraKeysetRewind (done);
		while ((dep = elektraKeysetNext (deps)) != NULL)
		{
			if (!strcmp (elektraKeyName (cur), elektraKeyString (dep))) continue;
			ElektraKey * ret = elektraKeysetLookupByName (done, elektraKeyString (dep), ELEKTRA_KDB_O_NONE);
			succeed_if (ret != NULL, "Failed, dependency not resolved correctly\n");
		}
		elektraKeysetDel (deps);
		elektraKeysetAppendKey (done, cur);
	}
	elektraKeysetDel (done);
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
	ElektraKeyset * test0 = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "whatever", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * test1 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * test2 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b, d", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/d", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "c, e", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "dep/#1", "/e", ELEKTRA_KEY_END),
		elektraKeyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
		elektraKeyNew ("/f", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		elektraKeyNew ("/g", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		elektraKeyNew ("/h", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * test3 = elektraKeysetNew (
		10, elektraKeyNew ("/5", ELEKTRA_KEY_VALUE, "11", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/11", ELEKTRA_KEY_END),
		elektraKeyNew ("/7", ELEKTRA_KEY_VALUE, "8, 11", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/8", ELEKTRA_KEY_META, "dep/#1", "/11", ELEKTRA_KEY_END),
		elektraKeyNew ("/3", ELEKTRA_KEY_VALUE, "8, 10", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/8", ELEKTRA_KEY_META, "dep/#1", "/10", ELEKTRA_KEY_END),
		elektraKeyNew ("/11", ELEKTRA_KEY_VALUE, "2, 9, 10", ELEKTRA_KEY_META, "dep", "#2", ELEKTRA_KEY_META, "dep/#0", "/2", ELEKTRA_KEY_META, "dep/#1", "/9", ELEKTRA_KEY_META,
			"dep/#2", "/10", ELEKTRA_KEY_END),
		elektraKeyNew ("/8", ELEKTRA_KEY_VALUE, "9", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/9", ELEKTRA_KEY_END),
		elektraKeyNew ("/2", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END), elektraKeyNew ("/9", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END), elektraKeyNew ("/10", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	ElektraKey ** array = elektraMalloc (elektraKeysetGetSize (test1) * sizeof (ElektraKey *));
	memset (array, 0, elektraKeysetGetSize (test1) * sizeof (ElektraKey *));
	elektraSortTopology (test1, array);

	checkTopArray (array, elektraKeysetGetSize (test1));

	elektraRealloc ((void **) &array, elektraKeysetGetSize (test2) * sizeof (ElektraKey *));
	memset (array, 0, elektraKeysetGetSize (test2) * sizeof (ElektraKey *));
	elektraSortTopology (test2, array);

	checkTopArray (array, elektraKeysetGetSize (test2));

	elektraRealloc ((void **) &array, elektraKeysetGetSize (test3) * sizeof (ElektraKey *));
	memset (array, 0, elektraKeysetGetSize (test3) * sizeof (ElektraKey *));
	elektraSortTopology (test3, array);

	checkTopArray (array, elektraKeysetGetSize (test3));

	elektraRealloc ((void **) &array, elektraKeysetGetSize (test0) * sizeof (ElektraKey *));
	memset (array, 0, elektraKeysetGetSize (test0) * sizeof (ElektraKey *));
	elektraSortTopology (test0, array);

	checkTopArray (array, elektraKeysetGetSize (test0));

	ElektraKeyset * testCycle = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_END),
				    elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * testCycle2 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b, d", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/d", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "c, e", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "dep/#1", "/e", ELEKTRA_KEY_END),
		elektraKeyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
		elektraKeyNew ("/f", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		elektraKeyNew ("/g", ELEKTRA_KEY_VALUE, "h", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/h", ELEKTRA_KEY_END),
		elektraKeyNew ("/h", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * testCycle3 = elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_END),
				     elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
				     elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_END),
				     elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_END),
				     elektraKeyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
				     elektraKeyNew ("/f", ELEKTRA_KEY_VALUE, "g", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/g", ELEKTRA_KEY_END),
				     elektraKeyNew ("/g", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * testCycle4 =
		elektraKeysetNew (10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_END),
		       elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		       elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_END),
		       elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_META, "dep/#1", "/g", ELEKTRA_KEY_END),
		       elektraKeyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_END),
		       elektraKeyNew ("/f", ELEKTRA_KEY_VALUE, "g", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/g", ELEKTRA_KEY_END),
		       elektraKeyNew ("/g", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_END), ELEKTRA_KS_END);


	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycle) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycle2) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycle3) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle3, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycle4) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycle4, array) == 0, "Cycle detection failed\n");

	ElektraKeyset * orderTest1 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	elektraRealloc ((void **) &array, elektraKeysetGetSize (orderTest1) * sizeof (ElektraKey *));
	memset (array, 0, elektraKeysetGetSize (orderTest1) * sizeof (ElektraKey *));
	int ret = elektraSortTopology (orderTest1, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder1 (array);


	ElektraKeyset * orderTest2 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "b, c", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "dep/#1", "/c", ELEKTRA_KEY_META,
			"order", "#0", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "-", ELEKTRA_KEY_META, "order", "#3", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	memset (array, 0, elektraKeysetGetSize (orderTest2) * sizeof (ElektraKey *));
	ret = elektraSortTopology (orderTest2, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder2 (array);


	ElektraKeyset * orderTest3 = elektraKeysetNew (
		10,
		elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#1", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "dep/#1", "/b", ELEKTRA_KEY_META, "order",
			"#2", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#5", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	memset (array, 0, elektraKeysetGetSize (orderTest3) * sizeof (ElektraKey *));
	ret = elektraSortTopology (orderTest3, array);
	succeed_if (ret == 1, "sort failed");
	if (ret == 1) checkTopOrder3 (array);


	ElektraKeyset * testCycleOrder1 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "1", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * testCycleOrder2 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "#2", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * testCycleOrder3 = elektraKeysetNew (
		10, elektraKeyNew ("/a", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/b", ELEKTRA_KEY_META, "order", "#2", ELEKTRA_KEY_END),
		elektraKeyNew ("/b", ELEKTRA_KEY_VALUE, "c", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/c", ELEKTRA_KEY_META, "order", "#0", ELEKTRA_KEY_END),
		elektraKeyNew ("/c", ELEKTRA_KEY_VALUE, "d", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/d", ELEKTRA_KEY_META, "order", "#1", ELEKTRA_KEY_END),
		elektraKeyNew ("/d", ELEKTRA_KEY_VALUE, "e", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/e", ELEKTRA_KEY_META, "order", "#3", ELEKTRA_KEY_END),
		elektraKeyNew ("/e", ELEKTRA_KEY_VALUE, "f", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/f", ELEKTRA_KEY_META, "order", "#5", ELEKTRA_KEY_END),
		elektraKeyNew ("/f", ELEKTRA_KEY_VALUE, "g", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/g", ELEKTRA_KEY_META, "order", "#4", ELEKTRA_KEY_END),
		elektraKeyNew ("/g", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_META, "dep", "#0", ELEKTRA_KEY_META, "dep/#0", "/a", ELEKTRA_KEY_META, "order", "#6", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycleOrder1) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycleOrder1, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycleOrder2) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycleOrder2, array) == 0, "Cycle detection failed\n");

	elektraRealloc ((void **) &array, elektraKeysetGetSize (testCycleOrder3) * sizeof (ElektraKey *));
	succeed_if (elektraSortTopology (testCycleOrder3, array) == 0, "Cycle detection failed\n");

	elektraKeysetDel (test0);
	elektraKeysetDel (test1);
	elektraKeysetDel (test2);
	elektraKeysetDel (test3);
	elektraKeysetDel (testCycle);
	elektraKeysetDel (testCycle2);
	elektraKeysetDel (testCycle3);
	elektraKeysetDel (testCycle4);
	elektraKeysetDel (orderTest1);
	elektraKeysetDel (orderTest2);
	elektraKeysetDel (orderTest3);
	elektraKeysetDel (testCycleOrder1);
	elektraKeysetDel (testCycleOrder2);
	elektraKeysetDel (testCycleOrder3);
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
