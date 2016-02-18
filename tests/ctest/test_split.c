/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <tests_internal.h>

KeySet * modules_config (void) { return ksNew (5, keyNew ("system/elektra/modules", KEY_END), KS_END); }


KeySet * simple_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user/tests/simple", KEY_END), KS_END);
}


KeySet * set_us ()
{
	return ksNew (50, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/user", KEY_END),
		      keyNew ("system/elektra/mountpoints/user/mountpoint", KEY_VALUE, "user", KEY_END),
		      keyNew ("system/elektra/mountpoints/system", KEY_END),
		      keyNew ("system/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END), KS_END);
}


KeySet * root_config (void)
{
	return ksNew (5, keyNew ("system/elektra/mountpoints", KEY_END), keyNew ("system/elektra/mountpoints/root", KEY_END),
		      keyNew ("system/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END), KS_END);
}


static void test_create ()
{
	printf ("Test create split\n");

	Split * split = elektraSplitNew ();
	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "alloc not correct");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");

	for (size_t i = 1; i <= APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		elektraSplitAppend (split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	elektraSplitDel (split);
}

static void test_resize ()
{
	printf ("Test resize split\n");

	Split * split = elektraSplitNew ();

	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	elektraSplitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 2, "resize not correct");

	elektraSplitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 4, "resize not correct");

	elektraSplitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 8, "resize not correct");

	elektraSplitDel (split);
}

static void test_append ()
{
	printf ("Test append split\n");

	Split * split = elektraSplitNew ();
	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	for (size_t i = 1; i <= APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		elektraSplitAppend (split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	for (size_t i = APPROXIMATE_NR_OF_BACKENDS + 1; i <= APPROXIMATE_NR_OF_BACKENDS * 2; ++i)
	{
		elektraSplitAppend (split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 2, "should realloc");
	}

	elektraSplitDel (split);
}

static void test_searchroot ()
{
	printf ("Test search root\n");

	Split * split = elektraSplitNew ();
	/* This here is in the trie */
	elektraSplitAppend (split, 0, keyNew ("user/bla/bla", KEY_END), 0);
	elektraSplitAppend (split, 0, keyNew ("user/bla/bla/something", KEY_END), 0);
	elektraSplitAppend (split, 0, keyNew ("user/bla/bla/deep/below", KEY_END), 0);

	Key * searchKey = keyNew ("user/bla/bla/deep/below", KEY_END);
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName (searchKey, "user/bla/bla/something");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName (searchKey, "user/bla/bla");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName (searchKey, "user/bla/bla/somewhere");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName (searchKey, "user/bla/bla/somewhere/else");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 1, "is full in it");
	keySetName (searchKey, "user/bla");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");
	keySetName (searchKey, "user/somewhere/else");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");
	keySetName (searchKey, "system");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root (mmh, cant be)");
	keySetName (searchKey, "user/bla/somewhere");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");
	keySetName (searchKey, "user/bla/somewhere/else");
	succeed_if (elektraSplitSearchRoot (split, searchKey) == 0, "is NOT full in it, need root");

	keyDel (searchKey);
	elektraSplitDel (split);
}

static void test_remove ()
{
	printf ("Test remove from split\n");

	Split * split = elektraSplitNew ();
	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	for (size_t i = 0; i < APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		elektraSplitAppend (split, 0, 0, i);
		succeed_if (split->size == i + 1, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	elektraSplitRemove (split, 3);
	succeed_if ((int)split->syncbits[0] == 0, "syncbits not correct");
	succeed_if ((int)split->syncbits[1] == 1, "syncbits not correct");
	succeed_if ((int)split->syncbits[2] == 2, "syncbits not correct");
	succeed_if ((int)split->syncbits[3] == 4, "did not remove third?");
	succeed_if ((int)split->syncbits[4] == 5, "did not remove third?");

	elektraSplitAppend (split, 0, 0, 100);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	elektraSplitAppend (split, 0, 0, 200);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 2, "should realloc");

	elektraSplitRemove (split, 3);
	succeed_if ((int)split->syncbits[0] == 0, "syncbits not correct");
	succeed_if ((int)split->syncbits[1] == 1, "syncbits not correct");
	succeed_if ((int)split->syncbits[2] == 2, "syncbits not correct");
	succeed_if ((int)split->syncbits[3] == 5, "did not remove third (again)?");
	succeed_if ((int)split->syncbits[4] == 6, "did not remove third (again)?");

	elektraSplitRemove (split, 0);
	succeed_if ((int)split->syncbits[0] == 1, "did not remove zeroth?");
	succeed_if ((int)split->syncbits[1] == 2, "did not remove zeroth?");
	succeed_if ((int)split->syncbits[2] == 5, "did not remove zeroth?");
	succeed_if ((int)split->syncbits[3] == 6, "did not remove zeroth?");
	succeed_if ((int)split->syncbits[4] == 7, "did not remove zeroth?");

	elektraSplitDel (split);
}


int main (int argc, char ** argv)
{
	printf ("SPLIT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_create ();
	test_resize ();
	test_append ();
	test_searchroot ();
	test_remove ();

	printf ("\ntest_split RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
