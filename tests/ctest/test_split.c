/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <../../src/libs/elektra/backend.c>
#include <../../src/libs/elektra/mount.c>
#include <../../src/libs/elektra/split.c>
#include <../../src/libs/elektra/trie.c>
#include <tests_internal.h>

KeySet * modules_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/modules", KEY_END), KS_END);
}


KeySet * simple_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/root", KEY_END),
		      keyNew ("system:/elektra/mountpoints/root/mountpoint", KEY_VALUE, "", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user:/tests/simple", KEY_END), KS_END);
}


KeySet * set_us (void)
{
	return ksNew (50, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/user", KEY_END),
		      keyNew ("system:/elektra/mountpoints/user/mountpoint", KEY_VALUE, "user", KEY_END),
		      keyNew ("system:/elektra/mountpoints/system", KEY_END),
		      keyNew ("system:/elektra/mountpoints/system/mountpoint", KEY_VALUE, "system", KEY_END), KS_END);
}


KeySet * root_config (void)
{
	return ksNew (5, keyNew ("system:/elektra/mountpoints", KEY_END), keyNew ("system:/elektra/mountpoints/root", KEY_END),
		      keyNew ("system:/elektra/mountpoints/root/mountpoint", KEY_VALUE, "/", KEY_END), KS_END);
}

#if 1 == 0
static void test_create (void)
{
	printf ("Test create split\n");

	Split * split = splitNew ();
	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "alloc not correct");

	succeed_if (split->keysets, "did not alloc keysets array");
	succeed_if (split->handles, "did not alloc handles array");

	for (size_t i = 1; i <= APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		splitAppend (split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	splitDel (split);
}

static void test_resize (void)
{
	printf ("Test resize split\n");

	Split * split = splitNew ();

	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	splitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 2, "resize not correct");

	splitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 4, "resize not correct");

	splitResize (split);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 8, "resize not correct");

	splitDel (split);
}

static void test_append (void)
{
	printf ("Test append split\n");

	Split * split = splitNew ();
	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	for (size_t i = 1; i <= APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		splitAppend (split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	for (size_t i = APPROXIMATE_NR_OF_BACKENDS + 1; i <= APPROXIMATE_NR_OF_BACKENDS * 2; ++i)
	{
		splitAppend (split, 0, 0, 0);
		succeed_if (split->size == i, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 2, "should realloc");
	}

	splitDel (split);
}

static void test_remove (void)
{
	printf ("Test remove from split\n");

	Split * split = splitNew ();
	exit_if_fail (split, "there must be a split");

	succeed_if (split->size == 0, "size should be zero");
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "initial size not correct");

	for (size_t i = 0; i < APPROXIMATE_NR_OF_BACKENDS; ++i)
	{
		splitAppend (split, 0, 0, i);
		succeed_if (split->size == i + 1, "size should be growing");
		succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	}

	splitRemove (split, 3);
	succeed_if ((int) split->syncbits[0] == 0, "syncbits not correct");
	succeed_if ((int) split->syncbits[1] == 1, "syncbits not correct");
	succeed_if ((int) split->syncbits[2] == 2, "syncbits not correct");
	succeed_if ((int) split->syncbits[3] == 4, "did not remove third?");
	succeed_if ((int) split->syncbits[4] == 5, "did not remove third?");

	splitAppend (split, 0, 0, 100);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS, "should not realloc");
	splitAppend (split, 0, 0, 200);
	succeed_if (split->alloc == APPROXIMATE_NR_OF_BACKENDS * 2, "should realloc");

	splitRemove (split, 3);
	succeed_if ((int) split->syncbits[0] == 0, "syncbits not correct");
	succeed_if ((int) split->syncbits[1] == 1, "syncbits not correct");
	succeed_if ((int) split->syncbits[2] == 2, "syncbits not correct");
	succeed_if ((int) split->syncbits[3] == 5, "did not remove third (again)?");
	succeed_if ((int) split->syncbits[4] == 6, "did not remove third (again)?");

	splitRemove (split, 0);
	succeed_if ((int) split->syncbits[0] == 1, "did not remove zeroth?");
	succeed_if ((int) split->syncbits[1] == 2, "did not remove zeroth?");
	succeed_if ((int) split->syncbits[2] == 5, "did not remove zeroth?");
	succeed_if ((int) split->syncbits[3] == 6, "did not remove zeroth?");
	succeed_if ((int) split->syncbits[4] == 7, "did not remove zeroth?");

	splitDel (split);
}
#endif

void addBackendForDivide (KeySet * backends, const char * mountpoint)
{
	BackendData data = { .backend = NULL, .keys = ksNew (0, KS_END) };
	ksAppendKey (backends, keyNew (mountpoint, KEY_BINARY, KEY_SIZE, sizeof (data), KEY_VALUE, &data, KEY_END));
}

void test_backendsDivide (void)
{
	printf ("Test backendsDivide");

	KeySet * backends = ksNew (0, KS_END);

	addBackendForDivide (backends, "dir:/");
	addBackendForDivide (backends, "user:/");
	addBackendForDivide (backends, "user:/bar");
	addBackendForDivide (backends, "user:/bar/bar");
	addBackendForDivide (backends, "user:/bar/baz");
	addBackendForDivide (backends, "user:/bar/foo");
	addBackendForDivide (backends, "user:/baz");
	addBackendForDivide (backends, "user:/foo");
	addBackendForDivide (backends, "system:/");
	addBackendForDivide (backends, "default:/");

	// clang-format off
	KeySet * ks = ksNew (20,
				keyNew ("user:/abc", KEY_END),
				keyNew ("user:/bar/abc", KEY_END),
				keyNew ("user:/bar/bar/abc", KEY_END),
				keyNew ("user:/bar/bar/def", KEY_END),
				keyNew ("user:/bar/bar/xyz", KEY_END),
				keyNew ("user:/bar/foo/abc", KEY_END),
				keyNew ("user:/bar/foo/xyz", KEY_END),
				keyNew ("user:/bar/xyz", KEY_END),
				keyNew ("user:/bak/abc", KEY_END),
				keyNew ("user:/foo/abc", KEY_END),
				keyNew ("user:/foo/xyz", KEY_END),
				keyNew ("user:/xyz", KEY_END),
				keyNew ("spec:/xyz", KEY_END),
				keyNew ("default:/xyz", KEY_END),
			     KS_END);
	// clang-format on

	succeed_if (backendsDivide (backends, ks) == 1, "couldn't split ks");

	// TODO: more thorough tests

	KeySet * ks0 = ksNew (10, KS_END);
	KeySet * ks1 = ksNew (10, keyNew ("user:/abc", KEY_END), keyNew ("user:/bak/abc", KEY_END), keyNew ("user:/xyz", KEY_END), KS_END);
	KeySet * ks2 = ksNew (10, keyNew ("user:/bar/abc", KEY_END), keyNew ("user:/bar/xyz", KEY_END), KS_END);
	KeySet * ks3 = ksNew (10, keyNew ("user:/bar/bar/abc", KEY_END), keyNew ("user:/bar/bar/def", KEY_END),
			      keyNew ("user:/bar/bar/xyz", KEY_END), KS_END);
	KeySet * ks4 = ksNew (10, KS_END);
	KeySet * ks5 = ksNew (10, keyNew ("user:/bar/foo/abc", KEY_END), keyNew ("user:/bar/foo/xyz", KEY_END), KS_END);
	KeySet * ks6 = ksNew (10, KS_END);
	KeySet * ks7 = ksNew (10, keyNew ("user:/foo/abc", KEY_END), keyNew ("user:/foo/xyz", KEY_END), KS_END);
	KeySet * ks8 = ksNew (10, KS_END);
	KeySet * ks9 = ksNew (10, keyNew ("default:/xyz", KEY_END), keyNew ("spec:/xyz", KEY_END), KS_END);

	compare_keyset (ks0, ((const BackendData *) keyValue (ksLookupByName (backends, "dir:/", 0)))->keys);
	compare_keyset (ks1, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/", 0)))->keys);
	compare_keyset (ks2, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar", 0)))->keys);
	compare_keyset (ks3, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/bar", 0)))->keys);
	compare_keyset (ks4, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/baz", 0)))->keys);
	compare_keyset (ks5, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/bar/foo", 0)))->keys);
	compare_keyset (ks6, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/baz", 0)))->keys);
	compare_keyset (ks7, ((const BackendData *) keyValue (ksLookupByName (backends, "user:/foo", 0)))->keys);
	compare_keyset (ks8, ((const BackendData *) keyValue (ksLookupByName (backends, "system:/", 0)))->keys);
	compare_keyset (ks9, ((const BackendData *) keyValue (ksLookupByName (backends, "default:/", 0)))->keys);

	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "dir:/", 0), "internal/kdb/needsync")), "1") != 0,
		    "shouldn't need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/", 0), "internal/kdb/needsync")), "1") == 0,
		    "should need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/bar", 0), "internal/kdb/needsync")), "1") == 0,
		    "should need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/bar/bar", 0), "internal/kdb/needsync")), "1") == 0,
		    "should need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/bar/baz", 0), "internal/kdb/needsync")), "1") != 0,
		    "shouldn't need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/bar/foo", 0), "internal/kdb/needsync")), "1") == 0,
		    "should need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/baz", 0), "internal/kdb/needsync")), "1") != 0,
		    "shouldn't need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "user:/foo", 0), "internal/kdb/needsync")), "1") == 0,
		    "should need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "system:/", 0), "internal/kdb/needsync")), "1") != 0,
		    "shouldn't need sync");
	succeed_if (strcmp (keyString (keyGetMeta (ksLookupByName (backends, "default:/", 0), "internal/kdb/needsync")), "1") == 0,
		    "should need sync");
}

int main (int argc, char ** argv)
{
	printf ("SPLIT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

#if 1 == 0
	test_create ();
	test_resize ();
	test_append ();
	test_remove ();
#endif

	test_backendsDivide ();

	printf ("\ntest_split RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
