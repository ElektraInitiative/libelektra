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

ElektraKeyset * modules_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/modules", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}


ElektraKeyset * simple_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/root", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/root/mountpoint", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/simple/mountpoint", ELEKTRA_KEY_VALUE, "user:/tests/simple", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}


ElektraKeyset * set_us (void)
{
	return elektraKeysetNew (50, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/user", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/user/mountpoint", ELEKTRA_KEY_VALUE, "user", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/system", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/system/mountpoint", ELEKTRA_KEY_VALUE, "system", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}


ElektraKeyset * root_config (void)
{
	return elektraKeysetNew (5, elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END), elektraKeyNew ("system:/elektra/mountpoints/root", ELEKTRA_KEY_END),
		      elektraKeyNew ("system:/elektra/mountpoints/root/mountpoint", ELEKTRA_KEY_VALUE, "/", ELEKTRA_KEY_END), ELEKTRA_KS_END);
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

void addBackendForDivide (ElektraKeyset * backends, const char * mountpoint)
{
	BackendData data = {
		.backend = NULL,
		.keys = elektraKeysetNew (0, ELEKTRA_KS_END),
		.definition = NULL,
		.plugins = NULL,
		.initialized = false,
	};
	elektraKeysetAppendKey (backends, elektraKeyNew (mountpoint, ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (data), ELEKTRA_KEY_VALUE, &data, ELEKTRA_KEY_END));
}

void test_backendsDivide (void)
{
	printf ("Test backendsDivide");

	ElektraKeyset * backends = elektraKeysetNew (0, ELEKTRA_KS_END);

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
	ElektraKeyset * ks = elektraKeysetNew (20,
				elektraKeyNew ("spec:/xyz", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/abc", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bak/abc", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/abc", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/bar/abc", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/bar/def", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/bar/xyz", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/foo/abc", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/foo/xyz", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/bar/xyz", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/foo/abc", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/foo/xyz", ELEKTRA_KEY_END),
				elektraKeyNew ("user:/xyz", ELEKTRA_KEY_END),
				elektraKeyNew ("default:/xyz", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	// clang-format on

	succeed_if (backendsDivide (backends, ks) == 1, "couldn't split ks");

	// TODO: more thorough tests

	ElektraKeyset * ks0 = elektraKeysetNew (10, ELEKTRA_KS_END);
	ElektraKeyset * ks1 = elektraKeysetNew (10, elektraKeyNew ("user:/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/bak/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/xyz", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks2 = elektraKeysetNew (10, elektraKeyNew ("user:/bar/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/bar/xyz", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks3 = elektraKeysetNew (10, elektraKeyNew ("user:/bar/bar/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/bar/bar/def", ELEKTRA_KEY_END),
			      elektraKeyNew ("user:/bar/bar/xyz", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks4 = elektraKeysetNew (10, ELEKTRA_KS_END);
	ElektraKeyset * ks5 = elektraKeysetNew (10, elektraKeyNew ("user:/bar/foo/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/bar/foo/xyz", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks6 = elektraKeysetNew (10, ELEKTRA_KS_END);
	ElektraKeyset * ks7 = elektraKeysetNew (10, elektraKeyNew ("user:/foo/abc", ELEKTRA_KEY_END), elektraKeyNew ("user:/foo/xyz", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks8 = elektraKeysetNew (10, ELEKTRA_KS_END);
	ElektraKeyset * ks9 = elektraKeysetNew (10, elektraKeyNew ("default:/xyz", ELEKTRA_KEY_END), elektraKeyNew ("spec:/xyz", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	compare_keyset (ks0, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "dir:/", 0)))->keys);
	compare_keyset (ks1, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/", 0)))->keys);
	compare_keyset (ks2, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar", 0)))->keys);
	compare_keyset (ks3, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar/bar", 0)))->keys);
	compare_keyset (ks4, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar/baz", 0)))->keys);
	compare_keyset (ks5, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar/foo", 0)))->keys);
	compare_keyset (ks6, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/baz", 0)))->keys);
	compare_keyset (ks7, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/foo", 0)))->keys);
	compare_keyset (ks8, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "system:/", 0)))->keys);
	compare_keyset (ks9, ((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "default:/", 0)))->keys);

	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "dir:/", 0)))->keyNeedsSync == false, "shouldn't need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/", 0)))->keyNeedsSync == true, "should need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar", 0)))->keyNeedsSync == true, "should need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar/bar", 0)))->keyNeedsSync == true,
		    "should need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar/baz", 0)))->keyNeedsSync == false,
		    "shouldn't need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/bar/foo", 0)))->keyNeedsSync == true,
		    "should need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/baz", 0)))->keyNeedsSync == false,
		    "shouldn't need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "user:/foo", 0)))->keyNeedsSync == true, "should need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "system:/", 0)))->keyNeedsSync == false,
		    "shouldn't need sync");
	succeed_if (((const BackendData *) elektraKeyValue (elektraKeysetLookupByName (backends, "default:/", 0)))->keyNeedsSync == true, "should need sync");
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
