/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

static void test_ksNew (void)
{
	ElektraKeyset * ks = 0;
	ElektraKeyset * keys = elektraKeysetNew (15, ELEKTRA_KS_END);
	ElektraKeyset * config;

	printf ("Test ks creation\n");
	exit_if_fail ((ks = elektraKeysetNew (0, ELEKTRA_KS_END)) != 0, "could not create new keyset");

	ElektraKeyset * ks2 = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetCopy (ks2, ks);
	succeed_if (elektraKeysetGetSize (ks2) == 0, "size not correct after copy");

	elektraKeysetClear (ks2); // useless, just test for double free
	elektraKeysetCopy (ks2, ks);
	succeed_if (elektraKeysetGetSize (ks2) == 0, "could not append 3 more keys");

	succeed_if (elektraKeysetDel (ks) == 0, "could not delete keyset");


	succeed_if (elektraKeysetGetSize (keys) == 0, "could not append 3 more keys");
	succeed_if (elektraKeysetGetAlloc (keys) == 15, "allocation size wrong");
	succeed_if (elektraKeysetDel (keys) == 0, "could not delete keyset");

	config = elektraKeysetNew (100, elektraKeyNew ("user:/sw/app/fixedConfiguration/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/sw/app/fixedConfiguration/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/sw/app/fixedConfiguration/key3", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (elektraKeysetGetSize (config) == 3, "could not append 3 keys in keyNew");
	succeed_if (elektraKeysetGetAlloc (config) == 100, "allocation size wrong");
	elektraKeyDel (elektraKeysetPop (config));
	succeed_if (elektraKeysetGetAlloc (config) == 49, "allocation size wrong");
	elektraKeyDel (elektraKeysetPop (config));
	succeed_if (elektraKeysetGetAlloc (config) == 24, "allocation size wrong");
	elektraKeyDel (elektraKeysetPop (config));
	succeed_if (elektraKeysetGetAlloc (config) == 15, "allocation size wrong");
	succeed_if (elektraKeysetDel (config) == 0, "could not delete keyset");

	config = elektraKeysetNew (17, elektraKeyNew ("user:/sw/app/fixedConfiguration/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/sw/app/fixedConfiguration/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/sw/app/fixedConfiguration/key3", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
			elektraKeyNew ("user:/sw/app/fixedConfiguration/key4", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (elektraKeysetGetSize (config) == 4, "could not append 5 keys in keyNew");
	succeed_if (elektraKeysetGetAlloc (config) == 17, "allocation size wrong");
	elektraKeysetAppendKey (config, elektraKeyNew ("user:/sw/app/fixedConfiguration/key6", ELEKTRA_KEY_VALUE, "value4", ELEKTRA_KEY_END));

	elektraKeysetClear (ks2);
	elektraKeysetCopy (ks2, config);
	compare_keyset (config, ks2);

	succeed_if (elektraKeysetDel (config) == 0, "could not delete keyset");
	succeed_if (elektraKeysetDel (ks2) == 0, "could not delete keyset");
}

static void test_ksDuplicate (void)
{
	printf ("Test bug duplicate\n");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("system:/duplicate", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END)) == 1, "could not append key");
	succeed_if (!strcmp (elektraKeyValue (elektraKeysetLookupByName (ks, "system:/duplicate", 0)), "abc"), "wrong value for inserted key");

	succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("system:/duplicate", ELEKTRA_KEY_VALUE, "xyz", ELEKTRA_KEY_END)) == 1, "could not append duplicate key");
	succeed_if (!strcmp (elektraKeyValue (elektraKeysetLookupByName (ks, "system:/duplicate", 0)), "xyz"), "wrong value for inserted key");

	elektraKeysetDel (ks);
}

static void test_ksHole (void)
{
	printf ("Test holes in keysets\n");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("system:/sw/new", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END)) == 1, "could not append key");
	succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("system:/sw/new/sub", ELEKTRA_KEY_VALUE, "xyz", ELEKTRA_KEY_END)) == 2, "could not append key");
	succeed_if (elektraKeysetAppendKey (ks, elektraKeyNew ("system:/sw/new/mis/sub", ELEKTRA_KEY_VALUE, "xyz", ELEKTRA_KEY_END)) == 3,
		    "could not append key which makes a hole");

	elektraKeysetDel (ks);
}

#define size 5

int fac (int i)
{
	if (i == 0)
		return 1;
	else
		return i * fac (i - 1);
}

/* Buggy, does not really yield all permutations */
static void per (int k, ElektraKey ** pool, ElektraKey ** result)
{
	int i;
	int cursize = size - 1;

	for (i = 0; i < size - 1; ++i)
	{
		// printf ("%d -- ", k);
		int selected = k % (cursize);
		k /= cursize + 1;
		cursize--;

		// copy the selected to the result
		result[i] = pool[selected];

		// remove the selected from the pool
		memmove (pool + selected,	// destination
			 pool + (selected + 1), // source
			 (size - selected - 1) * sizeof (struct ElektraKey *));
	}
	result[size - 1] = 0;
}

static void test_append (void)
{
	printf ("Test if keyset is sorted after appending\n");

	ElektraKey * key;
	ElektraKey * n;
	ElektraKey *s1, *s2, *s3;
	ElektraKeyset * ks;

	ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, key = elektraKeyNew ("system:/sw/new", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END)) == 1, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, n = elektraKeyNew ("system:/sw/new", ELEKTRA_KEY_VALUE, "xyz1", ELEKTRA_KEY_END)) == 1, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == n, "n not on position 0");
	succeed_if (ks->array[0] != key, "key is on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	elektraKeysetDel (ks);


	ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, key = elektraKeyNew ("system:/sw/new", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END)) == 1, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, n = elektraKeyNew ("system:/sw/new/sub1", ELEKTRA_KEY_VALUE, "xyz1", ELEKTRA_KEY_END)) == 2, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == n, "new key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	elektraKeysetDel (ks);


	ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, n = elektraKeyNew ("system:/sw/new/sub1", ELEKTRA_KEY_VALUE, "xyz1", ELEKTRA_KEY_END)) == 1, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == n, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, key = elektraKeyNew ("system:/sw/new", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END)) == 2, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == n, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	elektraKeysetDel (ks);


	key = elektraKeyNew ("system:/sw/new", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END);
	s1 = elektraKeyNew ("system:/sw/new/sub1", ELEKTRA_KEY_VALUE, "xyz1", ELEKTRA_KEY_END);
	s2 = elektraKeyNew ("system:/sw/new/sub2", ELEKTRA_KEY_VALUE, "xyz2", ELEKTRA_KEY_END);
	s3 = elektraKeyNew ("system:/sw/new/sub3", ELEKTRA_KEY_VALUE, "xyz3", ELEKTRA_KEY_END);
	elektraKeyIncRef (key);
	elektraKeyIncRef (s1);
	elektraKeyIncRef (s2);
	elektraKeyIncRef (s3);

	ElektraKey * solution[size] = { key, s1, s2, s3, 0 };
	ElektraKey * solutioncopy[size];
	ElektraKey * next[size];
	int i, j;

	for (i = 0; i < fac (size - 1); ++i)
	{
		memcpy (solutioncopy, solution, size * sizeof (struct ElektraKey *));
		per (i, solutioncopy, next);

		ks = elektraKeysetNew (0, ELEKTRA_KS_END);

		for (j = 0; j < size - 1; ++j)
		{
			// printf ("%p ", (void*)next[j]);
			// printf ("%s ", keyName(next[j]));
			succeed_if (elektraKeysetAppendKey (ks, next[j]) == j + 1, "could not append key");
		}
		// printf ("\nsolution is:\n");

		succeed_if (!memcmp (ks->array, solution, size), "solution is not correct");

		elektraKeysetDel (ks);
	}


	ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, key) == 1, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, s1) == 2, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, s2) == 3, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 3, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, s3) == 4, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 4, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == s3, "key not on position 3");
	succeed_if (ks->array[4] == 0, "array not null terminated");

	succeed_if (!memcmp (ks->array, solution, size), "solution is not correct");

	elektraKeysetDel (ks);


	ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (elektraKeysetAppendKey (ks, s3) == 1, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == s3, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, key) == 2, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s3, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, s1) == 3, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 3, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s3, "key not on position 2");
	succeed_if (ks->array[3] == 0, "array not null terminated");

	succeed_if (elektraKeysetAppendKey (ks, s2) == 4, "could not append key");
	succeed_if (elektraKeysetGetSize (ks) == 4, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == s3, "key not on position 3");
	succeed_if (ks->array[4] == 0, "array not null terminated");

	succeed_if (!memcmp (ks->array, solution, size), "solution is not correct");

	/*
	Key *it;
	ksRewind(ks);
	while ((it = ksNext(ks)) != 0)
	{
		printf ("%s\n", keyName(it));
	}
	*/


	elektraKeysetDel (ks);


	elektraKeyDecRef (key);
	elektraKeyDel (key);
	elektraKeyDecRef (s1);
	elektraKeyDel (s1);
	elektraKeyDecRef (s2);
	elektraKeyDel (s2);
	elektraKeyDecRef (s3);
	elektraKeyDel (s3);
}

static void test_equal (void)
{
	ElektraKey * k1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCmp (0, 0) == 0, "null pointers should be same");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	elektraKeySetName (k1, "");
	elektraKeySetName (k2, "");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	elektraKeySetName (k1, "user:/");
	elektraKeySetName (k2, "user:/");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	elektraKeySetName (k1, "system:/");
	elektraKeySetName (k2, "system:/");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	elektraKeySetName (k1, "user:/a");
	elektraKeySetName (k2, "user:/a");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "should be same");

	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

static void test_cmp (void)
{
	printf ("Compare two keys\n");

	ElektraKey * k1 = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKey * k2 = elektraKeyNew ("/", ELEKTRA_KEY_END);

	succeed_if (elektraKeyCmp (0, 0) == 0, "null keys comparision");

	elektraKeySetName (k1, "user:/a");
	elektraKeySetName (k2, "user:/a");
	succeed_if (elektraKeyCmp (k1, k1) == 0, "compare the same key");
	succeed_if (elektraKeyCmp (k1, k2) == 0, "compare the same key");
	succeed_if (elektraKeyCmp (k2, k1) == 0, "compare the same key");

	elektraKeySetName (k2, "user:/b");
	succeed_if (elektraKeyCmp (k1, k2) < 0, "compare key with different names");
	succeed_if (elektraKeyCmp (k2, k1) > 0, "compare key with different names");

	elektraKeyDel (k1);
	elektraKeyDel (k2);
}

int main (int argc, char ** argv)
{
	printf ("KEYSET ORDERING      TESTS\n");
	printf ("==========================\n\n");

	init (argc, argv);

	test_ksNew ();
	test_ksDuplicate ();

	test_ksHole ();
	test_append ();
	test_equal ();
	test_cmp ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", argv[0], nbTest, nbError);

	return nbError;
}
