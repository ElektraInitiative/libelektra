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
	KeySet * ks = 0;
	KeySet * keys = ksNew (15, KS_END);
	KeySet * config;

	printf ("Test ks creation\n");
	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");

	KeySet * ks2 = ksNew (0, KS_END);
	ksCopy (ks2, ks);
	succeed_if (ksGetSize (ks2) == 0, "size not correct after copy");

	ksClear (ks2); // useless, just test for double free
	ksCopy (ks2, ks);
	succeed_if (ksGetSize (ks2) == 0, "could not append 3 more keys");

	succeed_if (ksDel (ks) == 0, "could not delete keyset");


	succeed_if (ksGetSize (keys) == 0, "could not append 3 more keys");
	succeed_if (ksGetAlloc (keys) == 15, "allocation size wrong");
	succeed_if (ksDel (keys) == 0, "could not delete keyset");

	config = ksNew (100, keyNew ("user:/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key3", KEY_VALUE, "value3", KEY_END), KS_END);
	succeed_if (ksGetSize (config) == 3, "could not append 3 keys in keyNew");
	succeed_if (ksGetAlloc (config) == 100, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if (ksGetAlloc (config) == 49, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if (ksGetAlloc (config) == 24, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if (ksGetAlloc (config) == 15, "allocation size wrong");
	succeed_if (ksDel (config) == 0, "could not delete keyset");

	config = ksNew (17, keyNew ("user:/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key3", KEY_VALUE, "value1", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key4", KEY_VALUE, "value3", KEY_END), KS_END);

	succeed_if (ksGetSize (config) == 4, "could not append 5 keys in keyNew");
	succeed_if (ksGetAlloc (config) == 17, "allocation size wrong");
	ksAppendKey (config, keyNew ("user:/sw/app/fixedConfiguration/key6", KEY_VALUE, "value4", KEY_END));

	ksClear (ks2);
	ksCopy (ks2, config);
	compare_keyset (config, ks2);

	succeed_if (ksDel (config) == 0, "could not delete keyset");
	succeed_if (ksDel (ks2) == 0, "could not delete keyset");
}

static void test_ksDuplicate (void)
{
	printf ("Test bug duplicate\n");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, keyNew ("system:/duplicate", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (!strcmp (keyValue (ksLookupByName (ks, "system:/duplicate", 0)), "abc"), "wrong value for inserted key");

	succeed_if (ksAppendKey (ks, keyNew ("system:/duplicate", KEY_VALUE, "xyz", KEY_END)) == 1, "could not append duplicate key");
	succeed_if (!strcmp (keyValue (ksLookupByName (ks, "system:/duplicate", 0)), "xyz"), "wrong value for inserted key");

	ksDel (ks);
}

static void test_ksHole (void)
{
	printf ("Test holes in keysets\n");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, keyNew ("system:/sw/new", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (ksAppendKey (ks, keyNew ("system:/sw/new/sub", KEY_VALUE, "xyz", KEY_END)) == 2, "could not append key");
	succeed_if (ksAppendKey (ks, keyNew ("system:/sw/new/mis/sub", KEY_VALUE, "xyz", KEY_END)) == 3,
		    "could not append key which makes a hole");

	ksDel (ks);
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
static void per (int k, Key ** pool, Key ** result)
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
			 (size - selected - 1) * sizeof (struct Key *));
	}
	result[size - 1] = 0;
}

static void test_append (void)
{
	printf ("Test if keyset is sorted after appending\n");

	Key * key;
	Key * n;
	Key *s1, *s2, *s3;
	KeySet * ks;

	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key = keyNew ("system:/sw/new", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, n = keyNew ("system:/sw/new", KEY_VALUE, "xyz1", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->data->array[0] == n, "n not on position 0");
	succeed_if (ks->data->array[0] != key, "key is on position 0");
	succeed_if (ks->data->array[1] == 0, "array not null terminated");

	ksDel (ks);


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key = keyNew ("system:/sw/new", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, n = keyNew ("system:/sw/new/sub1", KEY_VALUE, "xyz1", KEY_END)) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == n, "new key not on position 1");
	succeed_if (ks->data->array[2] == 0, "array not null terminated");

	ksDel (ks);


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, n = keyNew ("system:/sw/new/sub1", KEY_VALUE, "xyz1", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->data->array[0] == n, "key not on position 0");
	succeed_if (ks->data->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, key = keyNew ("system:/sw/new", KEY_VALUE, "abc", KEY_END)) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == n, "key not on position 1");
	succeed_if (ks->data->array[2] == 0, "array not null terminated");

	ksDel (ks);


	key = keyNew ("system:/sw/new", KEY_VALUE, "abc", KEY_END);
	s1 = keyNew ("system:/sw/new/sub1", KEY_VALUE, "xyz1", KEY_END);
	s2 = keyNew ("system:/sw/new/sub2", KEY_VALUE, "xyz2", KEY_END);
	s3 = keyNew ("system:/sw/new/sub3", KEY_VALUE, "xyz3", KEY_END);
	keyIncRef (key);
	keyIncRef (s1);
	keyIncRef (s2);
	keyIncRef (s3);

	Key * solution[size] = { key, s1, s2, s3, 0 };
	Key * solutioncopy[size];
	Key * next[size];
	int i, j;

	for (i = 0; i < fac (size - 1); ++i)
	{
		memcpy (solutioncopy, solution, size * sizeof (struct Key *));
		per (i, solutioncopy, next);

		ks = ksNew (0, KS_END);

		for (j = 0; j < size - 1; ++j)
		{
			// printf ("%p ", (void*)next[j]);
			// printf ("%s ", keyName(next[j]));
			succeed_if (ksAppendKey (ks, next[j]) == j + 1, "could not append key");
		}
		// printf ("\nsolution is:\n");

		succeed_if (!memcmp (ks->data->array, solution, size), "solution is not correct");

		ksDel (ks);
	}


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s1) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == s1, "key not on position 1");
	succeed_if (ks->data->array[2] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s2) == 3, "could not append key");
	succeed_if (ksGetSize (ks) == 3, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == s1, "key not on position 1");
	succeed_if (ks->data->array[2] == s2, "key not on position 2");
	succeed_if (ks->data->array[3] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s3) == 4, "could not append key");
	succeed_if (ksGetSize (ks) == 4, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == s1, "key not on position 1");
	succeed_if (ks->data->array[2] == s2, "key not on position 2");
	succeed_if (ks->data->array[3] == s3, "key not on position 3");
	succeed_if (ks->data->array[4] == 0, "array not null terminated");

	succeed_if (!memcmp (ks->data->array, solution, size), "solution is not correct");

	ksDel (ks);


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, s3) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->data->array[0] == s3, "key not on position 0");
	succeed_if (ks->data->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, key) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == s3, "key not on position 1");
	succeed_if (ks->data->array[2] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s1) == 3, "could not append key");
	succeed_if (ksGetSize (ks) == 3, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == s1, "key not on position 1");
	succeed_if (ks->data->array[2] == s3, "key not on position 2");
	succeed_if (ks->data->array[3] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s2) == 4, "could not append key");
	succeed_if (ksGetSize (ks) == 4, "wrong size");
	succeed_if (ks->data->array[0] == key, "key not on position 0");
	succeed_if (ks->data->array[1] == s1, "key not on position 1");
	succeed_if (ks->data->array[2] == s2, "key not on position 2");
	succeed_if (ks->data->array[3] == s3, "key not on position 3");
	succeed_if (ks->data->array[4] == 0, "array not null terminated");

	succeed_if (!memcmp (ks->data->array, solution, size), "solution is not correct");

	ksDel (ks);
	keyDecRef (key);
	keyDel (key);
	keyDecRef (s1);
	keyDel (s1);
	keyDecRef (s2);
	keyDel (s2);
	keyDecRef (s3);
	keyDel (s3);
}

static void test_equal (void)
{
	Key * k1 = keyNew ("/", KEY_END);
	Key * k2 = keyNew ("/", KEY_END);

	succeed_if (keyCmp (0, 0) == 0, "null pointers should be same");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "");
	keySetName (k2, "");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user:/");
	keySetName (k2, "user:/");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "system:/");
	keySetName (k2, "system:/");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keyDel (k1);
	keyDel (k2);
}

static void test_cmp (void)
{
	printf ("Compare two keys\n");

	Key * k1 = keyNew ("/", KEY_END);
	Key * k2 = keyNew ("/", KEY_END);

	succeed_if (keyCmp (0, 0) == 0, "null keys comparision");

	keySetName (k1, "user:/a");
	keySetName (k2, "user:/a");
	succeed_if (keyCmp (k1, k1) == 0, "compare the same key");
	succeed_if (keyCmp (k1, k2) == 0, "compare the same key");
	succeed_if (keyCmp (k2, k1) == 0, "compare the same key");

	keySetName (k2, "user:/b");
	succeed_if (keyCmp (k1, k2) < 0, "compare key with different names");
	succeed_if (keyCmp (k2, k1) > 0, "compare key with different names");

	keyDel (k1);
	keyDel (k2);
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
