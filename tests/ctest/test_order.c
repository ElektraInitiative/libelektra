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

	succeed_if (ksAppendKey (ks, keyNew (0)) == -1, "could append a key with no name");
	succeed_if (ksAppendKey (ks, keyNew (0)) == -1, "could append a key with no name");
	succeed_if (ksAppendKey (ks, keyNew (0)) == -1, "could append a key with no name");
	succeed_if (ksGetSize (ks) == 0, "size not correct after 3 keys");

	KeySet * ks2 = ksNew (0, KS_END);
	ksCopy (ks2, ks);
	succeed_if (ksGetSize (ks2) == 0, "size not correct after 3 keys");

	succeed_if (ksAppendKey (ks, keyNew (0)) == -1, "could append a key with no name");
	succeed_if (ksAppendKey (ks, keyNew (0)) == -1, "could append a key with no name");
	succeed_if (ksAppendKey (ks, keyNew (0)) == -1, "could append a key with no name");
	succeed_if (ksGetSize (ks) == 0, "could not append 3 more keys");

	ksCopy (ks2, ks);
	succeed_if (ksGetSize (ks2) == 0, "could not append 3 more keys");

	ksClear (ks2); // useless, just test for double free
	ksCopy (ks2, ks);
	succeed_if (ksGetSize (ks2) == 0, "could not append 3 more keys");

	succeed_if (ksDel (ks) == 0, "could not delete keyset");


	succeed_if (ksGetSize (keys) == 0, "could not append 3 more keys");
	succeed_if (ksGetAlloc (keys) == 15, "allocation size wrong");
	succeed_if (ksDel (keys) == 0, "could not delete keyset");

	config = ksNew (100, keyNew ("user/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", 0),
			keyNew ("user/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", 0),
			keyNew ("user/sw/app/fixedConfiguration/key3", KEY_VALUE, "value3", 0), KS_END);
	succeed_if (ksGetSize (config) == 3, "could not append 3 keys in keyNew");
	succeed_if (ksGetAlloc (config) == 100, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if (ksGetAlloc (config) == 49, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if (ksGetAlloc (config) == 24, "allocation size wrong");
	keyDel (ksPop (config));
	succeed_if (ksGetAlloc (config) == 15, "allocation size wrong");
	succeed_if (ksDel (config) == 0, "could not delete keyset");

	config = ksNew (17, keyNew ("user/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", 0),
			keyNew ("user/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", 0),
			keyNew ("user/sw/app/fixedConfiguration/key3", KEY_VALUE, "value1", 0),
			keyNew ("user/sw/app/fixedConfiguration/key4", KEY_VALUE, "value3", 0), KS_END);

	succeed_if (ksGetSize (config) == 4, "could not append 5 keys in keyNew");
	succeed_if (ksGetAlloc (config) == 17, "allocation size wrong");
	ksAppendKey (config, keyNew ("user/sw/app/fixedConfiguration/key6", KEY_VALUE, "value4", 0));

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

	succeed_if (ksAppendKey (ks, keyNew ("system/duplicate", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (!strcmp (keyValue (ksLookupByName (ks, "system/duplicate", 0)), "abc"), "wrong value for inserted key");

	succeed_if (ksAppendKey (ks, keyNew ("system/duplicate", KEY_VALUE, "xyz", KEY_END)) == 1, "could not append duplicate key");
	succeed_if (!strcmp (keyValue (ksLookupByName (ks, "system/duplicate", 0)), "xyz"), "wrong value for inserted key");

	ksDel (ks);
}

static void test_ksHole (void)
{
	printf ("Test holes in keysets\n");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, keyNew ("system/sw/new", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (ksAppendKey (ks, keyNew ("system/sw/new/sub", KEY_VALUE, "xyz", KEY_END)) == 2, "could not append key");
	succeed_if (ksAppendKey (ks, keyNew ("system/sw/new/mis/sub", KEY_VALUE, "xyz", KEY_END)) == 3,
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
		memmove (pool + selected,       // destination
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

	succeed_if (ksAppendKey (ks, key = keyNew ("system/sw/new", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, n = keyNew ("system/sw/new", KEY_VALUE, "xyz1", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == n, "n not on position 0");
	succeed_if (ks->array[0] != key, "key is on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	ksDel (ks);


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key = keyNew ("system/sw/new", KEY_VALUE, "abc", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, n = keyNew ("system/sw/new/sub1", KEY_VALUE, "xyz1", KEY_END)) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == n, "new key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	ksDel (ks);


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, n = keyNew ("system/sw/new/sub1", KEY_VALUE, "xyz1", KEY_END)) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == n, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, key = keyNew ("system/sw/new", KEY_VALUE, "abc", KEY_END)) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == n, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	ksDel (ks);


	key = keyNew ("system/sw/new", KEY_VALUE, "abc", KEY_END);
	s1 = keyNew ("system/sw/new/sub1", KEY_VALUE, "xyz1", KEY_END);
	s2 = keyNew ("system/sw/new/sub2", KEY_VALUE, "xyz2", KEY_END);
	s3 = keyNew ("system/sw/new/sub3", KEY_VALUE, "xyz3", KEY_END);
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

		succeed_if (!memcmp (ks->array, solution, size), "solution is not correct");

		ksDel (ks);
	}


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s1) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s2) == 3, "could not append key");
	succeed_if (ksGetSize (ks) == 3, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s3) == 4, "could not append key");
	succeed_if (ksGetSize (ks) == 4, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == s3, "key not on position 3");
	succeed_if (ks->array[4] == 0, "array not null terminated");

	succeed_if (!memcmp (ks->array, solution, size), "solution is not correct");

	ksDel (ks);


	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, s3) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == s3, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, key) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s3, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s1) == 3, "could not append key");
	succeed_if (ksGetSize (ks) == 3, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s3, "key not on position 2");
	succeed_if (ks->array[3] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s2) == 4, "could not append key");
	succeed_if (ksGetSize (ks) == 4, "wrong size");
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
	Key * k1 = keyNew (0);
	Key * k2 = keyNew (0);

	succeed_if (keyCmp (0, 0) == 0, "null pointers should be same");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "");
	keySetName (k2, "");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user");
	keySetName (k2, "user");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "system");
	keySetName (k2, "system");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keySetName (k1, "user/a");
	keySetName (k2, "user/a");
	succeed_if (keyCmp (k1, k2) == 0, "should be same");

	keyDel (k1);
	keyDel (k2);
}

static void test_cmp (void)
{
	printf ("Compare two keys\n");

	Key * k1 = keyNew (0);
	Key * k2 = keyNew (0);

	succeed_if (keyCmp (0, 0) == 0, "null keys comparision");
	succeed_if (keyCmp (k1, 0) == 1, "compare null key with key with no name");
	succeed_if (keyCmp (0, k2) == -1, "compare null key with key with no name");

	keySetName (k1, "user/a");
	succeed_if (keyCmp (k2, k2) == 0, "null keys comparision");
	succeed_if (keyCmp (k1, k2) == 1, "compare key with no name with user/a");
	succeed_if (keyCmp (k2, k1) == -1, "compare key with no name with user/a");

	keySetName (k2, "user/a");
	succeed_if (keyCmp (k1, k1) == 0, "compare the same key");
	succeed_if (keyCmp (k1, k2) == 0, "compare the same key");
	succeed_if (keyCmp (k2, k1) == 0, "compare the same key");

	keySetOwner (k1, "non_existing_user");
	succeed_if (keyCmp (k2, k2) == 0, "null owner comparision");
	succeed_if (keyCmp (k1, k2) > 0, "compare key with no owner with non_existing_user");
	succeed_if (keyCmp (k2, k1) < 0, "compare key with no owner with non_existing_user");

	keySetOwner (k2, "other_non_existing_user");
	succeed_if (keyCmp (k1, k2) < 0, "compare key with owner non_existing_user with other_non_existing_user");
	succeed_if (keyCmp (k2, k1) > 0, "compare key with owner non_existing_user with other_non_existing_user");

	keySetName (k2, "user/b");
	succeed_if (keyCmp (k1, k2) < 0, "compare key with different names");
	succeed_if (keyCmp (k2, k1) > 0, "compare key with different names");

	keyDel (k1);
	keyDel (k2);
}

static void test_appendowner (void)
{
	Key *key, *s1, *s2, *s3;
	KeySet * ks;

	printf ("Append Keys with owner");

	key = keyNew ("system/sw/new", KEY_VALUE, "abc", KEY_END);
	s1 = keyNew ("system/sw/new", KEY_VALUE, "xyz1", KEY_OWNER, "s1", KEY_END);
	s2 = keyNew ("system/sw/new", KEY_VALUE, "xyz2", KEY_OWNER, "s2", KEY_END);
	s3 = keyNew ("system/sw/new", KEY_VALUE, "xyz3", KEY_OWNER, "s3", KEY_END);
	ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key) == 1, "could not append key");
	succeed_if (ksGetSize (ks) == 1, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s1) == 2, "could not append key");
	succeed_if (ksGetSize (ks) == 2, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s2) == 3, "could not append key");
	succeed_if (ksGetSize (ks) == 3, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == 0, "array not null terminated");

	succeed_if (ksAppendKey (ks, s3) == 4, "could not append key");
	succeed_if (ksGetSize (ks) == 4, "wrong size");
	succeed_if (ks->array[0] == key, "key not on position 0");
	succeed_if (ks->array[1] == s1, "key not on position 1");
	succeed_if (ks->array[2] == s2, "key not on position 2");
	succeed_if (ks->array[3] == s3, "key not on position 3");
	succeed_if (ks->array[4] == 0, "array not null terminated");

	ksDel (ks);
}

static void test_ksLookupCase (void)
{
	printf ("Test bug lookup with case\n");
	KeySet * ks = ksNew (32, keyNew ("system/ay/key", KEY_VALUE, "aykey", KEY_END),
			     keyNew ("system/mY/kex", KEY_VALUE, "mykex", KEY_END), keyNew ("system/xy/key", KEY_VALUE, "xykey", KEY_END),
			     keyNew ("system/My/key", KEY_VALUE, "Mykey", KEY_END), KS_END);
	// Does not work without KDB_O_NOALL
	Key * found = ksLookupByName (ks, "system/my/key", KDB_O_NOCASE | KDB_O_NOALL);
	succeed_if (found != 0, "could not find key (binary search fails when ignoring case)");
	ksDel (ks);
}

static void test_ksLookupOwner (void)
{
	printf ("Test bug lookup with owner\n");
	Key * found = 0;
	KeySet * ks = ksNew (32, keyNew ("user:fritz/my/key", KEY_VALUE, "fritz", KEY_END),
			     keyNew ("user:frotz/my/key", KEY_VALUE, "frotz", KEY_END),
			     keyNew ("user/my/key", KEY_VALUE, "current", KEY_END), KS_END);

	found = ksLookupByName (ks, "user/my/key", KDB_O_WITHOWNER);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "current"), "got wrong key");

	found = ksLookupByName (ks, "user:fritz/my/key", KDB_O_WITHOWNER);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "fritz"), "got wrong key");

	found = ksLookupByName (ks, "user:frotz/my/key", KDB_O_WITHOWNER);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "frotz"), "got wrong key");

	found = ksLookupByName (ks, "user:fretz/my/key", KDB_O_WITHOWNER);
	succeed_if (found == 0, "found non existing key");

	found = ksLookupByName (ks, "user/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "fritz"), "binary search seems to be non-deterministic");

	found = ksLookupByName (ks, "user:fritz/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "fritz"), "binary search seems to be non-deterministic");

	found = ksLookupByName (ks, "user:frotz/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "fritz"), "binary search seems to be non-deterministic");

	found = ksLookupByName (ks, "user:fretz/my/key", 0);
	succeed_if (found != 0, "could not find key");
	succeed_if (!strcmp (keyValue (found), "fritz"), "binary search seems to be non-deterministic");

	ksDel (ks);
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
	test_appendowner ();
	test_ksLookupCase ();
	test_ksLookupOwner ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", argv[0], nbTest, nbError);

	return nbError;
}
