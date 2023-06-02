/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <internal/core/keyset/api.h>
#include <tests.h>

#define NUMBER_OF_NAMESPACES 5

char * namespaces[] = { "spec:/", "proc:/", "dir:/", "user:/", "system:/", 0 };

static void test_ksNew (void)
{
	KeySet * ks = 0;
	KeySet * keys = ksNew (15, KS_END);
	KeySet * config;

	printf ("Test ks creation\n");
	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");

	succeed_if (ksAppendKey (ks, keyNew ("user:/a", KEY_END)) == 1, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/b", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/c", KEY_END)) == 3, "could not append a key");
	succeed_if (ksGetSize (ks) == 3, "size not correct after 3 keys");

	KeySet * ks2 = ksNew (0, KS_END);
	ksCopy (ks2, ks);
	compare_keyset (ks, ks2);

	succeed_if (ksAppendKey (ks, keyNew ("user:/d", KEY_END)) == 4, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/e", KEY_END)) == 5, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/f", KEY_END)) == 6, "could not append a key");
	succeed_if (ksGetSize (ks) == 6, "could not append 3 more keys");

	ksCopy (ks2, ks);
	compare_keyset (ks, ks2);

	ksClear (ks2); // useless, just test for double free
	ksCopy (ks2, ks);
	compare_keyset (ks, ks2);

	succeed_if (ksDel (ks) == 0, "could not delete keyset");


	succeed_if (ksGetSize (keys) == 0, "could not append 3 more keys");
	// succeed_if(ksGetAlloc(keys) == 15, "allocation size wrong");
	succeed_if (ksDel (keys) == 0, "could not delete keyset");

	config = ksNew (100, keyNew ("user:/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key3", KEY_VALUE, "value3", KEY_END), KS_END);
	succeed_if (ksGetSize (config) == 3, "could not append 3 keys in keyNew");
	// this behaviour might change, do not build on it,
	// and there is no compatible way to get the alloc info
	// succeed_if(ksGetAlloc(config) == 100, "allocation size wrong");
	keyDel (ksPop (config));
	// succeed_if(ksGetAlloc(config) == 49, "allocation size wrong");
	keyDel (ksPop (config));
	// succeed_if(ksGetAlloc(config) == 24, "allocation size wrong");
	keyDel (ksPop (config));
	// succeed_if(ksGetAlloc(config) == 15, "allocation size wrong");
	succeed_if (ksDel (config) == 0, "could not delete keyset");

	config = ksNew (10, keyNew ("user:/sw/app/fixedConfiguration/key1", KEY_VALUE, "value1", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key2", KEY_VALUE, "value2", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key3", KEY_VALUE, "value1", KEY_END),
			keyNew ("user:/sw/app/fixedConfiguration/key4", KEY_VALUE, "value3", KEY_END), KS_END);

	succeed_if (ksGetSize (config) == 4, "could not append 5 keys in keyNew");
	// succeed_if(ksGetAlloc(config) == 15, "allocation size wrong");
	ksAppendKey (config, keyNew ("user:/sw/app/fixedConfiguration/key6", KEY_VALUE, "value4", KEY_END));

	ksClear (ks2);
	ksCopy (ks2, config);
	compare_keyset (config, ks2);

	succeed_if (ksDel (config) == 0, "could not delete keyset");
	succeed_if (ksDel (ks2) == 0, "could not delete keyset");

	KeySet * ks_c = ksNew (5, keyNew ("user:/valid/key1", KEY_END), keyNew ("user:/valid/key2", KEY_END),
			       keyNew ("system:/valid/key1", KEY_END), keyNew ("system:/valid/key2", KEY_END), KS_END);

	succeed_if (ksCurrent (ks_c) == 0, "should be rewinded");
	ksDel (ks_c);

	succeed_if (ksDel (0) == -1, "No error on NULL pointer");
}

static void test_ksEmpty (void)
{
	printf ("Test empty keysets\n");
	KeySet * ks;
	KeySet * ks2;
	Key * current;

	ks = ksNew (0, KS_END);
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	succeed_if (ksPop (ks) == 0, "pop empty keyset");
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	ksDel (ks);

	ks = ksNew (1, current = keyNew ("user:/test", KEY_END), KS_END);
	succeed_if (ksGetSize (ks) == 1, "size not correct");
	succeed_if (ksPop (ks) == current, "pop empty keyset");
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	succeed_if (ksPop (ks) == 0, "pop empty keyset");
	succeed_if (ksGetSize (ks) == 0, "size not correct");
	keyDel (current);
	ksDel (ks);

	ks = ksNew (0, KS_END);
	ks2 = ksNew (0, KS_END);
	succeed_if (ksAppend (ks, ks2) == 0, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksPop (ks) == 0, "could not pop empty keyset");
	succeed_if (ksPop (ks2) == 0, "could not pop empty keyset2");
	ksDel (ks);
	ksDel (ks2);

	ks = ksNew (1, current = keyNew ("user:/test", KEY_END), KS_END);
	ks2 = ksNew (0, KS_END);
	succeed_if (ksGetSize (ks) == 1, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 1, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 1, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksPop (ks) == current, "could not pop keyset");
	succeed_if (ksPop (ks2) == 0, "could not pop empty keyset2");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 0, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	keyDel (current);
	ksDel (ks);
	ksDel (ks2);


	ks = ksNew (0, KS_END);
	ks2 = ksNew (1, current = keyNew ("user:/test", KEY_END), KS_END);
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 1, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 1, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 1, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 1, "empty keyset does not have correct size");
	succeed_if (ksPop (ks) == current, "could not pop keyset");
	succeed_if (ksPop (ks2) == current, "could not pop empty keyset2");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	succeed_if (ksAppend (ks, ks2) == 0, "could not append empty keyset");
	succeed_if (ksGetSize (ks) == 0, "empty keyset does not have correct size");
	succeed_if (ksGetSize (ks2) == 0, "empty keyset does not have correct size");
	keyDel (current); // only one keyDel, because ksPop decrements counter
	ksDel (ks);
	ksDel (ks2);
}

#define NR_KEYSETS 10

static void test_ksReference (void)
{
	KeySet * ks = 0;
	KeySet * ks1;
	Key *k1, *k2;
	KeySet * kss[NR_KEYSETS];
	int i;

	printf ("Test reference of key\n");

	ks = ksNew (0, KS_END);
	k1 = keyNew ("user:/aname", KEY_END);

	succeed_if (ksAtCursor (0, 0) == 0, "Not NULL on NULL KeySet");
	succeed_if (ksAtCursor (0, ksGetSize (0) - 1) == 0, "Not NULL on NULL KeySet");

	succeed_if (ksAtCursor (ks, 0) == 0, "Not NULL on empty KeySet");
	succeed_if (ksAtCursor (ks, ksGetSize (ks) - 1) == 0, "Not NULL on empty KeySet");

	succeed_if (keyGetRef (k1) == 0, "reference counter of new key");
	succeed_if (ksAppendKey (ks, k1) == 1, "size should be one");
	succeed_if (keyGetRef (k1) == 1, "reference counter of inserted key");
	succeed_if (ksGetSize (ks) == 1, "wrong size, should stay after inserting duplication");
	succeed_if (ksAtCursor (ks, 0) == k1, "head wrong");
	succeed_if (ksAtCursor (ks, ksGetSize (ks) - 1) == k1, "tail wrong");

	k2 = keyDup (k1, KEY_CP_ALL);
	keySetString (k2, "newvalue");

	succeed_if (keyGetRef (k2) == 0, "reference counter not resetted");
	succeed_if (ksAppendKey (ks, k2) == 1, "size should stay at 1");
	// k1 should be freed by now and instead k2 in the keyset
	succeed_if (ksGetSize (ks) == 1, "wrong size, should stay after inserting duplication");

	succeed_if_same_string (keyValue (ksAtCursor (ks, 0)), "newvalue");

	ksDel (ks);

	ks = ksNew (5, keyNew ("user:/key", KEY_END), keyNew ("system:/key", KEY_END), KS_END);

	k1 = ksLookupByName (ks, "system:/key", 0);
	k2 = ksLookupByName (ks, "user:/key", 0);
	succeed_if (keyGetRef (k1) == 1, "reference counter of new inserted key");
	succeed_if (keyGetRef (k2) == 1, "reference counter of new inserted key");
	succeed_if (ksAtCursor (ks, 0) == k2, "head wrong");
	succeed_if (ksAtCursor (ks, ksGetSize (ks) - 1) == k1, "tail wrong");

	ksDel (ks);

	ks = ksNew (5, keyNew ("user:/key", KEY_END), keyNew ("system:/key", KEY_END), KS_END);

	k1 = ksLookupByName (ks, "system:/key", 0);
	k2 = ksLookupByName (ks, "user:/key", 0);
	succeed_if (keyGetRef (k1) == 1, "reference counter of new inserted key");
	succeed_if (keyGetRef (k2) == 1, "reference counter of new inserted key");
	ks1 = ksDup (ks);
	succeed_if (ksAtCursor (ks1, 0) == k2, "head in dup wrong");
	succeed_if (ksAtCursor (ks1, ksGetSize (ks1) - 1) == k1, "tail in dup wrong");

	// COW - key references stay the same
	succeed_if (keyGetRef (k1) == 1, "reference counter after duplication of keyset");
	succeed_if (keyGetRef (k2) == 1, "reference counter after ksdup");

	k1 = ksPop (ks);
	succeed_if (keyGetRef (k1) == 1, "reference counter after pop");
	keyDel (k1);
	succeed_if (keyGetRef (k1) == 1, "reference counter");
	succeed_if (keyGetRef (k2) == 2, "reference counter should not be influenced");

	ksDel (ks);
	succeed_if (keyGetRef (k1) == 1, "reference counter, delete from first keyset");
	succeed_if (keyGetRef (k2) == 1, "reference counter, delete from first keyset");
	ksDel (ks1); // k1 and k2 deleted

	ks1 = ksNew (0, KS_END);
	k1 = keyNew ("user:/k1", KEY_END);
	succeed_if (keyGetRef (k1) == 0, "reference counter of new inserted key");
	succeed_if (ksAppendKey (ks1, k1) == 1, "appending did not work");
	succeed_if (ksGetSize (ks1) == 1, "size did not match");
	succeed_if (keyGetRef (k1) == 1, "reference counter of new inserted key");
	succeed_if (ksAppendKey (ks1, k1) == 1, "appending the very same key");
	succeed_if (ksGetSize (ks1) == 1, "size did not match");
	succeed_if (keyGetRef (k1) == 1, "reference counter of new inserted key should stay the same");

	k1 = ksPop (ks1);
	succeed_if (keyGetRef (k1) == 0, "reference counter of new inserted key");
	succeed_if (keyDel (k1) == 0, "keyDel did not work");

	succeed_if (ksDel (ks1) == 0, "could not delete key");


	kss[0] = ksNew (5, k1 = keyNew ("user:/key", KEY_END), k2 = keyNew ("system:/key", KEY_END), KS_END);
	for (i = 1; i < NR_KEYSETS; i++)
	{
		succeed_if (keyGetRef (k1) == i, "reference counter");
		succeed_if (keyGetRef (k2) == 1, "reference counter");
		kss[i] = ksDup (kss[i - 1]);
		// COW - key references stay the same after a ksDup
		succeed_if (keyGetRef (k2) == 1, "reference counter");
		succeed_if_same_string (keyName (ksPop (kss[i - 1])), "system:/key");
		succeed_if (keyGetRef (k2) == 1, "reference counter");
		succeed_if (keyDel (k2) == 1, "delete key");
		succeed_if (keyGetRef (k2) == 1, "reference counter");
	}
	succeed_if (keyGetRef (k1) == NR_KEYSETS, "reference counter");
	succeed_if (keyGetRef (k2) == 1, "reference counter");

	for (i = 0; i < NR_KEYSETS; i++)
	{
		succeed_if (keyGetRef (k1) == NR_KEYSETS - i, "reference counter");
		ksDel (kss[i]);
	}
}

static void test_ksDup (void)
{
	KeySet * ks = 0;
	KeySet * other = 0;

	printf ("Test ks duplication\n");

	succeed_if (ksDup (0) == 0, "No error on NULL pointer");

	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");
	other = ksDup (ks);
	succeed_if (other, "other creation failed");
	succeed_if (ksGetSize (ks) == 0, "ks has keys");
	succeed_if (ksGetSize (other) == 0, "other has keys");
	ksDel (other);
	ksDel (ks);

	exit_if_fail ((ks = ksNew (1, keyNew ("user:/anything", KEY_END), KS_END)) != 0, "could not create new keyset");
	other = ksDup (ks);
	succeed_if (other, "other creation failed");
	succeed_if (ksGetSize (ks) == 1, "ks has no keys");
	succeed_if (ksGetSize (other) == 1, "other has no keys");
	ksDel (other);
	ksDel (ks);

	exit_if_fail ((ks = ksNew (1, keyNew ("system:/some", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	other = ksDup (ks);
	succeed_if (other, "other creation failed");
	succeed_if (ksGetSize (ks) == 4, "ks has no keys");
	succeed_if (ksGetSize (other) == 4, "other has no keys");
	ksDel (other);
	ksDel (ks);

	exit_if_fail ((ks = ksNew (1, keyNew ("user:/any123", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	other = ksDup (ks);
	succeed_if (other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if (ksGetSize (ks) == 4, "ks has no keys");
	succeed_if (ksGetSize (other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);

	exit_if_fail ((ks = ksNew (1, keyNew ("system:/test", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	other = ksDup (ks);
	succeed_if (other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if (ksAppendKey (ks, keyNew ("user:/test4", KEY_END)) == 5, "could not append a key");
	succeed_if (ksGetSize (ks) == 5, "ks has no keys");
	succeed_if (ksGetSize (other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);
}

static void test_ksCopy (void)
{
	KeySet * ks = 0;
	KeySet * other = 0;

	printf ("Test ks copy\n");

	exit_if_fail ((ks = ksNew (1, keyNew ("user:/testro", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCopy (0, ks) == -1, "No error on NULL pointer");
	succeed_if (ksCopy (ks, 0) == 0, "Could not delete ks with ksCopy");
	succeed_if (ksGetSize (ks) == 0, "ks has keys after deleting with ksCopy");
	ksDel (ks);

	other = ksNew (0, KS_END);
	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");
	succeed_if (ksCopy (other, ks) == 1, "Copy failed");
	succeed_if (other, "other creation failed");
	succeed_if (ksGetSize (ks) == 0, "ks has keys");
	succeed_if (ksGetSize (other) == 0, "other has keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew (0, KS_END);
	exit_if_fail ((ks = ksNew (1, keyNew ("user:/test3", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksCopy (other, ks) == 1, "Copy failed");
	succeed_if (other, "other creation failed");
	succeed_if (ksGetSize (ks) == 1, "ks has no keys");
	succeed_if (ksGetSize (other) == 1, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew (0, KS_END);
	exit_if_fail ((ks = ksNew (1, keyNew ("user:/testro", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCopy (other, ks) == 1, "Copy failed");
	succeed_if (other, "other creation failed");
	succeed_if (ksGetSize (ks) == 4, "ks has no keys");
	succeed_if (ksGetSize (other) == 4, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew (0, KS_END);
	exit_if_fail ((ks = ksNew (1, keyNew ("system:/test", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCopy (other, ks) == 1, "Copy failed");
	succeed_if (other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if (ksGetSize (ks) == 4, "ks has no keys");
	succeed_if (ksGetSize (other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew (0, KS_END);
	exit_if_fail ((ks = ksNew (1, keyNew ("user:/mykeys", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test1", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test2", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/test3", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCopy (other, ks) == 1, "Copy failed");
	succeed_if (other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if (ksAppendKey (ks, keyNew ("user:/test", KEY_END)) == 5, "could not append a key");
	succeed_if (ksGetSize (ks) == 5, "ks has no keys");
	succeed_if (ksGetSize (other) == 3, "other has no keys");
	ksDel (other);
	ksDel (ks);

	other = ksNew (0, KS_END);
	exit_if_fail ((ks = ksNew (1, keyNew ("user:/a/b/c", KEY_END), KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, keyNew ("user:/a/test", KEY_END)) == 2, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/a/b/test", KEY_END)) == 3, "could not append a key");
	succeed_if (ksAppendKey (ks, keyNew ("user:/a/b/ctest", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCopy (other, ks) == 1, "Copy failed");
	succeed_if (other, "other creation failed");
	keyDel (ksPop (other));
	succeed_if (ksAppendKey (ks, keyNew ("user:/test", KEY_END)) == 5, "could not append a key");
	succeed_if (ksGetSize (ks) == 5, "ks has no keys");
	succeed_if (ksGetSize (other) == 3, "other has no keys");

	succeed_if (ksCopy (ks, 0) == 0, "Clear failed");
	succeed_if (ksGetSize (ks) == 0, "ks has keys");

	succeed_if (ksCopy (other, 0) == 0, "Clear failed");
	succeed_if (ksGetSize (other) == 0, "other has keys");
	ksDel (other);
	ksDel (ks);


	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/abc", KEY_META, "def", "egh", KEY_END));

	other = ksNew (0, KS_END);
	ksCopy (other, ks);
	compare_keyset (ks, other);

	ksDel (other);
	ksDel (ks);
}

static void test_ksIterate (void)
{
	KeySet * ks = ksNew (0, KS_END);
	KeySet * other = ksNew (0, KS_END);
	Key * key;
	int i;
	char name[] = "user:/n";

	printf ("Test keyset iterate\n");
	succeed_if (ksNext (0) == 0, "No NULL pointer on NULL pointer keyset");
	succeed_if (ksCurrent (0) == 0, "No NULL pointer on NULL pointer keyset");
	succeed_if (ksRewind (0) == -1, "No error on NULL pointer");

	succeed_if (ksCurrent (ks) == 0, "No NULL pointer on empty keyset");
	succeed_if (ksNext (ks) == 0, "No NULL pointer on empty keyset");
	succeed_if (ksRewind (ks) == 0, "Cannot rewind empty keyset");

	ksAppendKey (ks, keyNew ("user:/1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/3", KEY_END));
	ksAppendKey (ks, keyNew ("user:/4", KEY_END));
	ksAppendKey (ks, keyNew ("user:/5", KEY_END));
	succeed_if (ksGetSize (ks) == 5, "could not append 5 keys");

	succeed_if (ksRewind (ks) == 0, "Could not rewind keyset");
	succeed_if (ksRewind (ks) == 0, "Could not rewind keyset twice");

	succeed_if (ksGetCursor (ks) == -1, "Internal cursor after rewinding is set");

	succeed_if (ksNext (ks) != 0, "Could not get first key");
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/1");

	succeed_if (ksNext (ks) != 0, "Could not get second key");
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/2");

	succeed_if (ksNext (ks) != 0, "Could not get third key");
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/3");

	succeed_if (ksNext (ks) != 0, "Could not get fourth key");
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/4");

	succeed_if (ksNext (ks) != 0, "Could not get fifth key");
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/5");

	succeed_if (ksNext (ks) == 0, "Could not iterate over last");
	succeed_if (ksCurrent (ks) == 0, "This is not the beyond last key");

	succeed_if (ksNext (ks) == 0, "Could not iterate over last (again)");
	succeed_if (ksCurrent (ks) == 0, "This is not the beyond last key (again)");

	key = ksPop (ks);
	succeed_if_same_string (keyName (key), "user:/5");
	succeed_if (keyDel (key) == 0, "could not del popped key");

	succeed_if (ksAppend (other, ks) == 4, "could not append keys");

	for (i = 4; i >= 1; i--)
	{
		key = ksPop (other);
		succeed_if (key != 0, "got null pointer key");
		name[6] = '0' + i;
		succeed_if_same_string (keyName (key), name);
		keyDel (key);
	}

	succeed_if (ksAppendKey (other, keyNew ("user:/3", KEY_END)) == 1, "could not append one key");
	key = ksPop (other);
	succeed_if (key != 0, "got null pointer key");
	succeed_if_same_string (keyName (key), "user:/3");
	succeed_if (keyDel (key) == 0, "could not del popped key");
	ksDel (other);
	ksDel (ks);

	ks = ksNew (10, keyNew ("user:/0", KEY_END), keyNew ("user:/1", KEY_END), keyNew ("user:/2", KEY_END), keyNew ("user:/3", KEY_END),
		    KS_END);

	other = ksNew (10, keyNew ("user:/4", KEY_END), keyNew ("user:/5", KEY_END), keyNew ("user:/6", KEY_END),
		       keyNew ("user:/7", KEY_END), KS_END);

	succeed_if (ksAppend (ks, other) == 8, "could not append keys");

	for (i = 7; i >= 0; i--)
	{
		key = ksPop (ks);
		succeed_if (key != 0, "got null pointer key");
		name[6] = '0' + i;
		succeed_if_same_string (keyName (key), name);
		keyDel (key);
	}
	ksDel (ks);
	ksDel (other);
}

static void test_ksCursor (void)
{
	KeySet * ks = ksNew (0, KS_END);
	Key * key;
	elektraCursor cursor;
	Key * cur;
	int i;
	char name[] = "user:/n";

	printf ("Test keyset cursor\n");

	ksAppendKey (ks, cur = keyNew ("user:/1", KEY_END));
	succeed_if (ksCurrent (ks) == cur, "cursor not set after append key");
	ksAppendKey (ks, cur = keyNew ("user:/2", KEY_END));
	succeed_if (ksCurrent (ks) == cur, "cursor not set after append key");
	ksAppendKey (ks, cur = keyNew ("user:/3", KEY_END));
	succeed_if (ksCurrent (ks) == cur, "cursor not set after append key");
	cursor = ksGetCursor (ks);
	succeed_if_same_string (keyName (ksAtCursor (ks, cursor)), "user:/3");
	ksAppendKey (ks, cur = keyNew ("user:/4", KEY_END));
	succeed_if (ksCurrent (ks) == cur, "cursor not set after append key");
	ksAppendKey (ks, cur = keyNew ("user:/5", KEY_END));
	succeed_if (ksCurrent (ks) == cur, "cursor not set after append key");
	succeed_if (ksGetSize (ks) == 5, "could not append 5 keys");

	succeed_if_same_string (keyName (ksAtCursor (ks, cursor)), "user:/3");
	ksSetCursor (ks, cursor);
	succeed_if (cursor == ksGetCursor (ks), "cursor not set to 3");
	succeed_if_same_string (keyName (ksAtCursor (ks, cursor)), "user:/3");
	ksSetCursor (ks, cursor);
	succeed_if (cursor == ksGetCursor (ks), "cursor not set to 3 (again)");

	cursor = ksGetCursor (ks);
	key = ksPop (ks);
	succeed_if (cursor == ksGetCursor (ks), "cursor should stay the same");
	succeed_if_same_string (keyName (key), "user:/5");
	succeed_if (keyDel (key) == 0, "could not del popped key");

	ksRewind (ks);
	for (i = 0; i < 5; i++)
	{
		ksNext (ks);
		if (i == 1)
		{
			cursor = ksGetCursor (ks);
			name[6] = '0' + i;
		}
	}
	ksSetCursor (ks, cursor);
	ksCurrent (ks);

	ksDel (ks);

	ks = ksNew (10, keyNew ("user:/0", KEY_END), keyNew ("user:/1", KEY_END), keyNew ("user:/2", KEY_END), keyNew ("user:/3", KEY_END),
		    KS_END);

	ksRewind (ks);
	for (i = 0; i < 4; i++)
	{
		ksNext (ks);
		if (i == 1)
		{
			cursor = ksGetCursor (ks);
			name[6] = '0' + i;
		}
	}

	ksSetCursor (ks, cursor);
	key = ksCurrent (ks);
	succeed_if_same_string (keyName (key), name);

	ksDel (ks);

	ks = ksNew (10, keyNew ("user:/0", KEY_END), keyNew ("user:/1", KEY_END), keyNew ("user:/2", KEY_END), keyNew ("user:/3", KEY_END),
		    KS_END);

	ksRewind (ks);
	for (i = 0; i < 4; i++)
	{
		ksNext (ks);
		cursor = ksGetCursor (ks);
		name[6] = '0' + i;
		succeed_if_same_string (keyName (ksAtCursor (ks, cursor)), name);
	}

	succeed_if_same_string (keyName (ksAtCursor (ks, 0)), "user:/0");
	succeed_if_same_string (keyName (ksAtCursor (ks, 1)), "user:/1");
	succeed_if_same_string (keyName (ksAtCursor (ks, 2)), "user:/2");
	succeed_if_same_string (keyName (ksAtCursor (ks, 3)), "user:/3");
	succeed_if (ksAtCursor (ks, -1) == 0, "bounds check not correct");
	succeed_if (ksAtCursor (ks, 4) == 0, "bounds check not correct");

	ksDel (ks);
}

static void test_ksAtCursor (void)
{
	KeySet * ks;
	Key * current;
	Key * testKeys[5];
	ks = ksNew (0, KS_END);

	testKeys[0] = keyNew ("user:/test1", KEY_END);
	testKeys[1] = keyNew ("user:/test2", KEY_END);
	testKeys[2] = keyNew ("user:/test3", KEY_END);
	testKeys[3] = keyNew ("user:/test4", KEY_END);
	testKeys[4] = keyNew ("user:/test5", KEY_END);

	for (size_t index = 0; index < 5; index++)
	{
		ksAppendKey (ks, testKeys[index]);
	}

	ksRewind (ks);

	elektraCursor cursor;

	/* test whether the correct key is returned */
	for (size_t index = 0; index < 5; index++)
	{
		current = testKeys[index];
		ksNext (ks);
		cursor = ksGetCursor (ks);
		Key * other = ksAtCursor (ks, cursor);
		succeed_if_same_string (keyName (current), keyName (other));
	}

	succeed_if (ksAtCursor (ks, 5) == 0, "Not NULL on invalid cursor position");

	/* test whether the correct key is returned even if
	 * the internal cursor is positioned somewhere else */
	ksRewind (ks);
	ksNext (ks);
	cursor = ksGetCursor (ks);
	ksNext (ks);
	ksNext (ks);
	current = ksAtCursor (ks, cursor);
	succeed_if_same_string (keyName (current), "user:/test1");

	/* test whether the internal cursor is modified */
	ksRewind (ks);
	ksNext (ks);
	cursor = ksGetCursor (ks);
	ksNext (ks);
	current = ksAtCursor (ks, cursor);
	succeed_if_same_string (keyName (current), "user:/test1");
	current = ksNext (ks);
	succeed_if_same_string (keyName (current), "user:/test3");

	/* test postconditions */
	succeed_if (!ksAtCursor (0, cursor), "did not return NULL on NULL keyset");
	succeed_if (!ksAtCursor (ks, -1), "did not return NULL on negative cursor");
	succeed_if (!ksAtCursor (ks, 10), "did not return NULL on invalid cursor");

	ksDel (ks);
}

static void test_ksSort (void)
{
	KeySet * ks;
	Key *key, *k1, *k2;
	int i;

	printf ("Test ks sort\n");

	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/bname", KEY_END));
	ksAppendKey (ks, keyNew ("user:/aname", KEY_END));
	ksAppendKey (ks, keyNew ("user:/cname", KEY_END));

	ksRewind (ks);
	key = ksNext (ks);
	succeed_if_same_string (keyName (key), "user:/aname");

	key = ksNext (ks);
	succeed_if_same_string (keyName (key), "user:/bname");

	key = ksNext (ks);
	succeed_if_same_string (keyName (key), "user:/cname");
	ksDel (ks);

	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/e", KEY_END));
	ksAppendKey (ks, keyNew ("user:/b1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/h2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/b2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/d", KEY_END));
	ksAppendKey (ks, keyNew ("user:/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/g", KEY_END));
	ksAppendKey (ks, keyNew ("user:/g", KEY_END));
	ksAppendKey (ks, keyNew ("user:/c2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/c1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/g", KEY_END));
	ksAppendKey (ks, keyNew ("user:/h1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/f", KEY_END));

	ksRewind (ks);
	for (i = 0; (key = ksNext (ks)) != 0; i++)
	{
		switch (i)
		{
		case 0:
			succeed_if_same_string (keyName (key), "user:/a");
			break;
		case 1:
			succeed_if_same_string (keyName (key), "user:/b1");
			break;
		case 2:
			succeed_if_same_string (keyName (key), "user:/b2");
			break;
		case 3:
			succeed_if_same_string (keyName (key), "user:/c1");
			break;
		case 4:
			succeed_if_same_string (keyName (key), "user:/c2");
			break;
		case 5:
			succeed_if_same_string (keyName (key), "user:/d");
			break;
		case 6:
			succeed_if_same_string (keyName (key), "user:/e");
			break;
		case 7:
			succeed_if_same_string (keyName (key), "user:/f");
			break;
		case 8:
			succeed_if_same_string (keyName (key), "user:/g");
			break;
		case 9:
			succeed_if_same_string (keyName (key), "user:/h1");
			break;
		case 10:
			succeed_if_same_string (keyName (key), "user:/h2");
			break;
		default:
			succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);

	ks = ksNew (0, KS_END);
	k1 = keyNew ("user:/xname", KEY_END);
	ksAppendKey (ks, k1);

	k2 = keyDup (k1, KEY_CP_ALL);

	succeed_if (keyGetRef (k2) == 0, "reference counter not resetted");
	ksAppendKey (ks, k2);
	succeed_if (keyGetRef (k2) == 1, "reference counter not incremented after insertion");

	ksRewind (ks);
	ksNext (ks);
	ksDel (ks);

	ks = ksNew (0, KS_END);
	k1 = keyNew ("user:/yname", KEY_END);
	k2 = keyDup (k1, KEY_CP_ALL);
	ksAppendKey (ks, k2);
	ksAppendKey (ks, k1);

	ksRewind (ks);
	ksNext (ks);
	ksDel (ks);

	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/e", KEY_END));
	ksAppendKey (ks, keyNew ("user:/b", KEY_END));
	ksAppendKey (ks, keyNew ("user:/b", KEY_END));
	ksAppendKey (ks, keyNew ("user:/d", KEY_END));
	ksAppendKey (ks, keyNew ("user:/c", KEY_END));
	ksAppendKey (ks, keyNew ("user:/c", KEY_END));
	ksAppendKey (ks, keyNew ("user:/g", KEY_END));
	ksAppendKey (ks, keyNew ("user:/h", KEY_END));
	ksAppendKey (ks, keyNew ("user:/h", KEY_END));
	ksAppendKey (ks, keyNew ("user:/f", KEY_END));

	ksRewind (ks);
	for (i = 0; (key = ksNext (ks)) != 0; i++)
	{
		switch (i)
		{
		case 0:
			succeed_if_same_string (keyName (key), "user:/a");
			break;
		case 1:
			succeed_if_same_string (keyName (key), "user:/b");
			break;
		case 2:
			succeed_if_same_string (keyName (key), "user:/c");
			break;
		case 3:
			succeed_if_same_string (keyName (key), "user:/d");
			break;
		case 4:
			succeed_if_same_string (keyName (key), "user:/e");
			break;
		case 5:
			succeed_if_same_string (keyName (key), "user:/f");
			break;
		case 6:
			succeed_if_same_string (keyName (key), "user:/g");
			break;
		case 7:
			succeed_if_same_string (keyName (key), "user:/h");
			break;
		default:
			succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);


	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/e", KEY_END));
	ksAppendKey (ks, keyNew ("user:/b/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/b", KEY_END));
	ksAppendKey (ks, keyNew ("user:/d", KEY_END));
	ksAppendKey (ks, keyNew ("user:/c", KEY_END));
	ksAppendKey (ks, keyNew ("user:/c/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/g", KEY_END));
	ksAppendKey (ks, keyNew ("user:/h/a", KEY_END));
	ksAppendKey (ks, keyNew ("user:/h", KEY_END));
	ksAppendKey (ks, keyNew ("user:/f", KEY_END));

	ksRewind (ks);
	// output_keyset(ks,0);
	for (i = 0; (key = ksNext (ks)) != 0; i++)
	{
		switch (i)
		{
		case 10:
			succeed_if_same_string (keyName (key), "user:/h/a");
			break;
		case 9:
			succeed_if_same_string (keyName (key), "user:/h");
			break;
		case 8:
			succeed_if_same_string (keyName (key), "user:/g");
			break;
		case 7:
			succeed_if_same_string (keyName (key), "user:/f");
			break;
		case 6:
			succeed_if_same_string (keyName (key), "user:/e");
			break;
		case 5:
			succeed_if_same_string (keyName (key), "user:/d");
			break;
		case 4:
			succeed_if_same_string (keyName (key), "user:/c/a");
			break;
		case 3:
			succeed_if_same_string (keyName (key), "user:/c");
			break;
		case 2:
			succeed_if_same_string (keyName (key), "user:/b/a");
			break;
		case 1:
			succeed_if_same_string (keyName (key), "user:/b");
			break;
		case 0:
			succeed_if_same_string (keyName (key), "user:/a");
			break;
		default:
			succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);

	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/dir1/key1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir1/key2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir1/key3", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir2/key1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir3/key1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir3", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir3/key2", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir4", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir5/key1", KEY_END));
	ksAppendKey (ks, keyNew ("user:/dir6/key1", KEY_END));

	ksRewind (ks);
	// output_keyset(ks,0);
	for (i = 0; (key = ksNext (ks)) != 0; i++)
	{
		switch (i)
		{
		case 9:
			succeed_if_same_string (keyName (key), "user:/dir5/key1");
			break;
		case 4:
			succeed_if_same_string (keyName (key), "user:/dir2/key1");
			break;
		case 3:
			succeed_if_same_string (keyName (key), "user:/dir2");
			break;
		case 2:
			succeed_if_same_string (keyName (key), "user:/dir1/key3");
			break;
		case 0:
			succeed_if_same_string (keyName (key), "user:/dir1/key1");
			break;
		case 1:
			succeed_if_same_string (keyName (key), "user:/dir1/key2");
			break;
		case 5:
			succeed_if_same_string (keyName (key), "user:/dir3");
			break;
		case 6:
			succeed_if_same_string (keyName (key), "user:/dir3/key1");
			break;
		case 7:
			succeed_if_same_string (keyName (key), "user:/dir3/key2");
			break;
		case 8:
			succeed_if_same_string (keyName (key), "user:/dir4");
			break;
		case 10:
			succeed_if_same_string (keyName (key), "user:/dir6/key1");
			break;
		default:
			succeed_if (0, "should not reach");
			break;
		}
	}
	ksDel (ks);
}

static void ksUnsort (KeySet * ks)
{
	KeySet * randks = ksNew (0, KS_END); /*This is the final randomized keyset*/
	KeySet * tempks = ksNew (0, KS_END); /*Temporary storage for keys not chosen to be inserted*/

	while (ksGetSize (ks) > 0)
	{
		ksRewind (ks);
		size_t size = ksGetSize (ks);
		/* printf ("iterating %d\n", size); */
		Key * cur;
		while ((cur = ksPop (ks)) != 0)
		{
			/* printf ("\titerating %s\n", keyName(cur)); */
			if (!(rand () % size))
				ksAppendKey (randks, cur);
			else
				ksAppendKey (tempks, cur);
		}
		ksAppend (ks, tempks);
		ksCopy (tempks, 0);
	}

	ksCopy (ks, randks);

	ksDel (randks);
	ksDel (tempks);
}

static void test_ksLookup (void)
{
	printf ("Test lookup\n");

	Key * simpleKey = keyNew ("user:/find_me", KEY_END);
	KeySet * simple = ksNew (5, simpleKey, KS_END);

	Key * foundKey = ksLookup (simple, simpleKey, 0);
	succeed_if (foundKey == simpleKey, "could not find key in keyset");

	Key * simpleKey2 = keyNew ("user:/find_me/a", KEY_END);
	ksAppendKey (simple, simpleKey2);

	foundKey = ksLookup (simple, simpleKey, 0);
	succeed_if (foundKey == simpleKey, "could not find key in keyset again");
	// output_key(foundKey);

	foundKey = ksLookup (simple, simpleKey2, 0);
	succeed_if (foundKey == simpleKey2, "could not find other key in keyset");
	// output_keyset(simple);
	ksDel (simple);

	int i, j;
	Key * k[1000];
	KeySet * ks = ksNew (30,
			     // clang-format off
		       /* keys that are searched */
		       k[0] = keyNew ("user:/rem3", KEY_END),
		       k[1] = keyNew ("user:/rem2", KEY_END),
		       k[2] = keyNew ("user:/rem1/key2", KEY_END),
		       k[3] = keyNew ("user:/rem1/key1", KEY_END),
		       k[4] = keyNew ("user:/rem1", KEY_END),
		       k[5] = keyNew ("user:/dir1", KEY_END),
		       k[6] = keyNew ("user:/dir1/key1", KEY_VALUE, "value1", KEY_END),
		       k[7] = keyNew ("user:/dir1/key2", KEY_VALUE, "value2", KEY_END),
		       k[8] = keyNew ("user:/dir1/key3", KEY_VALUE, "value3", KEY_END),
		       k[9] = keyNew ("user:/dir1/key4", KEY_VALUE, "value4", KEY_END),
		       k[10] = keyNew ("user:/dir1/.inactive1", KEY_META, "comment/#0", "key is inactive", KEY_END),
		       k[11] = keyNew ("user:/dir1/.inactive2", KEY_META, "comment/#0", "additional information", KEY_END),
		       k[12] = keyNew ("user:/dir2", KEY_END),
		       k[13] = keyNew ("user:/dir2/key1", KEY_VALUE, "value1", KEY_END),
		       k[14] = keyNew ("user:/dir2/key2", KEY_VALUE, "value2", KEY_END),
		       k[15] = keyNew ("user:/dir2/key3", KEY_VALUE, "value3", KEY_END),
		       k[16] = keyNew ("user:/dir2/key4", KEY_VALUE, "value4", KEY_END),
		       k[17] = keyNew ("user:/dir3", KEY_END),
		       k[18] = keyNew ("user:/dir3/key1", KEY_VALUE, "value1", KEY_END),
		       k[19] = keyNew ("user:/dir3/.inactive1", KEY_META, "comment/#0", "key is inactive", KEY_END),
		       k[20] = keyNew ("user:/dir3/.inactive2", KEY_META, "comment/#0", "a users comment", KEY_END),
		       k[21] = keyNew ("user:/dir4", KEY_END),
		       k[22] = keyNew ("user:/dir5", KEY_END),
			     // clang-format on
			     KS_END);

	KeySet * lookupKeys = ksNew (30,
				     /* lookup keys, keyset only for ksDel */
				     // clang-format off
				     k[23] = keyNew ("user:/DiR1", KEY_END),
				     k[24] = keyNew ("user:/DiR1/KEY1", KEY_END),
				     k[25] = keyNew ("user:/DiR1/KEY1", KEY_END),
				     k[26] = keyNew ("user:/DiR1/KEY1", KEY_END),
				     k[27] = keyNew ("user:/dir1/key1", KEY_END),
				     k[28] = keyNew ("user:/dir1/key1", KEY_END),
				     k[29] = keyNew ("user:/dir2/key1", KEY_END),
				     k[30] = keyNew ("user:/dir2/key1", KEY_END),
				     k[31] = keyNew ("user:/dir2/key1", KEY_END),
				     k[32] = keyNew ("/dir1/key1", KEY_END),
				     k[33] = keyNew ("/dirX/keyY", KEY_END),
				     // clang-format on
				     KS_END);
	succeed_if (keyGetNameSize (k[32]) == 11, "initial size of name wrong");
	succeed_if (keyGetNameSize (k[33]) == 11, "initial size of name wrong");

	srand (23);

	succeed_if (ksLookup (0, k[23], 0) == 0, "null pointer");
	succeed_if (ksLookup (ks, 0, 0) == 0, "null pointer");

	for (i = 0; i < 100; i++)
	{
		ksUnsort (ks);
		for (j = 0; j < 23; j++)
		{
			succeed_if (ksLookup (ks, k[j], 0) == k[j], "did not find key");
		}
		succeed_if (ksLookup (ks, k[23], 0) == 0, "found wrong key");
		succeed_if (ksLookup (ks, k[26], 0) == 0, "found wrong key");
		succeed_if (ksLookup (ks, k[28], 0) == k[6], "did not find key");
		succeed_if (ksLookup (ks, k[32], 0) == k[6], "did not find key");
		succeed_if (ksLookup (ks, k[33], 0) == 0, "found wrong key");

		succeed_if (keyGetNameSize (k[32]) == 11, "size of name was changed");
		succeed_if (keyGetNameSize (k[33]) == 11, "size of name was changed");
		/* Empty lines to add more tests:
		succeed_if (ksLookup(ks, k[], ) == k[], "did not find key");
		succeed_if (ksLookup(ks, k[], ) == 0, "found wrong key");
		*/
	}

	ksDel (ks);
	ksDel (lookupKeys);
}

static void test_ksLookupByName (void)
{
	printf ("Test lookup by name\n");

	int i, j;
	char * name[1000];
	Key * k[1000];
	KeySet * ks = ksNew (30, k[0] = keyNew (name[0] = "user:/rem3", KEY_END), k[1] = keyNew (name[1] = "user:/rem2", KEY_END),
			     k[2] = keyNew (name[2] = "user:/rem1/key2", KEY_END), k[3] = keyNew (name[3] = "user:/rem1/key1", KEY_END),
			     k[4] = keyNew (name[4] = "user:/rem1", KEY_END), k[5] = keyNew (name[5] = "user:/dir1", KEY_END),
			     k[6] = keyNew (name[6] = "user:/dir1/key1", KEY_VALUE, "value1", KEY_END),
			     k[7] = keyNew (name[7] = "user:/dir1/key2", KEY_VALUE, "value2", KEY_END),
			     k[8] = keyNew (name[8] = "user:/dir1/key3", KEY_VALUE, "value3", KEY_END),
			     k[9] = keyNew (name[9] = "user:/dir1/key4", KEY_VALUE, "value4", KEY_END),
			     k[10] = keyNew (name[10] = "user:/dir1/.inactive1", KEY_META, "comment/#0", "key is inactive", KEY_END),
			     k[11] = keyNew (name[11] = "user:/dir1/.inactive2", KEY_META, "comment/#0", "additional information", KEY_END),
			     k[12] = keyNew (name[12] = "user:/dir2", KEY_END),
			     k[13] = keyNew (name[13] = "user:/dir2/key1", KEY_VALUE, "value1", KEY_END),
			     k[14] = keyNew (name[14] = "user:/dir2/key2", KEY_VALUE, "value2", KEY_END),
			     k[15] = keyNew (name[15] = "user:/dir2/key3", KEY_VALUE, "value3", KEY_END),
			     k[16] = keyNew (name[16] = "user:/dir2/key4", KEY_VALUE, "value4", KEY_END),
			     k[17] = keyNew (name[17] = "user:/dir3", KEY_END),
			     k[18] = keyNew (name[18] = "user:/dir3/key1", KEY_VALUE, "value1", KEY_END),
			     k[19] = keyNew (name[19] = "user:/dir3/.inactive1", KEY_META, "comment/#0", "key is inactive", KEY_END),
			     k[20] = keyNew (name[20] = "user:/dir3/.inactive2", KEY_META, "comment/#0", "a users comment", KEY_END),
			     k[21] = keyNew (name[21] = "user:/dir4", KEY_END), k[22] = keyNew (name[22] = "user:/dir5", KEY_END), KS_END);

	name[23] = "user:/DiR1";
	name[24] = "user:/DiR1/KEY1";
	name[25] = "user:/DiR1/KEY1";
	name[26] = "user:/DiR1/KEY1";
	name[27] = "user:/dir1/key1";
	name[28] = "user:/dir1/key1";
	name[29] = "user:/dir2/key1";
	name[30] = "user:/dir2/key1";
	name[31] = "user:/dir2/key1";
	name[32] = "user://dir1";
	name[33] = "user:///dir1";
	name[34] = "user:///./dir1";
	name[35] = "user:///./../dir1";
	name[36] = "user:///./../dir1/";
	name[37] = "user:///./../dir1//";

	srand (23);

	succeed_if (ksLookupByName (0, name[23], 0) == 0, "null pointer");
	succeed_if (ksLookup (ks, 0, 0) == 0, "null pointer");

	for (i = 0; i < 100; i++)
	{
		ksUnsort (ks);
		for (j = 0; j < 23; j++)
			succeed_if (ksLookupByName (ks, name[j], 0) == k[j], "did not find key");
		succeed_if (ksLookupByName (ks, name[23], 0) == 0, "found wrong key");
		succeed_if (ksLookupByName (ks, name[24], 0) == 0, "found wrong key");
		succeed_if (ksLookupByName (ks, name[28], 0) == k[6], "did not find key");
		for (int n = 32; n < 38; ++n)
			succeed_if (ksLookupByName (ks, name[n], 0) == k[5], "did not find key");
		/* Empty lines to add more tests:
		succeed_if (ksLookupByName(ks, name[], ) == name[], "did not find key");
		succeed_if (ksLookupByName(ks, name[], ) == 0, "found wrong key");
		*/
	}

	ksDel (ks);
}


#ifdef __SANITIZE_ADDRESS__
ELEKTRA_UNUSED
#endif
static void test_ksLookupName (void)
{
	Key * found;
	KeySet * ks = ksNew (0, KS_END);

	printf ("Test lookup functions\n");

	ksAppendKey (ks, keyNew ("user:/domain/key", KEY_VALUE, "domainvalue", KEY_END));
	ksAppendKey (ks, keyNew ("user:/single/key", KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey (ks, keyNew ("user:/named/key", KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/syskey", KEY_VALUE, "syskey", KEY_END));
	ksAppendKey (ks, keyNew ("system:/sysonly/key", KEY_VALUE, "sysonlykey", KEY_END));
	ksAppendKey (ks, keyNew ("user:/named/bin", KEY_BINARY, KEY_SIZE, strlen ("binary\1\2data"), KEY_VALUE, "binary\1\2data", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/bin", KEY_BINARY, KEY_SIZE, strlen ("sys\1bin\2"), KEY_VALUE, "sys\1bin\2", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/key", KEY_BINARY, KEY_SIZE, strlen ("syskey"), KEY_VALUE, "syskey", KEY_END));
	succeed_if (ksGetSize (ks) == 8, "could not append all keys");

	// a positive test case
	found = ksLookupByName (ks, "user:/named/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");

	succeed_if (found != 0, "did not find correct name");
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");

	ksAppendKey (ks, found = keyNew ("user:/single/key", KEY_VALUE, "singlevalue", KEY_END));
	succeed_if (ksCurrent (ks) == found, "current update after append");
	succeed_if_same_string (keyName (found), "user:/single/key");
	succeed_if_same_string (keyValue (found), "singlevalue");

	// here you can't find the keys
	succeed_if (ksLookupByName (ks, "named/key", 0) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "u/named/key", 0) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "usea/named/key", 0) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, " user:/named/key", 0) == 0, "found key with bad prefix");

	succeed_if (ksLookupByName (ks, "user:/named/Key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User:/Named/key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User:/named/key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "user:/NAMED/key", 0) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "USER:/NAMED/KEY", 0) == 0, "found wrong case key");

	succeed_if (ksLookupByName (ks, "user:/named/keys", 0) == 0, "wrong postfix");
	succeed_if (ksLookupByName (ks, "user:/named/key_", 0) == 0, "wrong postfix");

	succeed_if (ksLookupByName (ks, "user:/named/k/ey", 0) == 0, "seperation that should be");
	succeed_if (ksLookupByName (ks, "user:/na/med/key", 0) == 0, "seperation that should be");

	succeed_if (ksLookupByName (ks, "system:/domain/key", 0) == 0, "found key in wrong domain");

	// broken names
	succeed_if (ksLookupByName (ks, "sys", 0) == 0, "found key with broken entry");
	succeed_if (ksLookupByName (ks, "what", 0) == 0, "found key with broken entry");
	succeed_if (ksLookupByName (ks, "", 0) == 0, "found key with empty entry");
	succeed_if (ksLookupByName (ks, "_", 0) == 0, "found key with broken entry");
#ifdef COMPAT
	succeed_if (ksLookupByName (ks, "\\", 0) == 0, "found key with broken entry");
#endif
	succeed_if (ksLookupByName (ks, "\\/", 0) == 0, "found key with broken entry");

	// now try to find them, and compare value
	found = ksLookupByName (ks, "user:/domain/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	exit_if_fail (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "user:/domain/key");
	succeed_if_same_string (keyValue (found), "domainvalue");

	found = ksLookupByName (ks, "user:/single/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "user:/single/key");
	succeed_if_same_string (keyValue (found), "singlevalue");

	found = ksLookupByName (ks, "system:/named/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "system:/named/key");
	succeed_if (strncmp (keyValue (found), "syskey", strlen ("syskey")) == 0, "not correct value in found key");

	found = ksLookupByName (ks, "user:/named/bin", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "user:/named/bin");
	succeed_if (strncmp (keyValue (found), "binary\1\2data", strlen ("binary\1\2data")) == 0, "not correct value in found key");

	found = ksLookupByName (ks, "user:/named/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");

	ksDel (ks);
}

static void test_ksLookupNameCascading (void)
{
	Key * found;
	KeySet * ks = ksNew (0, KS_END);

	printf ("Test cascading lookup functions\n");

	succeed_if (ksLookupByName (ks, "/named/", 0) == 0, "found in empty keyset");
	succeed_if (ksLookupByName (ks, "//named/", 0) == 0, "found in empty keyset");
	succeed_if (ksLookupByName (ks, "////named/", 0) == 0, "found in empty keyset");
	succeed_if (ksLookupByName (ks, "//Person/Visits", 0) == 0, "found in empty keyset");

	ksAppendKey (ks, keyNew ("user:/named/key", KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/key", KEY_VALUE, "wrong value", KEY_END));
	ksAppendKey (ks, keyNew ("user:/single/key", KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey (ks, keyNew ("system:/sysonly/key", KEY_VALUE, "sysonlykey", KEY_END));
	ksAppendKey (ks, keyNew ("user:/named/otherkey", KEY_VALUE, "singlevalue", KEY_END));

	found = ksLookupByName (ks, "/named/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");

	found = ksLookupByName (ks, "/sysonly/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if_same_string (keyName (found), "system:/sysonly/key");
	succeed_if_same_string (keyValue (found), "sysonlykey");

	succeed_if (ksLookupByName (ks, "/named/", 0) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "/named/keyd", 0) == 0, "found part of key with cascading, bad postfix");


	// cascading double slash

	found = ksLookupByName (ks, "///named/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");

	found = ksLookupByName (ks, "//sysonly/key", 0);
	succeed_if (ksCurrent (ks) == found, "current not set correctly");
	succeed_if (found != 0, "could not find same key again, nocase used");
	succeed_if_same_string (keyName (found), "system:/sysonly/key");
	succeed_if_same_string (keyValue (found), "sysonlykey");

	succeed_if (ksLookupByName (ks, "//Person/Visits", 0) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "////named/", 0) == 0, "found part of key with cascading");
	succeed_if (ksLookupByName (ks, "/////named/keyd", 0) == 0, "found part of key with cascading, bad postfix");

	ksAppendKey (ks, keyNew ("user:/named/key", KEY_VALUE, "myvalue", KEY_END));
	found = ksLookupByName (ks, "//named/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 4, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksDel (ks);


	ks = ksNew (10, KS_END);
	ksAppendKey (ks, keyNew ("system:/test/myapp/key", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (ks, keyNew ("user:/test/myapp/key", KEY_VALUE, "correct", KEY_END));

	succeed_if_same_string (keyString (ksLookupByName (ks, "/test/myapp/key", 0)), "correct");
	Key * s = 0;
	succeed_if_same_string (keyString (s = ksLookupByName (ks, "/test/myapp/key", KDB_O_POP)), "correct");
	keyDel (s);
	succeed_if_same_string (keyString (s = ksLookupByName (ks, "/test/myapp/key", KDB_O_POP)), "wrong");
	keyDel (s);
	ksDel (ks);


	ks = ksNew (10, KS_END);
	Key * k1;
	Key * k2;
	ksAppendKey (ks, k1 = keyNew ("system:/test/myapp/key", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (ks, k2 = keyNew ("user:/test/myapp/key", KEY_VALUE, "correct", KEY_END));
	ksAppendKey (ks, keyDup ((s = keyNew ("/test/myapp/key", KEY_END)), KEY_CP_ALL));
	succeed_if (ksGetSize (ks) == 3, "initial size of keyset");
	succeed_if (keyGetNameSize (s) == 16, "initial name size");

	succeed_if (ksLookup (ks, s, 0) == k2, "got wrong key (not user)");
	succeed_if (ksLookup (ks, s, 0) != k1, "got system key first");
	succeed_if (ksLookup (ks, s, 0) != s, "got cascading key");
	succeed_if_same_string (keyString (ksLookup (ks, s, 0)), "correct");
	succeed_if (ksGetSize (ks) == 3, "lookup without pop changed size");
	succeed_if (keyGetNameSize (s) == 16, "size changed after lookup");

	succeed_if_same_string (keyString (ksLookup (ks, s, KDB_O_POP)), "correct");
	succeed_if (ksGetSize (ks) == 2, "lookup with pop did not change size");
	succeed_if (keyGetNameSize (s) == 16, "size changed after lookup");

	succeed_if (ksLookup (ks, s, 0) == k1, "got wrong key (not system)");
	succeed_if (ksLookup (ks, s, 0) != k2, "got user key again");
	succeed_if (ksLookup (ks, s, 0) != s, "got cascading key");
	succeed_if_same_string (keyString (ksLookup (ks, s, KDB_O_POP)), "wrong");
	succeed_if (ksGetSize (ks) == 1, "lookup with pop did not change size");
	succeed_if (keyGetNameSize (s) == 16, "size changed after lookup");
	ksDel (ks);
	keyDel (s);
	keyDel (k1);
	keyDel (k2);


	ks = ksNew (10, KS_END);
	ksAppendKey (ks, k1 = keyNew ("system:/test/myapp/key", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (ks, k2 = keyNew ("user:/test/myapp/key", KEY_VALUE, "correct", KEY_END));

	succeed_if_same_string (keyString (ksLookup (ks, k2, KDB_O_POP)), "correct");
	succeed_if_same_string (keyString (ksLookup (ks, k1, KDB_O_POP)), "wrong");

	keyDel (k1);
	keyDel (k2);
	ksDel (ks);


	ks = ksNew (10, KS_END);
	ksAppendKey (ks, k1 = keyNew ("system:/test/myapp/key", KEY_VALUE, "wrong", KEY_END));
	ksAppendKey (ks, k2 = keyNew ("user:/test/myapp/key", KEY_VALUE, "correct", KEY_END));

	succeed_if_same_string (keyString (ksLookup (ks, k1, KDB_O_POP)), "wrong");
	succeed_if_same_string (keyString (ksLookup (ks, k2, KDB_O_POP)), "correct");

	keyDel (k1);
	keyDel (k2);
	ksDel (ks);
}

static void test_ksExample (void)
{
	KeySet * ks = ksNew (0, KS_END);
	Key * key;

	ksAppendKey (ks, keyNew ("user:/test", KEY_END)); // an empty key

	ksAppendKey (ks, keyNew ("user:/sw", // the name of the key
				 KEY_END));  // no more args

	ksAppendKey (ks, keyNew ("user:/tmp/ex1", KEY_VALUE, "some data", // set a string value
				 KEY_END));				  // end of args

	ksAppendKey (ks, keyNew ("user:/tmp/ex4",
				 KEY_BINARY,		 // key type
				 KEY_SIZE, 7,		 // assume binary length 7
				 KEY_VALUE, "some data", // value that will be truncated in 7 bytes
				 KEY_META, "comment/#0", "value is truncated",
				 KEY_END)); // end of args

	ksAppendKey (ks, keyNew ("user:/tmp/ex5",
				 KEY_BINARY,			      // binary value
				 KEY_SIZE, 7, KEY_VALUE, "some data", // value that will be truncated in 7 bytes
				 KEY_META, "comment/#0", "value is truncated",
				 KEY_END)); // end of args

	ksRewind (ks);

	key = ksNext (ks);
	succeed_if (key != NULL, "no next key");
	succeed_if_same_string (keyName (key), "user:/sw");

	key = ksNext (ks);
	succeed_if (key != NULL, "no next key");
	succeed_if_same_string (keyName (key), "user:/test");

	key = ksNext (ks);
	succeed_if (key != NULL, "no next key");
	succeed_if_same_string (keyName (key), "user:/tmp/ex1");

	key = ksNext (ks);
	succeed_if (key != NULL, "no next key");
	succeed_if_same_string (keyName (key), "user:/tmp/ex4");

	key = ksNext (ks);
	succeed_if (key != NULL, "no next key");
	succeed_if_same_string (keyName (key), "user:/tmp/ex5");

	ksDel (ks);
}

static void test_ksAppend (void)
{
	int i;

	printf ("Test appending keys\n");

	Key * key = keyNew ("user:/test", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (ksAppendKey (0, key) == -1, "No error on NULL pointer");
	succeed_if (ksAppendKey (ks, 0) == -1, "No error on NULL pointer");
	ksDel (ks);
	keyDel (key);

	KeySet * returned =
#include "../data/data_keyset.c"
		KeySet * testDirectBelow =
#include "../data/data_dbelow.c"
			KeySet * testReturned =
#include "../data/data_others.c"
				Key * parentKey[2];
	parentKey[0] = keyNew ("user:/test/keyset", KEY_END);
	parentKey[1] = keyNew ("user:/test/keyset/dir1", KEY_END);

	/* A real world example out in kdb.c */
	for (i = 0; i < 2; i++)
	{
		KeySet * tmp = ksNew (ksGetSize (returned), KS_END);
		KeySet * keys = ksNew (0, KS_END);

		/* add all keys direct below parentKey */
		ksRewind (returned);
		Key * current;
		while ((current = ksPop (returned)) != 0)
		{
			if (keyIsDirectlyBelow (parentKey[i], current))
			{
				ksAppendKey (keys, current);
			}
			else
			{
				ksAppendKey (tmp, current);
			}
		}
		ksAppend (returned, tmp);

		/*
		ksOutput (tmp, stdout, KDB_O_HEADER);
		ksOutput (returned, stdout, KDB_O_HEADER);
		printf (" ----- keys -------\n");
		ksOutput (keys, stdout, KDB_O_HEADER);
		*/

		if (!i)
		{
			compare_keyset (returned, testReturned);
			compare_keyset (keys, testDirectBelow);

			succeed_if (ksGetSize (tmp) == 84, "size not correct");
			succeed_if (ksGetSize (returned) == 84, "size not correct");
			succeed_if (ksGetSize (keys) == 18, "size not correct");

			// succeed_if (ksGetAlloc (tmp) == 102, "alloc not correct");
			// succeed_if (ksGetAlloc (returned) == 127, "alloc not correct");
			// succeed_if (ksGetAlloc (keys) == 31, "alloc not correct");
		}

		ksAppend (returned, keys); /* add the keys back */

		ksDel (tmp);
		ksDel (keys);
	}

	keyDel (parentKey[0]);
	keyDel (parentKey[1]);

	ksDel (testReturned);
	ksDel (testDirectBelow);
	ksDel (returned);

	ks = ksNew (0, KS_END);
	ksAppendKey (ks, keyNew ("user:/abc", KEY_META, "xyz", "egh", KEY_END));

	KeySet * other = ksNew (0, KS_END);
	ksAppend (other, ks);
	compare_keyset (ks, other);
	compare_keyset (ks, ks);

	succeed_if (ksAppend (ks, 0) == -1, "No error on NULL pointer");
	succeed_if (ksAppend (0, ks) == -1, "No error on NULL pointer");

	ksDel (other);
	ksDel (ks);
}


/**A functional mode to keys.
 *
 * Instead of writing your own loop you can write
 * a function working with a key and pass it to
 * this method.
 *
 * The function will be executed for all keys in
 * the keyset.
 *
 * @param ks the keyset to work with
 * @param func the function to execute on every key of the keyset
 * @return the sum of all return values
 * @return -1 if any function returned -1, the execution will stop there, but
 * 	ksCurrent() will tell you where it stopped.
 * @see ksFilter()
 */
int ksForEach (KeySet * ks, int (*func) (Key * k))
{
	int ret = 0;
	Key * current;

	elektraCursor cursor = ksGetCursor (ks);
	ksRewind (ks);
	while ((current = ksNext (ks)) != 0)
	{
		int rc = func (current);
		if (rc == -1) return -1;
		ret += rc;
	}
	ksSetCursor (ks, cursor);
	return ret;
}


/**Filter a keyset.
 *
 * filter is executed for every key in the keyset result. When it returns 0,
 * the key will be dropped, when it returns 1 it will be ksAppendKey()ed to result,
 * when it returns -1 the processing will be stopped. You can use ksCurrent()
 * on input to see where the problem was. Because of this input is not const,
 * apart from ksCurrent() the input will not be changed. The keys that have
 * been in result before will stay untouched.
 *
 * @param result is the keyset where keys are added.
 * @param input is the keyset the filter works on.
 * @param filter is the function to execute on every key of the keyset to decide if
 * 	it should be ksAppendKey()ed to the result.
 * @return the number of keys added on success
 * @return 0 when nothing was done
 * @return -1 when filter returned an error (-1), ksCurrent() of input will
 * 	be the problematic key.
 * @see ksForEach()
 **/
int ksFilter (KeySet * result, KeySet * input, int (*filter) (Key * k))
{
	int ret = 0;
	Key * current;

	elektraCursor cursor = ksGetCursor (input);
	ksRewind (input);
	while ((current = ksNext (input)) != 0)
	{
		int rc = filter (current);
		if (rc == -1)
			return -1;
		else if (rc != 0)
		{
			++ret;
			ksAppendKey (result, keyDup (current, KEY_CP_ALL));
		}
	}
	ksSetCursor (input, cursor);
	return ret;
}


Key * global_a;

int add_string (Key * check)
{
	return keySetString (check, "string");
}
// int add_comment (Key *check) { return keySetComment (check, "comment"); }
int has_a (Key * check)
{
	return keyName (check)[6] == 'a';
}
int below_a (Key * check)
{
	return keyIsBelow (global_a, check);
}
int direct_below_a (Key * check)
{
	return keyIsDirectlyBelow (global_a, check);
}

int sum_helper (Key * check)
{
	return atoi (keyValue (check));
}
int below_30 (Key * check)
{
	return atoi (keyValue (check)) < 30;
}
int find_80 (Key * check)
{
	int n = atoi (keyValue (check));
	return n > 70 ? -1 : 1;
}

static void test_ksFunctional (void)
{
	Key * found;
	Key * current;
	KeySet * out;
	KeySet * ks = ksNew (64, keyNew ("user:/a/1", KEY_END), keyNew ("user:/a/2", KEY_END), keyNew ("user:/a/b/1", KEY_END),
			     keyNew ("user:/a/b/2", KEY_END), keyNew ("user:/ab/2", KEY_END), keyNew ("user:/b/1", KEY_END),
			     keyNew ("user:/b/2", KEY_END), KS_END);
	global_a = keyNew ("user:/a", KEY_END);

	printf ("Test functional style\n");

	ksForEach (ks, add_string);
	// ksForEach (ks, add_comment);

	ksRewind (ks);
	while ((current = ksNext (ks)) != 0)
	{
		succeed_if_same_string (keyValue (current), "string");
		// succeed_if_same_string (keyComment (current), "comment");
	}

	out = ksNew (0, KS_END);
	succeed_if (ksGetSize (ks) == 7, "initial size wrong");
	succeed_if (ksGetSize (out) == 0, "initial size wrong");
	ksFilter (out, ks, has_a);
	succeed_if (ksGetSize (out) == 5, "has_a cut more than the user:/b");
	ksDel (out);

	out = ksNew (0, KS_END);
	ksFilter (out, ks, below_a);
	succeed_if (ksGetSize (out) == 4, "below_a cut more than the user:/ab/2");
	ksDel (out);

	out = ksNew (0, KS_END);
	ksFilter (out, ks, direct_below_a);
	succeed_if (ksGetSize (out) == 2, "direct_below_a cut more than the user:/a/b/*");
	ksDel (out);

	ksDel (ks);
	keyDel (global_a);
	global_a = 0;

	KeySet * values = ksNew (64, keyNew ("user:/a", KEY_VALUE, "40", KEY_END), keyNew ("user:/b", KEY_VALUE, "20", KEY_END),
				 keyNew ("user:/c", KEY_VALUE, "80", KEY_END), keyNew ("user:/d", KEY_VALUE, "24", KEY_END),
				 keyNew ("user:/e", KEY_VALUE, "32", KEY_END), keyNew ("user:/f", KEY_VALUE, "12", KEY_END),
				 keyNew ("user:/g", KEY_VALUE, "43", KEY_END), KS_END);

	succeed_if (ksForEach (values, sum_helper) == 251, "could not sum up");

	KeySet * values_below_30 = ksNew (0, KS_END);
	ksFilter (values_below_30, values, below_30);
	succeed_if (ksGetSize (values_below_30) == 3, "could not filter out everything above 30");
	succeed_if (ksForEach (values_below_30, sum_helper) == 56, "could not sum up");

	succeed_if (ksForEach (values, find_80) == -1, "did not find 80");
	found = ksCurrent (values);
	succeed_if (ksLookupByName (values, "user:/c", 0) == found, "did not find 80");
	/*succeed_if (ksLookupByString (values, "80", 0) == found, "lookup by value did not find 80");*/
	ksDel (values);
	ksDel (values_below_30);
}

#ifdef __SANITIZE_ADDRESS__
ELEKTRA_UNUSED
#endif
static void test_ksLookupPop (void)
{
	printf ("Test ksLookup with KDB_O_POP\n");

	Key * found;
	Key *a, *b, *c;
	KeySet * small =
		ksNew (5, a = keyNew ("user:/a", KEY_END), b = keyNew ("user:/b", KEY_END), c = keyNew ("user:/c", KEY_END), KS_END);

	ksRewind (small);
	ksNext (small);
	succeed_if (ksCurrent (small) == a, "current not set correctly");

	succeed_if (ksGetSize (small) == 3, "could not append all keys");
	found = ksLookupByName (small, "user:/a", KDB_O_POP);
	succeed_if (found == a, "not correct key");
	succeed_if_same_string (keyName (found), "user:/a");
	succeed_if (ksCurrent (small) == 0, "current not set correctly");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksNext (small);
	ksNext (small);
	succeed_if (ksCurrent (small) == c, "current not set correctly");

	succeed_if (ksGetSize (small) == 2, "could not append all keys");
	found = ksLookupByName (small, "user:/b", KDB_O_POP);
	succeed_if (found == b, "not correct key");
	succeed_if_same_string (keyName (found), "user:/b");
	succeed_if (ksCurrent (small) == 0, "current not set correctly");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	succeed_if (ksGetSize (small) == 1, "could not append all keys");
	found = ksLookupByName (small, "user:/b", KDB_O_POP);
	succeed_if (found == 0, "found something, but should not");
	succeed_if (ksCurrent (small) == 0, "current not set correctly");

	succeed_if (ksGetSize (small) == 1, "could not append all keys");
	found = ksLookupByName (small, "user:/c", KDB_O_POP);
	succeed_if (found == c, "not correct key");
	succeed_if_same_string (keyName (found), "user:/c");
	succeed_if (ksCurrent (small) == 0, "current not set correctly");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	succeed_if (ksGetSize (small) == 0, "could not append all keys");
	found = ksLookupByName (small, "user:/d", KDB_O_POP);
	succeed_if (found == 0, "found something, but should not");
	succeed_if (ksCurrent (small) == 0, "current not set correctly");

	ksDel (small);

	KeySet * ks = ksNew (0, KS_END);

	ksAppendKey (ks, keyNew ("user:/domain/key", KEY_VALUE, "domainvalue", KEY_END));
	ksAppendKey (ks, keyNew ("user:/single/key", KEY_VALUE, "singlevalue", KEY_END));
	ksAppendKey (ks, keyNew ("user:/named/key", KEY_VALUE, "myvalue", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/skey", KEY_VALUE, "syskey", KEY_END));
	ksAppendKey (ks, keyNew ("system:/sysonly/key", KEY_VALUE, "sysonlykey", KEY_END));
	ksAppendKey (ks, keyNew ("user:/named/bin", KEY_BINARY, KEY_SIZE, strlen ("binary\1\2data"), KEY_VALUE, "binary\1\2data", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/bin", KEY_BINARY, KEY_SIZE, strlen ("sys\1bin\2"), KEY_VALUE, "sys\1bin\2", KEY_END));
	ksAppendKey (ks, keyNew ("system:/named/key", KEY_BINARY, KEY_SIZE, strlen ("syskey"), KEY_VALUE, "syskey", KEY_END));
	succeed_if (ksGetSize (ks) == 8, "could not append all keys");

	// a positive test case
	found = ksLookupByName (ks, "user:/named/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 7, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksAppendKey (ks, keyNew ("user:/named/key", KEY_VALUE, "singlevalue", KEY_END));
	succeed_if (ksGetSize (ks) == 8, "did not append key");

	// here you can't find the keys
	succeed_if (ksLookupByName (ks, "named/key", KDB_O_POP) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "u/named/key", KDB_O_POP) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, "usea/named/key", KDB_O_POP) == 0, "not valid keyname");
	succeed_if (ksLookupByName (ks, " user:/named/key", KDB_O_POP) == 0, "found key with bad prefix");

	succeed_if (ksLookupByName (ks, "user:/named/Key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User:/Named/key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "User:/named/key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "user:/NAMED/key", KDB_O_POP) == 0, "found wrong case key");
	succeed_if (ksLookupByName (ks, "USER:/NAMED/KEY", KDB_O_POP) == 0, "found wrong case key");

	succeed_if (ksLookupByName (ks, "user:/named/keys", KDB_O_POP) == 0, "wrong postfix");
	succeed_if (ksLookupByName (ks, "user:/named/key_", KDB_O_POP) == 0, "wrong postfix");

	succeed_if (ksLookupByName (ks, "user:/named/k/ey", KDB_O_POP) == 0, "seperation that should be");
	succeed_if (ksLookupByName (ks, "user:/na/med/key", KDB_O_POP) == 0, "seperation that should be");

	// a positive test case
	found = ksLookupByName (ks, "user:/named/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 7, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "singlevalue");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksAppendKey (ks, keyNew ("user:/named/otherkey", KEY_VALUE, "singlevalue", KEY_END));
	succeed_if (ksGetSize (ks) == 8, "did not append key");

	succeed_if (ksLookupByName (ks, "system:/domain/key", KDB_O_POP) == 0, "found key in wrong domain");

	// now try to find them, and compare value
	found = ksLookupByName (ks, "user:/domain/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 7, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "user:/domain/key");
	succeed_if_same_string (keyValue (found), "domainvalue");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "user:/single/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 6, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "user:/single/key");
	succeed_if_same_string (keyValue (found), "singlevalue");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "system:/named/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 5, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "system:/named/key");
	succeed_if (strncmp (keyValue (found), "syskey", strlen ("syskey")) == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "user:/named/bin", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 4, "pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "did not find correct name");
	succeed_if_same_string (keyName (found), "user:/named/bin");
	succeed_if (strncmp (keyValue (found), "binary\1\2data", strlen ("binary\1\2data")) == 0, "not correct value in found key");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	found = ksLookupByName (ks, "user:/named/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 4, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found == 0, "could find same key again");

	// cascading

	ksAppendKey (ks, keyNew ("user:/named/key", KEY_VALUE, "myvalue", KEY_END));
	printf ("Test cascading lookup functions\n");
	found = ksLookupByName (ks, "/named/key", KDB_O_POP);
	succeed_if (ksGetSize (ks) == 4, "did not pop key");
	succeed_if (ksCurrent (ks) == 0, "current not set correctly");
	succeed_if (found != 0, "cascading search failed");
	succeed_if_same_string (keyName (found), "user:/named/key");
	succeed_if_same_string (keyValue (found), "myvalue");
	succeed_if (keyDel (found) == 0, "could not del popped key");

	ksDel (ks);
}

static void test_ksDoubleFree (void)
{
	/* Valgrind only test */
	KeySet * ks1 = ksNew (5, keyNew ("user:/abc1", KEY_VALUE, "abc1", KEY_END), keyNew ("user:/abc2", KEY_VALUE, "abc1", KEY_END),
			      keyNew ("user:/abc3", KEY_VALUE, "abc1", KEY_END), KS_END);

	KeySet * ks2 = ksNew (5, keyNew ("user:/abc1", KEY_VALUE, "abc2", KEY_END), keyNew ("user:/abc2", KEY_VALUE, "abc2", KEY_END),
			      keyNew ("user:/abc3", KEY_VALUE, "abc2", KEY_END), KS_END);

	Key * cur;
	ksRewind (ks1);
	while ((cur = ksNext (ks1)) != 0)
	{
		ksAppendKey (ks2, cur);
	}

	ksDel (ks1);
	ksDel (ks2);
}

static void test_ksDoubleAppend (void)
{
	printf ("Test double appending\n");

	KeySet * ks1 = ksNew (5, keyNew ("user:/abc1", KEY_VALUE, "abc1", KEY_END), keyNew ("user:/abc2", KEY_VALUE, "abc1", KEY_END),
			      keyNew ("user:/abc3", KEY_VALUE, "abc1", KEY_END), KS_END);

	KeySet * ks2 = ksNew (5, keyNew ("user:/abc1", KEY_VALUE, "abc2", KEY_END), keyNew ("user:/abc2", KEY_VALUE, "abc2", KEY_END),
			      keyNew ("user:/abc3", KEY_VALUE, "abc2", KEY_END), KS_END);

	ksAppend (ks1, ks2);
	succeed_if (ksGetSize (ks1) == 3, "size not correct");
	succeed_if (ksGetSize (ks2) == 3, "size not correct");

	ksDel (ks1);
	ksDel (ks2);
}

static void test_ksDoubleAppendKey (void)
{
	printf ("Test double appending of key\n");

	Key * k = keyNew ("user:/my_double_key", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	ksAppendKey (ks, k);
	succeed_if (ksGetSize (ks) == 1, "size not correct");

	ksAppendKey (ks, k);
	succeed_if (ksGetSize (ks) == 1, "size not correct");

	ksAppendKey (ks, k);
	succeed_if (ksGetSize (ks) == 1, "size not correct");

	Key * k2 = keyNew ("user:/other", KEY_END);

	ksAppendKey (ks, k2);
	succeed_if (ksGetSize (ks) == 2, "size not correct");

	keyDel (k); // has no effect

	ksDel (ks);
	// don't free key here!!
}

static void test_ksAppendKey (void)
{
	printf ("Test cursor after appending key\n");
	KeySet * ks = 0;
	Key * cur;

	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/a", KEY_END)) == 1, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/b", KEY_END)) == 2, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/x", KEY_END)) == 3, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksGetSize (ks) == 3, "size not correct after 3 keys");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/b", KEY_END)) == 3, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position (same key)");
	succeed_if (ksGetSize (ks) == 3, "size not correct after double append");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/0", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position (front key)");
	succeed_if (ksGetSize (ks) == 4, "size not correct after 4 keys");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/c", KEY_END)) == 5, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position (key in between)");
	succeed_if (ksGetSize (ks) == 5, "size not correct after 5 keys");

	ksDel (ks);

	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");
	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/", KEY_END)) == 1, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksGetSize (ks) == 1, "size not correct after 1 keys");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/tests", KEY_END)) == 2, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksGetSize (ks) == 2, "size not correct after 2 keys");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/tests/folder", KEY_END)) == 3, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksGetSize (ks) == 3, "size not correct after 3 keys");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/tests/folder/bool_key", KEY_END)) == 4, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (ksGetSize (ks) == 4, "size not correct after 4 keys");

	Key * newKey = keyDup (cur, KEY_CP_ALL);
	keySetBaseName (newKey, "second_bool_key");

	succeed_if (ksAppendKey (ks, newKey) == 5, "could not append a key");
	succeed_if (ksCurrent (ks) == newKey, "did not update current position");
	succeed_if (ksGetSize (ks) == 5, "size not correct after 5 keys");
	ksDel (ks);
}

static void test_ksModifyKey (void)
{
	printf ("Test modify key after insertion\n");

	KeySet * ks = 0;
	Key * cur;

	exit_if_fail ((ks = ksNew (0, KS_END)) != 0, "could not create new keyset");

	succeed_if (ksAppendKey (ks, cur = keyNew ("user:/a", KEY_END)) == 1, "could not append a key");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");
	succeed_if (keySetName (cur, "user:/b") == -1, "set name with appended key should be disallowed");
	succeed_if (keySetString (cur, "x") > 0, "changing value is ok");
	succeed_if (keySetMeta (cur, "x", "y") > 0, "changing meta is ok");
	succeed_if (ksCurrent (ks) == cur, "did not update current position");

	ksDel (ks);
}

static void test_ksOrder (void)
{
	KeySet * ks = ksNew (20, keyNew ("user:/test/test", KEY_END), keyNew ("user:/test/test/bar", KEY_END),
			     keyNew ("user:/test/test/foo", KEY_END), keyNew ("user:/test/test-foo", KEY_END), KS_END);

	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/test/test");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/test/test/bar");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/test/test/foo");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/test/test-foo");

	ksDel (ks);

	ks = ksNew (20, keyNew ("user:/x", KEY_END), keyNew ("user:/x/%", KEY_END), keyNew ("user:/x/%/a", KEY_END),
		    keyNew ("user:/x/%/b", KEY_END), keyNew ("user:/x/\\%", KEY_END), keyNew ("user:/x/\\%/a", KEY_END),
		    keyNew ("user:/x/\\%/b", KEY_END), keyNew ("user:/x/A", KEY_END), keyNew ("user:/x/A/a", KEY_END),
		    keyNew ("user:/x/A/b", KEY_END), keyNew ("user:/x/%a", KEY_END), keyNew ("user:/x/%b", KEY_END),
		    keyNew ("user:/x/a\\/", KEY_END), keyNew ("user:/x/a\\/b", KEY_END), keyNew ("user:/x/a\\/b/a", KEY_END),
		    keyNew ("user:/x/a\\/b/b", KEY_END), keyNew ("user:/x/aA", KEY_END), keyNew ("user:/x/aA/a", KEY_END),
		    keyNew ("user:/x/aA/b", KEY_END), keyNew ("user:/x/aa", KEY_END), keyNew ("user:/x/aa/a", KEY_END),
		    keyNew ("user:/x/aa/b", KEY_END), KS_END);

	succeed_if (ksCurrent (ks) == 0, "not rewinded");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/%");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/%/a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/%/b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/\\%");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/\\%/a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/\\%/b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/%a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/%b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/A");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/A/a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/A/b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/a\\/");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/a\\/b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/a\\/b/a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/a\\/b/b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/aA");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/aA/a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/aA/b");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/aa");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/aa/a");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/x/aa/b");
	ksDel (ks);
}

static void test_ksOrderNs (void)
{
	Key * cascadingKey = keyNew ("/key", KEY_END);
	Key * metaKey = keyNew ("meta:/key", KEY_END);
	Key * specKey = keyNew ("spec:/key", KEY_END);
	Key * procKey = keyNew ("proc:/key", KEY_END);
	Key * dirKey = keyNew ("dir:/key", KEY_END);
	Key * userKey = keyNew ("user:/key", KEY_END);
	Key * systemKey = keyNew ("system:/key", KEY_END);
	Key * defaultKey = keyNew ("default:/key", KEY_END);

	KeySet * ks = ksNew (8, systemKey, userKey, metaKey, defaultKey, cascadingKey, specKey, procKey, dirKey, KS_END);

	succeed_if (ksAtCursor (ks, 0) == cascadingKey, "cascading not first");
	succeed_if (ksAtCursor (ks, 1) == metaKey, "meta not second");
	succeed_if (ksAtCursor (ks, 2) == specKey, "spec not third");
	succeed_if (ksAtCursor (ks, 3) == procKey, "proc not fourth");
	succeed_if (ksAtCursor (ks, 4) == dirKey, "dir not fifth");
	succeed_if (ksAtCursor (ks, 5) == userKey, "user not sixth");
	succeed_if (ksAtCursor (ks, 6) == systemKey, "system not seventh");
	succeed_if (ksAtCursor (ks, 7) == defaultKey, "default not last");

	ksDel (ks);
}

KeySet * fill_vaargs (size_t size, ...)
{
	va_list ap;
	va_start (ap, size);
	KeySet * ks = ksVNew (size, ap);
	va_end (ap);
	return ks;
}

static void test_keyVNew (void)
{
	printf ("Test keyVNew\n");

	KeySet * ks = 0;

	/*
		Not possible on some platforms:

		ks = ksVNew(0, *(va_list*)KS_END);
		succeed_if (ks != 0, "did not create KeySet");
		ksDel(ks);

		ks = ksVNew(10, *(va_list*)KS_END);
		succeed_if (ks != 0, "did not create KeySet");
		ksDel(ks);
	*/

	ks = fill_vaargs (20, keyNew ("user:/a", KEY_END), KS_END);
	succeed_if (ks != 0, "did not create KeySet");
	succeed_if (ksGetSize (ks) == 1, "KeySet wrong size");
	succeed_if (ksLookupByName (ks, "user:/a", 0) != 0, "could not lookup key");
	ksDel (ks);
}


static KeySet * set_a (void)
{
	return ksNew (16, keyNew ("user:/0", KEY_END), keyNew ("user:/a", KEY_END), keyNew ("user:/a/a", KEY_END),
		      keyNew ("user:/a/a/a", KEY_END), keyNew ("user:/a/a/b", KEY_END), keyNew ("user:/a/b", KEY_END),
		      keyNew ("user:/a/b/a", KEY_END), keyNew ("user:/a/b/b", KEY_END), keyNew ("user:/a/c", KEY_END),
		      keyNew ("user:/a/d", KEY_END), keyNew ("user:/a/x/a", KEY_END), keyNew ("user:/a/x/b", KEY_END),
		      keyNew ("user:/a/x/c", KEY_END), keyNew ("user:/a/x/c/a", KEY_END), keyNew ("user:/a/x/c/b", KEY_END),
		      keyNew ("user:/x", KEY_END), KS_END);
}

static KeySet * set_oa (void)
{
	return ksNew (14, keyNew ("user:/a", KEY_END), keyNew ("user:/a/a", KEY_END), keyNew ("user:/a/a/a", KEY_END),
		      keyNew ("user:/a/a/b", KEY_END), keyNew ("user:/a/b", KEY_END), keyNew ("user:/a/b/a", KEY_END),
		      keyNew ("user:/a/b/b", KEY_END), keyNew ("user:/a/c", KEY_END), keyNew ("user:/a/d", KEY_END),
		      keyNew ("user:/a/x/a", KEY_END), keyNew ("user:/a/x/b", KEY_END), keyNew ("user:/a/x/c", KEY_END),
		      keyNew ("user:/a/x/c/a", KEY_END), keyNew ("user:/a/x/c/b", KEY_END), KS_END);
}

static void test_below (void)
{
	printf ("Testing ksBelow\n");

	KeySet * orig;
	ssize_t origSize;
	Key * cutpoint;
	KeySet * result;
	KeySet * real_orig;

	orig = ksNew (0, KS_END);
	cutpoint = keyNew ("user:/b", KEY_END);

	succeed_if (ksBelow (0, cutpoint) == 0, "No Error on NULL pointer");
	succeed_if (ksBelow (orig, 0) == 0, "No Error on NULL pointer");

	result = ksBelow (orig, cutpoint);
	succeed_if (result, "result is null");
	succeed_if (ksGetSize (result) == 0, "result not empty");
	succeed_if (ksGetSize (orig) == 0, "orig size changed");
	ksDel (orig);
	ksDel (result);
	keyDel (cutpoint);

	orig = set_oa ();
	origSize = ksGetSize (orig);
	cutpoint = keyNew ("user:/a", KEY_END);
	result = ksBelow (orig, cutpoint);
	succeed_if (ksGetSize (orig) == origSize, "orig size changed");
	real_orig = set_oa ();
	compare_keyset (result, real_orig);
	ksDel (orig);
	ksDel (result);
	ksDel (real_orig);
	keyDel (cutpoint);

	KeySet * cmp_orig[16];
	KeySet * cmp_result[16];
#include "../data/data_cut.c"

	for (int i = 0; i < 16; ++i)
	{
		orig = set_a ();
		KeySet * origCopy = set_a ();
		origSize = ksGetSize (orig);
		cutpoint = keyDup (ksAtCursor (orig, i), KEY_CP_ALL);
		result = ksBelow (orig, cutpoint);

		compare_keyset (result, cmp_result[i]);
		compare_keyset (orig, origCopy);

		/*
		Key *key;
		printf ("orig[%d] = ksNew (%zd, ", i, ksGetSize(orig));
		ksRewind(orig); while ((key=ksNext(orig))!= 0) printf ("keyNew (\"%s\", KEY_END), ", keyName(key));
		printf ("KS_END);\n");

		printf ("result[%d] = ksNew (%zd, ", i, ksGetSize(result));
		ksRewind(result); while ((key=ksNext(result))!= 0) printf ("keyNew (\"%s\", KEY_END), ", keyName(key));
		printf ("KS_END);\n");
		*/

		keyDel (cutpoint);
		ksDel (result);
		ksDel (orig);
		ksDel (origCopy);
		ksDel (cmp_orig[i]);
		ksDel (cmp_result[i]);
	}
}


static void test_cut (void)
{
	printf ("Testing operation cut\n");

	KeySet * orig;
	Key * cutpoint;
	KeySet * result;
	KeySet * real_orig;

	orig = ksNew (0, KS_END);
	cutpoint = keyNew ("user:/b", KEY_END);

	succeed_if (ksCut (0, cutpoint) == 0, "No Error on NULL pointer");
	succeed_if (ksCut (orig, 0) == 0, "No Error on NULL pointer");

	result = ksCut (orig, cutpoint);
	succeed_if (result, "result is null");
	succeed_if (ksGetSize (result) == 0, "result not empty");
	ksDel (orig);
	ksDel (result);
	keyDel (cutpoint);

	orig = set_oa ();
	cutpoint = keyNew ("user:/a", KEY_END);
	result = ksCut (orig, cutpoint);
	succeed_if (ksGetSize (orig) == 0, "orig not empty");
	real_orig = set_oa ();
	compare_keyset (result, real_orig);
	ksDel (orig);
	ksDel (result);
	ksDel (real_orig);
	keyDel (cutpoint);


	KeySet * cmp_orig[16];
	KeySet * cmp_result[16];
#include "../data/data_cut.c"

	for (int i = 0; i < 16; ++i)
	{
		orig = set_a ();
		cutpoint = keyDup (ksAtCursor (orig, i), KEY_CP_ALL);
		result = ksCut (orig, cutpoint);

		compare_keyset (result, cmp_result[i]);
		compare_keyset (orig, cmp_orig[i]);

		/*
		Key *key;
		printf ("orig[%d] = ksNew (%zd, ", i, ksGetSize(orig));
		ksRewind(orig); while ((key=ksNext(orig))!= 0) printf ("keyNew (\"%s\", KEY_END), ", keyName(key));
		printf ("KS_END);\n");

		printf ("result[%d] = ksNew (%zd, ", i, ksGetSize(result));
		ksRewind(result); while ((key=ksNext(result))!= 0) printf ("keyNew (\"%s\", KEY_END), ", keyName(key));
		printf ("KS_END);\n");
		*/

		keyDel (cutpoint);
		ksDel (result);
		ksDel (orig);
		ksDel (cmp_orig[i]);
		ksDel (cmp_result[i]);
	}
}

static void test_cutpoint (void)
{
	printf ("Testing operation cut point\n");

	Key * cutpoint = keyNew ("user:/a/b/c", KEY_END);
	KeySet * orig =
		ksNew (30, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), cutpoint, keyNew ("user:/a/b/c/d", KEY_END),
		       keyNew ("user:/a/b/c/d/e", KEY_END), keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	ksRewind (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a");
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a/b");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a/b");

	KeySet * cmp_orig = ksNew (15, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part = ksNew (15, cutpoint, keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END),
				   keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
}

static void test_cascadingCutpoint (void)
{
	printf ("Testing operation cascading cut point\n");

	Key * cutpoint = keyNew ("/a/b/c", KEY_END);
	KeySet * orig =
#include "../data/data_nscut.c"
		ksRewind (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "spec:/a");
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "spec:/a/b");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if_same_string (keyName (ksCurrent (orig)), "spec:/a/b");

	KeySet * cmp_orig = ksNew (15, keyNew ("spec:/a", KEY_END), keyNew ("spec:/a/b", KEY_END),

				   keyNew ("proc:/a", KEY_END), keyNew ("proc:/a/b", KEY_END),

				   keyNew ("dir:/a", KEY_END), keyNew ("dir:/a/b", KEY_END),

				   keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END),

				   keyNew ("system:/a", KEY_END), keyNew ("system:/a/b", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	// output_keyset(orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part =
		ksNew (25, keyNew ("spec:/a/b/c", KEY_END), keyNew ("spec:/a/b/c/d", KEY_END), keyNew ("spec:/a/b/c/d/e", KEY_END),
		       keyNew ("spec:/a/b/c/e", KEY_END), keyNew ("spec:/a/b/c/e/d", KEY_END),

		       keyNew ("proc:/a/b/c", KEY_END), keyNew ("proc:/a/b/c/d", KEY_END), keyNew ("proc:/a/b/c/d/e", KEY_END),
		       keyNew ("proc:/a/b/c/e", KEY_END), keyNew ("proc:/a/b/c/e/d", KEY_END),

		       keyNew ("dir:/a/b/c", KEY_END), keyNew ("dir:/a/b/c/d", KEY_END), keyNew ("dir:/a/b/c/d/e", KEY_END),
		       keyNew ("dir:/a/b/c/e", KEY_END), keyNew ("dir:/a/b/c/e/d", KEY_END),

		       keyNew ("user:/a/b/c", KEY_END), keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END),
		       keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END),

		       keyNew ("system:/a/b/c", KEY_END), keyNew ("system:/a/b/c/d", KEY_END), keyNew ("system:/a/b/c/d/e", KEY_END),
		       keyNew ("system:/a/b/c/e", KEY_END), keyNew ("system:/a/b/c/e/d", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	// output_keyset(part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

static void test_cascadingRootCutpoint (void)
{
	printf ("Testing operation cascading root cut point\n");

	Key * cutpoint = keyNew ("/", KEY_END);
	KeySet * orig =
#include "../data/data_nscut.c"
		ksRewind (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "spec:/a");
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "spec:/a/b");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if (ksGetSize (orig) == 0, "keyset not empty");
	succeed_if (ksCurrent (orig) == 0, "empty keyset not rewinded");
	ksDel (orig);

	KeySet * cmp_part =
#include "../data/data_nscut.c"
		compare_keyset (part, cmp_part);
	// output_keyset(part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

static void test_cutpointRoot (void)
{
	printf ("Testing operation cut root point\n");

	Key * cutpoint = keyNew ("user:/", KEY_END);
	KeySet * orig = ksNew (30, keyNew ("dir:/a", KEY_END), keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END),
			       keyNew ("user:/a/b/c", KEY_END), keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END),
			       keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	ksRewind (orig);
	ksNext (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if_same_string (keyName (ksCurrent (orig)), "dir:/a");

	KeySet * cmp_orig = ksNew (15, keyNew ("dir:/a", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part = ksNew (15, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), keyNew ("user:/a/b/c", KEY_END),
				   keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END),
				   keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}


static void test_cutpoint_1 (void)
{
	printf ("Testing operation cut point 1\n");

	Key * cutpoint = keyNew ("user:/a/b/c", KEY_END);
	KeySet * orig =
		ksNew (30, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), cutpoint, keyNew ("user:/a/b/c/d", KEY_END),
		       keyNew ("user:/a/b/c/d/e", KEY_END), keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	ksRewind (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a");
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a/b");
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a/b/c");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/a/b");

	KeySet * cmp_orig = ksNew (15, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part = ksNew (15, cutpoint, keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END),
				   keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
}

static void test_unique_cutpoint (void)
{
	printf ("Testing operation cut with unique cutpoint\n");

	Key * cutpoint = keyNew ("user:/a/b/c", KEY_END);
	KeySet * orig = ksNew (30, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), keyNew ("user:/a/b/c", KEY_END),
			       keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END), keyNew ("user:/a/b/c/e", KEY_END),
			       keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);

	KeySet * part = ksCut (orig, cutpoint);

	KeySet * cmp_orig = ksNew (15, keyNew ("user:/a", KEY_END), keyNew ("user:/a/b", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part =
		ksNew (15, keyNew ("user:/a/b/c", KEY_END), keyNew ("user:/a/b/c/d", KEY_END), keyNew ("user:/a/b/c/d/e", KEY_END),
		       keyNew ("user:/a/b/c/e", KEY_END), keyNew ("user:/a/b/c/e/d", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

static void test_cutbelow (void)
{
	printf ("Testing cutting below some keys\n");

	Key * cutpoint = keyNew ("user:/export", KEY_END);
	KeySet * orig =
		ksNew (30, keyNew ("user:/export/a", KEY_END), keyNew ("user:/export/c", KEY_END), keyNew ("user:/export/c/x", KEY_END),
		       keyNew ("user:/export/c/x/b/blah", KEY_END), keyNew ("user:/export/xyz", KEY_END),
		       keyNew ("user:/export-backup/b", KEY_END), keyNew ("user:/export-backup-2/x", KEY_END), KS_END);
	ksRewind (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/export/a");
	ksLookupByName (orig, "user:/export-backup/b", 0);
	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/export-backup/b");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if_same_string (keyName (ksCurrent (orig)), "user:/export-backup/b");

	KeySet * cmp_orig = ksNew (15, keyNew ("user:/export-backup-2/x", KEY_END), keyNew ("user:/export-backup/b", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part =
		ksNew (15, keyNew ("user:/export/a", KEY_END), keyNew ("user:/export/c", KEY_END), keyNew ("user:/export/c/x", KEY_END),
		       keyNew ("user:/export/c/x/b/blah", KEY_END), keyNew ("user:/export/xyz", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

static void test_cascading_cutbelow (void)
{
	printf ("Testing cutting below some keys (cascading)\n");

	Key * cutpoint = keyNew ("/export", KEY_END);
	KeySet * orig = ksNew (30, keyNew ("/export/a", KEY_END), keyNew ("/export/c", KEY_END), keyNew ("/export/c/x", KEY_END),
			       keyNew ("/export/c/x/b/blah", KEY_END), keyNew ("/export/xyz", KEY_END),
			       keyNew ("/export-backup/b", KEY_END), keyNew ("/export-backup-2/x", KEY_END), KS_END);
	ksRewind (orig);
	ksNext (orig);
	succeed_if_same_string (keyName (ksCurrent (orig)), "/export/a");
	ksLookupByName (orig, "/export-backup/b", 0);
	succeed_if_same_string (keyName (ksCurrent (orig)), "/export-backup/b");

	KeySet * part = ksCut (orig, cutpoint);

	succeed_if_same_string (keyName (ksCurrent (orig)), "/export-backup/b");

	KeySet * cmp_orig = ksNew (15, keyNew ("/export-backup-2/x", KEY_END), keyNew ("/export-backup/b", KEY_END), KS_END);
	compare_keyset (orig, cmp_orig);
	ksDel (orig);
	ksDel (cmp_orig);

	KeySet * cmp_part = ksNew (15, keyNew ("/export/a", KEY_END), keyNew ("/export/c", KEY_END), keyNew ("/export/c/x", KEY_END),
				   keyNew ("/export/c/x/b/blah", KEY_END), keyNew ("/export/xyz", KEY_END), KS_END);
	compare_keyset (part, cmp_part);
	ksDel (part);
	ksDel (cmp_part);
	keyDel (cutpoint);
}

KeySet * set_simple (void)
{
	return ksNew (50, keyNew ("system:/elektra/mountpoints/simple", KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/more", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/more/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/more/config/below", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/config/path", KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/getplugins", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/more", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config/below", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/path", KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user:/tests/backend/simple", KEY_END),

		      keyNew ("system:/elektra/mountpoints/simple/setplugins", KEY_END),
		      keyNew ("system:/elektra/mountpoints/simple/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END), KS_END);
}

static void test_simple (void)
{
	KeySet * config = set_simple ();
	KeySet * result_res = ksNew (16, keyNew ("system:/elektra/mountpoints/simple/config", KEY_END),
				     keyNew ("system:/elektra/mountpoints/simple/config/anything", KEY_VALUE, "backend", KEY_END),
				     keyNew ("system:/elektra/mountpoints/simple/config/more", KEY_END),
				     keyNew ("system:/elektra/mountpoints/simple/config/more/config", KEY_END),
				     keyNew ("system:/elektra/mountpoints/simple/config/more/config/below", KEY_END),
				     keyNew ("system:/elektra/mountpoints/simple/config/path", KEY_END), KS_END);
	KeySet * result_config =
		ksNew (22, keyNew ("system:/elektra/mountpoints/simple", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer", KEY_VALUE, "tracer", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/anything", KEY_VALUE, "plugin", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/more", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/more/config/below", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/getplugins/#1tracer/config/path", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/mountpoint", KEY_VALUE, "user:/tests/backend/simple", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/setplugins", KEY_END),
		       keyNew ("system:/elektra/mountpoints/simple/setplugins/#1tracer", KEY_VALUE, "tracer", KEY_END), KS_END);
	Key * key = ksLookup (config, keyNew ("system:/elektra/mountpoints/simple/config", KEY_END), KDB_O_DEL);
	succeed_if (ksGetCursor (config) == 1, "cursor not set correctly");
	KeySet * res = ksCut (config, key);
	succeed_if (ksGetCursor (config) == 0, "cursor should stay as is");
	compare_keyset (config, result_config);
	compare_keyset (res, result_res);

	ksDel (result_config);
	ksDel (result_res);
	ksDel (res);
	ksDel (config);
}

static void test_cursor (void)
{
	printf ("test cut cursor\n");

	KeySet * config = set_simple ();

	succeed_if (ksGetCursor (0) == -1, "No error on NULL pointer");

	ksRewind (config);
	succeed_if (ksGetCursor (config) == -1, "should be invalid cursor");
	succeed_if (ksNext (config) != 0, "should be root key");
	succeed_if (ksGetCursor (config) == 0, "cursor on first position");
	succeed_if_same_string (keyName (ksCurrent (config)), "system:/elektra/mountpoints/simple");
	succeed_if (ksNext (config) != 0, "should be on config");
	succeed_if (ksGetCursor (config) == 1, "cursor on config");
	succeed_if_same_string (keyName (ksCurrent (config)), "system:/elektra/mountpoints/simple/config");

	KeySet * res = ksCut (config, ksCurrent (config));
	succeed_if (ksGetCursor (config) == 0, "cursor on first position");
	succeed_if_same_string (keyName (ksCurrent (config)), "system:/elektra/mountpoints/simple");

	succeed_if (ksNext (config) != 0, "should be on config");
	succeed_if (ksGetCursor (config) == 1, "cursor on getplugins");
	succeed_if_same_string (keyName (ksCurrent (config)), "system:/elektra/mountpoints/simple/getplugins");

	KeySet * getplugins = ksCut (config, ksCurrent (config));
	succeed_if (ksGetCursor (getplugins) == -1, "should be invalid cursor");
	succeed_if (ksNext (getplugins) != 0, "should be root key");
	succeed_if (ksGetCursor (getplugins) == 0, "cursor on first position");

	succeed_if (ksNext (getplugins) != 0, "should be tracer");
	succeed_if (ksGetCursor (getplugins) == 1, "cursor not correct");

	KeySet * gettracer = ksCut (getplugins, ksCurrent (getplugins));
	succeed_if (ksNext (getplugins) == 0, "should be no more getplugins");

	succeed_if (ksNext (config) != 0, "next did not work");
	succeed_if (ksGetCursor (config) == 1, "cursor not correct");
	succeed_if_same_string (keyName (ksCurrent (config)), "system:/elektra/mountpoints/simple/mountpoint");

	succeed_if (ksNext (config) != 0, "next did not work");
	succeed_if (ksGetCursor (config) == 2, "cursor not correct");
	succeed_if_same_string (keyName (ksCurrent (config)), "system:/elektra/mountpoints/simple/setplugins");

	KeySet * setplugins = ksCut (config, ksCurrent (config));
	succeed_if (ksNext (config) == 0, "should be no more config");
	succeed_if (ksNext (setplugins) != 0, "ksnext did not work");
	succeed_if_same_string (keyName (ksCurrent (setplugins)), "system:/elektra/mountpoints/simple/setplugins");
	succeed_if (ksNext (setplugins) != 0, "ksnext did not work");

	KeySet * settracer = ksCut (setplugins, ksCurrent (setplugins));
	succeed_if (ksNext (setplugins) == 0, "should be no more setplugins");
	succeed_if (ksGetSize (settracer) == 1, "should be only one key");

	succeed_if (ksGetSize (config) == 2, "should be only three keys remaining: root, mountpoint");

	succeed_if (ksSetCursor (0, 1) == -1, "No error on NULL keyset");
	succeed_if (ksSetCursor (config, -1) == 0, "No error on invalid position");
	succeed_if (ksSetCursor (config, 2) == 1, "Can not set cursor to KeySet");

	ksDel (setplugins);
	ksDel (getplugins);
	ksDel (settracer);
	ksDel (gettracer);
	ksDel (config);
	ksDel (res);
}

static void test_morecut (void)
{
	printf ("More cut test cases\n");

	KeySet * ks = ksNew (5, keyNew ("user:/valid/key1", KEY_END), keyNew ("user:/valid/key2", KEY_END),
			     keyNew ("system:/valid/key1", KEY_END), keyNew ("system:/valid/key2", KEY_END), KS_END);
	succeed_if (ksCurrent (ks) == 0, "should be rewinded");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/valid/key1");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/valid/key2");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "system:/valid/key1");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "system:/valid/key2");

	KeySet * split1 = ksNew (3, keyNew ("user:/valid/key1", KEY_END), keyNew ("user:/valid/key2", KEY_END), KS_END);
	KeySet * split2 = ksNew (3, keyNew ("system:/valid/key1", KEY_END), keyNew ("system:/valid/key2", KEY_END), KS_END);

	Key * userKey = keyNew ("user:/", KEY_END);

	KeySet * cut = ksCut (ks, userKey);

	compare_keyset (cut, split1);
	compare_keyset (ks, split2);
	ksDel (cut);

	keyDel (userKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
}

static void test_cutafter (void)
{
	printf ("More cut after\n");

	KeySet * ks = ksNew (5, keyNew ("user:/a/valid/key", KEY_END), keyNew ("user:/a/x/valid/key", KEY_END),
			     keyNew ("user:/b/valid/key", KEY_END), keyNew ("user:/b/x/valid/key", KEY_END),
			     keyNew ("user:/c/valid/key", KEY_END), keyNew ("user:/c/x/valid/key", KEY_END), KS_END);
	ksRewind (ks);
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/a/valid/key");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/a/x/valid/key");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/b/valid/key");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/b/x/valid/key");
	ksNext (ks);
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/c/valid/key");
	// printf ("%s\n", keyName(ksCurrent(ks)));

	KeySet * split1 = ksNew (8, keyNew ("user:/b/valid/key", KEY_END), keyNew ("user:/b/x/valid/key", KEY_END), KS_END);
	KeySet * split2 = ksNew (8, keyNew ("user:/a/valid/key", KEY_END), keyNew ("user:/a/x/valid/key", KEY_END),
				 keyNew ("user:/c/valid/key", KEY_END), keyNew ("user:/c/x/valid/key", KEY_END), KS_END);

	Key * userKey = keyNew ("user:/b", KEY_END);

	KeySet * cut = ksCut (ks, userKey);
	// printf ("%s\n", keyName(ksCurrent(ks)));
	succeed_if_same_string (keyName (ksCurrent (ks)), "user:/c/valid/key");

	compare_keyset (cut, split1);
	compare_keyset (ks, split2);
	ksDel (cut);

	keyDel (userKey);

	ksDel (ks);
	ksDel (split1);
	ksDel (split2);
}

static void test_simpleLookup (void)
{
	printf ("Test simple lookup\n");

	KeySet * ks = ksNew (10, KS_END);

	Key * searchKey = keyNew ("user:/something", KEY_VALUE, "a value", KEY_END);
	Key * k0 = ksLookup (ks, searchKey, 0);
	succeed_if (!k0, "we have a problem: found not inserted key");

	Key * dup = keyDup (searchKey, KEY_CP_ALL);
	succeed_if_same_string (keyName (dup), "user:/something");
	succeed_if_same_string (keyString (dup), "a value");
	ksAppendKey (ks, dup);
	// output_keyset(ks);

	Key * k1 = ksLookup (ks, searchKey, 0);
	succeed_if (k1, "we have a problem: did not find key");
	succeed_if (k1 != searchKey, "same key, even though dup was used");
	succeed_if_same_string (keyName (k1), "user:/something");
	succeed_if_same_string (keyString (k1), "a value");

	Key * returnedKey;
	returnedKey = ksLookup (ks, searchKey, KDB_O_DEL);
	succeed_if_same_string (keyName (returnedKey), keyName (dup));
	succeed_if_same_string (keyString (returnedKey), keyString (dup));
	succeed_if (ksGetSize (ks) == 1, "key deleted from keyset");

	ksDel (ks);
}

static void test_nsLookup (void)
{
	printf ("Test lookup in all namespaces\n");

	KeySet * ks =
#include "../data/data_ns.c"

		for (int i = 0; i < NUMBER_OF_NAMESPACES; ++i)
	{
		Key * searchKey = keyNew (namespaces[i], KEY_VALUE, "value1", KEY_META, "comment/#0", "comment1", KEY_END);
		keyAddName (searchKey, "test/keyset/dir7/key1");

		Key * lookupKey = keyNew (namespaces[i], KEY_END);
		keyAddName (lookupKey, "something/not/found");
		Key * k0 = ksLookup (ks, lookupKey, 0);
		succeed_if (!k0, "we have a problem: found not inserted key");

		keySetName (lookupKey, namespaces[i]);
		keyAddName (lookupKey, "test/keyset/dir7/key1");
		Key * k1 = ksLookup (ks, lookupKey, 0);
		compare_key (k1, searchKey);

		keySetName (lookupKey, "/test/keyset/dir7/key1");
		if (strcmp (namespaces[i], "spec:/") == 0)
		{
			keySetName (searchKey, "proc:/");
			keyAddName (searchKey, "test/keyset/dir7/key1");
			Key * k2 = ksLookup (ks, lookupKey, 0);
			compare_key (k2, searchKey);
		}
		else
		{
			Key * k2 = ksLookup (ks, lookupKey, 0);
			compare_key (k2, searchKey);
		}

		keySetName (lookupKey, namespaces[i]);
		ksDel (ksCut (ks, lookupKey));

		keySetName (lookupKey, namespaces[i]);
		keyAddName (lookupKey, "test/keyset/dir7/key1");
		Key * k3 = ksLookup (ks, lookupKey, 0);
		succeed_if (!k3, "we have a problem: found key cut out");

		keyDel (lookupKey);
		keyDel (searchKey);
	}
	ksDel (ks);
}

static void test_ksAppend2 (void)
{
	printf ("Test more involved appending\n");

	Key * inks = keyNew ("user:/key_with_meta_data", KEY_END);
	KeySet * ks = ksNew (0, KS_END);
	ksAppendKey (ks, inks);

	succeed_if (keyGetMeta (inks, "hello") == 0, "hello was not set up to now");
	succeed_if (keyGetMeta (inks, "error") == 0, "hello was not set up to now");

	keySetMeta (inks, "hello", "hello_world");
	succeed_if_same_string (keyValue (keyGetMeta (inks, "hello")), "hello_world");
	succeed_if (keyGetMeta (inks, "error") == 0, "hello was not set up to now");

	KeySet * ks2 = ksDup (ks);
	ksRewind (ks2);
	ksNext (ks2);
	succeed_if_same_string (keyValue (keyGetMeta (ksCurrent (ks2), "hello")), "hello_world");
	succeed_if (keyGetMeta (ksCurrent (ks2), "error") == 0, "hello was not set up to now");

	Key * dup = keyDup (inks, KEY_CP_ALL);
	succeed_if_same_string (keyValue (keyGetMeta (inks, "hello")), "hello_world");
	succeed_if (keyGetMeta (inks, "error") == 0, "hello was not set up to now");

	succeed_if_same_string (keyValue (keyGetMeta (dup, "hello")), "hello_world");
	succeed_if (keyGetMeta (dup, "error") == 0, "hello was not set up to now");

	keySetMeta (inks, "error", "some error information");
	succeed_if_same_string (keyValue (keyGetMeta (inks, "hello")), "hello_world");
	succeed_if_same_string (keyValue (keyGetMeta (inks, "error")), "some error information");

	succeed_if_same_string (keyValue (keyGetMeta (dup, "hello")), "hello_world");
	succeed_if (keyGetMeta (dup, "error") == 0, "hello was not set up to now");

	ksDel (ks);
	keyDel (dup);
	ksDel (ks2);

	Key * parent = keyNew ("user:/test/rename", KEY_END);
	succeed_if (keyGetRef (parent) == 0, "ref wrong");
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, parent);
	succeed_if (keyGetRef (parent) == 1, "ref wrong");
	KeySet * iter = ksDup (ks);
	// COW - key reference stays same after ksDup
	succeed_if (keyGetRef (parent) == 1, "ref wrong");
	ksRewind (iter);
	Key * key = ksNext (iter);
	succeed_if (keyGetMeta (key, "name") == 0, "no such meta exists");
	Key * result = keyDup (key, KEY_CP_ALL);
	succeed_if (keyGetRef (parent) == 1, "ref wrong");
	succeed_if (keyGetRef (result) == 0, "ref wrong");
	keySetName (result, keyName (parent));
	keyAddBaseName (result, "cut");
	Key * lok = ksLookup (ks, key, KDB_O_POP);
	keyDel (lok);
	succeed_if (keyGetRef (parent) == 1, "ref wrong");
	succeed_if (keyGetRef (key) == 1, "ref wrong");
	succeed_if (keyGetRef (result) == 0, "ref wrong");
	ksAppendKey (ks, result);
	succeed_if (keyGetRef (parent) == 1, "ref wrong");
	succeed_if (keyGetRef (key) == 1, "ref wrong");
	succeed_if (keyGetRef (result) == 1, "ref wrong");
	keyDel (result);
	keyDel (key);
	ksDel (iter);
	// parent+key removed!
	succeed_if (ksLookupByName (ks, "user:/test/rename/cut", 0) != 0, "did not find key");
	succeed_if (ksGetSize (ks) == 1, "only result in it") ksDel (ks);

	parent = keyNew ("user:/test/rename", KEY_END);
	ks = ksNew (0, KS_END);
	ksAppendKey (ks, parent);
	Key * lk = ksLookup (ks, parent, KDB_O_POP);
	keyDel (lk);
	ksDel (ks);
}

static void test_ksAppend3 (void)
{
	printf ("Test appending same key\n");

	Key * key = keyNew ("user:/key", KEY_END);
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (ksAppendKey (ks, key) == 1, "could not append key");
	succeed_if (ksLookupByName (ks, "user:/key", 0) == key, "did not find key");
	succeed_if (ksAppendKey (ks, key) == 1, "could not append key");
	succeed_if (ksLookupByName (ks, "user:/key", 0) == key, "did not find key again");

	ksDel (ks);
}


int main (int argc, char ** argv)
{
	printf ("KEYSET ABI   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_ksNew ();
	test_ksEmpty ();
	test_ksReference ();
	test_ksDup ();
	test_ksCopy ();
	test_ksIterate ();
	test_ksCursor ();
	test_ksAtCursor ();
	test_ksSort ();
	test_ksLookup ();
	test_ksLookupByName ();
#ifndef __SANITIZE_ADDRESS__
	test_ksLookupName ();
#endif
	test_ksLookupNameCascading ();
	test_ksExample ();
	test_ksAppend ();
	test_ksFunctional ();
#ifndef __SANITIZE_ADDRESS__
	test_ksLookupPop ();
#endif
	test_ksDoubleFree ();
	test_ksDoubleAppend ();
	test_ksDoubleAppendKey ();
	test_ksAppendKey ();
	test_keyVNew ();
	test_ksModifyKey ();
	test_cut ();
	test_below ();
	test_cutpoint ();
	test_cascadingCutpoint ();
	test_cascadingRootCutpoint ();
	test_cutpoint_1 ();
	test_cutpointRoot ();
	test_unique_cutpoint ();
	test_cutbelow ();
	test_cascading_cutbelow ();
	test_simple ();
	test_cursor ();
	test_morecut ();
	test_cutafter ();
	test_ksOrder ();
	test_simpleLookup ();
	test_nsLookup ();
	test_ksAppend2 ();
	test_ksAppend3 ();
	test_ksOrderNs ();

	printf ("\ntestabi_ks RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
