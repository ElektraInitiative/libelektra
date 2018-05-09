#include <kdbprivate.h>
/**
 * @file
 *
 * @brief Tests for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdbprivate.h>

#include <tests_plugin.h>

#include "mmapstorage.h"

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define KEY_NAME_LENGTH 1000
#define NUM_DIR 10
#define NUM_KEY 10

#define TEST_ROOT_KEY "user/tests/mmapstorage"

/* -- KeySet test data ------------------------------------------------------------------------------------------------------------------ */

static KeySet * simpleTestKeySet (void)
{
	return ksNew (10, keyNew ("user/tests/mmapstorage/simpleKey", KEY_VALUE, "root key", KEY_END),
		      keyNew ("user/tests/mmapstorage/simpleKey/a", KEY_VALUE, "a value", KEY_END),
		      keyNew ("user/tests/mmapstorage/simpleKey/b", KEY_VALUE, "b value", KEY_END), KS_END);
}

static KeySet * metaTestKeySet (void)
{
	return ksNew (10,
		      keyNew ("user/tests/mmapstorage", KEY_VALUE, "root key", KEY_META, "a",
			      "b aksdjfh aksjdhf aklsjdhf aksljdhf aklsjdhf aksljdhf ", KEY_END),
		      keyNew ("user/tests/mmapstorage/a", KEY_VALUE, "a value", KEY_META, "ab",
			      "cd oiahsdkfhga sdjkfhgsuzdgf kashgdf aszgdf uashdf ", KEY_END),
		      keyNew ("user/tests/mmapstorage/b", KEY_VALUE, "b value", KEY_META, "longer val",
			      "here some even more with ugly €@\\1¹²³¼ chars", KEY_END),
		      KS_END);
}

static KeySet * largeTestKeySet (void)
{
	int i, j;
	char name[KEY_NAME_LENGTH + 1];
	char value[] = "data";
	KeySet * large = ksNew (NUM_KEY * NUM_DIR, KS_END);

	for (i = 0; i < NUM_DIR; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", TEST_ROOT_KEY, "dir", i);
		ksAppendKey (large, keyNew (name, KEY_VALUE, value, KEY_END));
		for (j = 0; j < NUM_KEY; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", TEST_ROOT_KEY, "dir", i, "key", j);
			ksAppendKey (large, keyNew (name, KEY_VALUE, value, KEY_END));
		}
	}
	return large;
}

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_mmap_get_set (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	KeySet * toAppend = simpleTestKeySet ();
	ksAppend (ks, toAppend);
	ksDel (toAppend);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset (expected, ks);
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_set_get (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	KeySet * expected = simpleTestKeySet ();
	compare_keyset (expected, returned);

	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_after_reopen (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset (expected, returned);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_set_get_large_keyset (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = largeTestKeySet ();
	KeySet * expected = ksDeepDup (ks);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	compare_keyset (expected, returned);

	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_ks_copy (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	ksDel (returned);

	returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset (expected, returned);

	KeySet * copiedKs = ksNew (0, KS_END);
	ksCopy (copiedKs, returned);
	compare_keyset (expected, copiedKs);

	ksDel (copiedKs);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_empty_after_clear (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	succeed_if (ksGetSize (returned) == 0, "KeySet not empty after clear (or nullptr)");

	ksDel (returned);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_meta (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = metaTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset (expected, returned);

	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_meta_get_after_reopen (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset (expected, ks);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_metacopy (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = metaTestKeySet ();

	Key * shareMeta = keyNew (0);
	keySetMeta (shareMeta, "sharedmeta", "shared meta key test");

	Key * current;
	ksRewind (ks);
	while ((current = ksNext (ks)) != 0)
	{
		keyCopyMeta (current, shareMeta, "sharedmeta");
	}
	KeySet * expected = ksDeepDup (ks);


	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * returned = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");


	compare_keyset (expected, returned);

	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	keyDel (shareMeta);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ks_copy_with_meta (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);

	KeySet * returned = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = metaTestKeySet ();
	compare_keyset (expected, returned);

	KeySet * copiedKs = ksNew (0, KS_END);
	ksCopy (copiedKs, returned);
	compare_keyset (expected, copiedKs);

	ksDel (copiedKs);
	ksDel (expected);
	ksDel (returned);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void clearStorage (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

#ifdef DEBUG
int cmpfunc (const void * a, const void * b)
{
	return (*(int *) a - *(int *) b);
}

static void testDynArray1 (void)
{
	size_t testData[] = { 8466, 2651, 6624, 9575, 4628, 9361, 417,  8932, 4570, 343,  1866, 3135, 6617, 344,  9419, 2094, 5623,
			      4920, 2209, 8037, 8437, 7955, 5575, 8355, 1133, 6527, 8543, 3338, 1772, 2278, 7446, 8834, 7728, 665,
			      8519, 6079, 5060, 7429, 3843, 6923, 4073, 2245, 2784, 6620, 2887, 8497, 9360, 5752, 3195, 538,  1491,
			      8087, 8378, 5746, 4961, 5499, 8050, 2138, 1196, 1860, 4372, 6553, 4530, 8828, 4017, 9934, 3,    6274,
			      4405, 5021, 3416, 854,  4635, 9902, 5383, 7947, 5210, 8242, 1928, 3792, 7234, 759,  6571, 9514, 8451,
			      918,  9958, 1577, 96,   8644, 6815, 5584, 8585, 1252, 808,  5695, 910,  4157, 701,  77 };

	DynArray dynArray;
	dynArray.keyArray = calloc (100, sizeof (Key *));
	dynArray.size = 0;
	dynArray.alloc = 100;

	for (size_t i = 0; i < 100; ++i)
	{
		findOrInsert ((Key *) testData[i], &dynArray);
	}

	qsort (testData, 100, sizeof (size_t), cmpfunc);

	int error = 0;
	for (size_t i = 0; i < 100; ++i)
	{
		if (testData[i] != (size_t) dynArray.keyArray[i])
		{
			++error;
		}
	}

	succeed_if (error == 0, "dynArray does not sort array properly");

	elektraFree (dynArray.keyArray);
}
#endif

/* -- KeySet operation tests ------------------------------------------------------------------------------------------------------------ */

static void test_mmap_ksDupFun (const char * tmpFile, KeySet * copyFunction (const KeySet * source))
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if ((ks->flags & KS_FLAG_MMAP_ARRAY) == KS_FLAG_MMAP_ARRAY, "KeySet array not in mmap");

	KeySet * dupKs = copyFunction (ks);
	compare_keyset (dupKs, ks);
	compare_keyset (ks, dupKs);

	ksDel (dupKs);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksCopy (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if ((ks->flags & KS_FLAG_MMAP_ARRAY) == KS_FLAG_MMAP_ARRAY, "KeySet array not in mmap");

	KeySet * copyKs = ksNew (0, KS_END);
	if (ksCopy (copyKs, ks) == 1)
	{
		compare_keyset (copyKs, ks);
		compare_keyset (ks, copyKs);
	}
	else
	{
		yield_error ("ksCopy failed");
	}

	ksDel (copyKs);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksPop_ksNeedSync (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	Key * lastKey = ksPop (ks);
	succeed_if (ksNeedSync (ks) == 1, "ksNeedSync was not 1, even though ks needs sync");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if (ksNeedSync (ks) == 0, "ksNeedSync was 1, even though ks needs no sync");

	keyDel (lastKey);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksGetSize (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	ssize_t origSize = ksGetSize (ks);
	succeed_if (origSize > 0, "ks was empty before kdbSet");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ssize_t returnedSize = ksGetSize (ks);
	succeed_if (origSize == returnedSize, "ksGetSize before and after kdbSet, kdbGet differ");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksAppendKey (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	ssize_t origSize = ksGetSize (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * toAppend = keyNew ("user/tests/mmapstorage/simpleKey/c", KEY_VALUE, "c value", KEY_END);
	if (ksAppendKey (ks, toAppend) == -1)
	{
		yield_error ("ksAppendKey failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ssize_t returnedSize = ksGetSize (ks);

	succeed_if (returnedSize == (origSize + 1), "ksGetSize after append should be incremented");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


/* -- Key operation tests --------------------------------------------------------------------------------------------------------------- */
/* -- Key name operation tests ---------------------------------------------------------------------------------------------------------- */
/* -- Key value operation tests --------------------------------------------------------------------------------------------------------- */


/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

#ifdef DEBUG
	testDynArray1 ();
#endif

	const char * tmpFile = elektraFilename ();

	test_mmap_get_set (tmpFile);
	clearStorage (tmpFile);

	test_mmap_set_get (tmpFile);
	test_mmap_get_after_reopen (tmpFile);
	test_mmap_set_get_large_keyset (tmpFile);
	test_mmap_ks_copy (tmpFile);

	clearStorage (tmpFile);

	test_mmap_empty_after_clear (tmpFile);

	test_mmap_meta (tmpFile);
	test_mmap_meta_get_after_reopen (tmpFile);

	test_mmap_metacopy (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ks_copy_with_meta (tmpFile);

	/* KeySet operation tests */
	clearStorage (tmpFile);
	test_mmap_ksDupFun (tmpFile, ksDup);

	clearStorage (tmpFile);
	test_mmap_ksDupFun (tmpFile, ksDeepDup);

	clearStorage (tmpFile);
	test_mmap_ksCopy (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksPop_ksNeedSync (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksGetSize (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksAppendKey (tmpFile);

	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
