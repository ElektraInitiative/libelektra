/**
 * @file
 *
 * @brief Tests for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */
#define _POSIX_C_SOURCE 200112L

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <stdio.h> // fopen(), fileno()
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>  // stat()
#include <sys/types.h> // ftruncate ()
#include <unistd.h>    // ftruncate()

#include <kdbconfig.h>
#include <kdbmmap.h>
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

static void test_mmap_get_set_empty (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_set (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	ksDel (ks);
	ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * expected = simpleTestKeySet ();
	compare_keyset (expected, ks);
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_truncated_file (const char * tmpFile)
{
	// first write a mmap file
	{
		Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("mmapstorage");

		KeySet * ks = simpleTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

		keyDel (parentKey);
		ksDel (ks);
		PLUGIN_CLOSE ();
	}

	// now truncate the mmap file by the size of the mmap footer
	FILE * fp;
	if ((fp = fopen (tmpFile, "r+")) == 0)
	{
		yield_error ("error opening file");
	}
	struct stat sbuf;
	if (stat (tmpFile, &sbuf) == -1)
	{
		yield_error ("stat error");
	}

	int fd = fileno (fp);
	if ((ftruncate (fd, sbuf.st_size - sizeof (MmapFooter))) == -1)
	{
		yield_error ("ftruncate error");
	}
	if (fp)
	{
		fclose (fp);
	}

	// truncated file should be detected by mmapstorage
	{
		// after the file was truncated, we expect an error here
		Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("mmapstorage");

		KeySet * ks = ksNew (0, KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet did not detect truncated file");

		keyDel (parentKey);
		ksDel (ks);
		PLUGIN_CLOSE ();
	}
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

static void test_mmap_opmphm (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");
	KeySet * ks = largeTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);

	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user/tests/mmapstorage/dir7/key3";
	Key * found = ksLookupByName (ks, name, KDB_O_OPMPHM);

	if (!found)
	{
		yield_error ("Key not found.")
	}

	// write keyset with OPMPHM structures
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ksDel (ks);
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

/*
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
*/

/* -- KeySet API tests ------------------------------------------------------------------------------------------------------------------ */

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

static void test_mmap_double_get (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * first = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, first, parentKey) == 1, "kdbGet was not successful");
	KeySet * second = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, second, parentKey) == 1, "kdbGet was not successful");
	succeed_if (first->array != second->array, "ks->array points to same thing");

	compare_keyset (first, ks);
	compare_keyset (ks, first);
	compare_keyset (second, ks);
	compare_keyset (ks, second);

	ksDel (ks);
	ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	KeySet * simple = simpleTestKeySet ();
	compare_keyset (first, simple);
	compare_keyset (second, simple);
	ksDel (first);
	ksDel (second);
	ksDel (simple);
	ksDel (ks);
	keyDel (parentKey);
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
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ssize_t appendSize = 0;
	Key * toAppend = keyNew ("user/my/new/key", KEY_END);

	if ((appendSize = ksAppendKey (ks, toAppend)) == -1)
	{
		yield_error ("ksAppendKey failed");
	}

	succeed_if (appendSize == (origSize + 1), "ksAppendKey after append should be incremented");
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

static void test_mmap_ksAppend (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * toAppend = ksNew (10, keyNew ("user/tests/mmapstorage/zzzz", KEY_VALUE, "root key", KEY_END),
				   keyNew ("user/tests/mmapstorage/simpleKey/c", KEY_VALUE, "c value", KEY_END),
				   keyNew ("user/tests/mmapstorage/simpleKey/d", KEY_VALUE, "d value", KEY_END), KS_END);
	if (ksAppend (ks, toAppend) == -1)
	{
		yield_error ("ksAppend failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	ksDel (ks);
	ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ksDel (toAppend);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksCut (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	// create keyset with some folder 'other' that we will then cut
	KeySet * ks = simpleTestKeySet ();
	KeySet * other = ksNew (10, keyNew ("user/tests/mmapstorage/other", KEY_VALUE, "other key", KEY_END),
				keyNew ("user/tests/mmapstorage/other/a", KEY_VALUE, "other a value", KEY_END),
				keyNew ("user/tests/mmapstorage/other/b", KEY_VALUE, "other b value", KEY_END), KS_END);
	if (ksAppend (ks, other) == -1)
	{
		yield_error ("ksAppend failed");
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	// now cut the 'other' folder
	Key * cutKey = keyNew ("user/tests/mmapstorage/other", KEY_END);
	KeySet * returned = ksCut (ks, cutKey);
	succeed_if (returned, "keyset is empty (does not contain the cut keyset)");

	KeySet * simple = simpleTestKeySet ();
	compare_keyset (simple, ks);
	compare_keyset (other, returned);

	ksDel (other);
	ksDel (returned);
	ksDel (simple);
	keyDel (cutKey);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksPop (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	KeySet * poppedKeys = ksNew (0, KS_END);
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (ksGetSize (ks) == 1, "ksGetSize after ksPop should be decremented");
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) != -1, "ksAppendKey failed");
	succeed_if (ksGetSize (poppedKeys) == 3, "expecting three keys to be in ks");
	succeed_if (ksPop (ks) == 0, "ks should be empty");
	succeed_if (ksAppendKey (poppedKeys, ksPop (ks)) == -1, "ks should be empty, but is not");

	KeySet * test = simpleTestKeySet ();
	compare_keyset (poppedKeys, test);
	ksDel (test);

	ksDel (poppedKeys);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksLookup (const char * tmpFile, option_t options)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * lookup = keyNew ("user/tests/mmapstorage/simpleKey/a", KEY_END);
	Key * found = ksLookup (ks, lookup, options);
	succeed_if (found, "did not find key");
	if (options == KDB_O_POP)
	{
		// make sure key is really popped
		keyDel (found);
		found = ksLookup (ks, lookup, 0);
		succeed_if (!found, "found key that should not exist");
	}
	keyDel (lookup);

	lookup = keyNew ("user/tests/mmapstorage/simpleKey/foo", KEY_END);
	found = ksLookup (ks, lookup, options);
	succeed_if (!found, "found key that should not exist");
	keyDel (lookup);

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksLookupByName (const char * tmpFile, option_t options)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user/tests/mmapstorage/simpleKey/a";
	Key * found = ksLookupByName (ks, name, options);
	succeed_if (found, "did not find key");
	if (options == KDB_O_POP)
	{
		// make sure key is really popped
		keyDel (found);
		found = ksLookupByName (ks, name, 0);
		succeed_if (!found, "found key that should not exist");
	}

	name = "user/tests/mmapstorage/simpleKey/foo";
	found = ksLookupByName (ks, name, options);
	succeed_if (!found, "found key that should not exist");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

/* -- Key API tests --------------------------------------------------------------------------------------------------------------------- */

static void test_mmap_keyFlags (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = ksNew (10, keyNew ("user/tests/mmapstorage/testKey", KEY_FLAGS, KEY_BINARY, KEY_VALUE, "test key", KEY_END), KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/testKey", 0);
	succeed_if (found, "did not find key");
	succeed_if (keyIsBinary (found) == 1, "Key is not binary.");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyDup (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/b", 0);
	succeed_if (found, "did not find key");

	Key * duplicate = keyDup (found);

	// check that keyDup has not changed KeySet
	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	// check that KeySet is intact after deleting duplicate Key
	keyDel (duplicate);
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyCopy_newKey (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/b", 0);
	succeed_if (found, "did not find key");

	Key * copy = keyNew (0, KEY_END);
	succeed_if (keyCopy (copy, found) != -1, "keyCopy failed");

	compare_key (found, copy);

	// check that keyCopy has not changed KeySet
	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	// check that KeySet is intact after deleting Key copy
	keyDel (copy);
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyCopy_clearOverwriteKey (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * toCopy = keyNew ("user/tests/mmapstorage/newnewkey", KEY_VALUE, "new key", KEY_END);

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/b", 0);
	succeed_if (found, "did not find key");

	// overwrite Key in KeySet
	succeed_if (keyCopy (found, 0) == 0, "keyCopy: clear destination failed");
	succeed_if (keyCopy (found, toCopy) == 1, "keyCopy failed");
	compare_key (found, toCopy);
	keyDel (toCopy);

	// write KeySet back
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = ksLookupByName (ks, "user/tests/mmapstorage/newnewkey", 0);
	succeed_if (found, "did not find key");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyDel (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/b", 0);
	succeed_if (found, "did not find key");

	succeed_if (keyDel (found) > 0, "Key was NULL or free()'d unexpectedly");

	// check that keyDel has not changed KeySet
	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyClear (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/a", 0);
	succeed_if (found, "did not find key");

	succeed_if (keyClear (found) == 0, "Key was NULL, keyClear failed");

	keySetName (found, "user/tests/mmapstorage/foo");
	keySetString (found, "new key value");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

/* -- Key name API tests ---------------------------------------------------------------------------------------------------------------- */

static void test_mmap_keyName (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user/tests/mmapstorage/a";
	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t nameSize = keyGetNameSize (found);
	succeed_if (elektraStrNCmp (name, keyName (found), nameSize) == 0, "wrong Key name");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keySetName (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, "user/tests/mmapstorage/b", 0);
	succeed_if (found, "did not find key");

	Key * duplicate = keyDup (found);
	keySetName (duplicate, "user/tests/mmapstorage/z");
	keySetString (duplicate, "zzz");

	KeySet * expected = metaTestKeySet ();
	compare_keyset (ks, expected);

	ksDel (expected);
	keyDel (duplicate);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyGetFullName (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user/tests/mmapstorage/a";
	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t fullNameSize = keyGetFullNameSize (found);
	char * fullName = elektraMalloc (fullNameSize);
	ssize_t ret = keyGetFullName (found, fullName, fullNameSize);
	if (ret < 1)
	{
		yield_error ("Key full name NULL or size error");
	}
	else
	{
		succeed_if ((size_t) ret >= elektraStrLen (name), "Key full name size too small");
	}

	elektraFree (fullName);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyGetBaseName (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user/tests/mmapstorage/a";
	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	const char * constBaseName = "a";
	size_t constBaseNameSize = elektraStrLen (constBaseName);
	ssize_t baseNameSize = keyGetFullNameSize (found);
	char * baseName = elektraMalloc (baseNameSize);
	ssize_t ret = keyGetBaseName (found, baseName, baseNameSize);
	if (ret < 1)
	{
		yield_error ("Key base name NULL or size error");
	}
	else
	{
		succeed_if ((size_t) ret == elektraStrLen (constBaseName), "Key base name has wrong size");
	}

	succeed_if (elektraStrNCmp (constBaseName, baseName, constBaseNameSize) == 0, "Key base name is wrong");

	elektraFree (baseName);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

/* -- Key value API tests --------------------------------------------------------------------------------------------------------------- */

static void test_mmap_keyValue (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	const char * name = "user/tests/mmapstorage/specialkey";
	size_t valueSize = 42;
	void * value = elektraMalloc (valueSize);
	memset (value, 42, valueSize);

	Key * key = keyNew (name, KEY_END);
	keySetBinary (key, value, valueSize);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	succeed_if (elektraStrNCmp (value, keyValue (found), valueSize) == 0, "Key binary value is wrong");

	elektraFree (value);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyString (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	const char * name = "user/tests/mmapstorage/specialkey";
	const char * value = "special value";
	size_t valueSize = elektraStrLen (value);
	Key * key = keyNew (name, KEY_VALUE, value, KEY_END);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	succeed_if (elektraStrNCmp (value, keyString (found), valueSize) == 0, "Key string value is wrong");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyGetBinary (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	const char * name = "user/tests/mmapstorage/specialkey";
	size_t realValueSize = 42;
	void * value = elektraMalloc (realValueSize);
	memset (value, 42, realValueSize);

	Key * key = keyNew (name, KEY_END);
	keySetBinary (key, value, realValueSize);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (keyGetBinary (found, apiValue, apiValueSize) == (ssize_t) realValueSize, "Key binary has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) == 0, "Key binary value is wrong");

	elektraFree (apiValue);
	elektraFree (value);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keyGetString (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	const char * name = "user/tests/mmapstorage/specialkey";
	const char * value = "special value";
	size_t realValueSize = elektraStrLen (value);
	Key * key = keyNew (name, KEY_VALUE, value, KEY_END);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiString = elektraMalloc (apiValueSize);
	succeed_if (keyGetString (found, apiString, apiValueSize) == (ssize_t) realValueSize, "Key string has wrong size");

	succeed_if (elektraStrNCmp (value, apiString, realValueSize) == 0, "Key string value is wrong");

	elektraFree (apiString);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keySetBinary (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	const char * name = "user/tests/mmapstorage/specialkey";
	size_t realValueSize = 42;
	void * value = elektraMalloc (realValueSize);
	memset (value, 42, realValueSize);

	Key * key = keyNew (name, KEY_END);
	keySetBinary (key, value, realValueSize);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, KDB_O_POP);
	succeed_if (found, "did not find key");

	// now set a new key value to the Key _after_ kdbGet
	size_t newValueSize = 4096;
	void * newValue = elektraMalloc (newValueSize);
	memset (newValue, 253, newValueSize);

	succeed_if (keySetBinary (found, newValue, newValueSize) == (ssize_t) newValueSize, "Key binary could not be set");

	ksAppendKey (ks, found);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (keyGetBinary (found, apiValue, apiValueSize) == (ssize_t) newValueSize, "Key binary has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) != 0, "Key binary value is wrong");
	succeed_if (elektraStrNCmp (newValue, apiValue, newValueSize) == 0, "Key binary value is wrong");

	elektraFree (newValue);
	elektraFree (apiValue);
	elektraFree (value);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_keySetString (const char * tmpFile)
{
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = metaTestKeySet ();
	const char * name = "user/tests/mmapstorage/specialkey";
	const char * value = "special value";
	size_t realValueSize = elektraStrLen (value);
	Key * key = keyNew (name, KEY_VALUE, value, KEY_END);
	ksAppendKey (ks, key);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * found = ksLookupByName (ks, name, KDB_O_POP);
	succeed_if (found, "did not find key");

	// now set a new key string to the Key _after_ kdbGet
	const char * newValue = "some new special value";
	size_t newValueSize = elektraStrLen (newValue);

	succeed_if (keySetString (found, newValue) == (ssize_t) newValueSize, "Key string could not be set");

	ksAppendKey (ks, found);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	found = ksLookupByName (ks, name, 0);
	succeed_if (found, "did not find key");

	ssize_t apiValueSize = keyGetValueSize (found);
	char * apiValue = elektraMalloc (apiValueSize);
	succeed_if (keyGetString (found, apiValue, apiValueSize) == (ssize_t) newValueSize, "Key string has wrong size");

	succeed_if (elektraStrNCmp (value, apiValue, realValueSize) != 0, "Key string value is wrong");
	succeed_if (elektraStrNCmp (newValue, apiValue, newValueSize) == 0, "Key string value is wrong");

	elektraFree (apiValue);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	const char * tmpFile = elektraFilename ();
	/*
	#ifdef DEBUG
		testDynArray1 ();
	#endif
	*/
	// call once before clearStorage, to test non existent file
	test_mmap_get_set (tmpFile);

	clearStorage (tmpFile);
	test_mmap_truncated_file (tmpFile);

	clearStorage (tmpFile);
	test_mmap_get_set_empty (tmpFile);

	clearStorage (tmpFile);
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

	clearStorage (tmpFile);
	test_mmap_opmphm (tmpFile);

	// KeySet API tests
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
	test_mmap_double_get (tmpFile); // regression test

	clearStorage (tmpFile);
	test_mmap_ksAppendKey (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksAppend (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksCut (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksPop (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ksLookup (tmpFile, 0);

	clearStorage (tmpFile);
	test_mmap_ksLookup (tmpFile, KDB_O_POP);

	clearStorage (tmpFile);
	test_mmap_ksLookupByName (tmpFile, 0);

	clearStorage (tmpFile);
	test_mmap_ksLookupByName (tmpFile, KDB_O_POP);

	// Key API tests
	clearStorage (tmpFile);
	test_mmap_keyFlags (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyDup (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyCopy_newKey (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyCopy_clearOverwriteKey (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyDel (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyClear (tmpFile);

	// Key Name API tests
	clearStorage (tmpFile);
	test_mmap_keyName (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keySetName (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyGetFullName (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyGetBaseName (tmpFile);

	// Key Value API tests
	clearStorage (tmpFile);
	test_mmap_keyValue (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyString (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyGetBinary (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keyGetString (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keySetBinary (tmpFile);

	clearStorage (tmpFile);
	test_mmap_keySetString (tmpFile);

	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
