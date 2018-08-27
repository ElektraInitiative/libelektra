/**
 * @file
 *
 * @brief Tests for mmapstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#define _POSIX_C_SOURCE 200112L

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <stdio.h> // fopen(), fileno()
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>  // mmap()
#include <sys/stat.h>  // stat()
#include <sys/types.h> // ftruncate ()
#include <tests.h>
#include <unistd.h> // ftruncate(), pipe()

#include <kdbconfig.h>
#include <kdbmmap.h>
#include <kdbprivate.h>

#include <tests_plugin.h>

#include "dynarray.h"
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

static void test_mmap_wrong_magic_number (const char * tmpFile)
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

	// now manipulate magic number inside the mmap header
	FILE * fp;
	if ((fp = fopen (tmpFile, "r+")) == 0)
	{
		yield_error ("fopen() error");
	}
	struct stat sbuf;
	if (stat (tmpFile, &sbuf) == -1)
	{
		yield_error ("stat() error");
	}

	int fd = fileno (fp);
	char * mappedRegion = mmap (0, sbuf.st_size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	if (mappedRegion == MAP_FAILED)
	{
		ELEKTRA_LOG_WARNING ("error mapping file %s\nmmapSize: %zu", tmpFile, sbuf.st_size);
		yield_error ("mmap() error");
		return;
	}
	if (fp)
	{
		fclose (fp);
	}

	MmapHeader * mmapHeader = (MmapHeader *) mappedRegion;
	mmapHeader->mmapMagicNumber = ELEKTRA_MAGIC_MMAP_NUMBER - 1;

	if (msync ((void *) mappedRegion, sbuf.st_size, MS_SYNC) != 0)
	{
		yield_error ("msync() error");
		return;
	}

	if (munmap (mappedRegion, sbuf.st_size) != 0)
	{
		yield_error ("munmap() error");
		return;
	}

	// manipulated magic number should be detected now
	{
		// we expect an error here
		Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("mmapstorage");

		KeySet * ks = ksNew (0, KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "kdbGet did not detect wrong magic number");

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

static void test_mmap_unlink_dirty_plugindata (const char * tmpFile)
{
	// insert dirty metadata into plugin data, regression test for unlinking error handling
	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	// we know that plugin data has a key (the linked file from kdbGet above)
	KeySet * mappedFiles = (KeySet *) elektraPluginGetData (plugin);
	Key * found = ksLookup (mappedFiles, parentKey, 0);
	if (found)
	{
		keySetMeta (found, "some erroneous key", "which should not exist here");
		char tooLarge[1024];
		sprintf (tooLarge, "%lu", UINTPTR_MAX);
		tooLarge[1023] = '\0';
		keySetMeta (found, tooLarge, "0xZZZZ");
	}
	else
	{
		yield_error ("test eror, missing key in plugin data for unlinking");
	}

	// trigger unlinking via kdbSet
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_open_pipe (void)
{
	// try writing to an invalid file, we simply use a pipe here
	int pipefd[2];
	pipe (pipefd);
	char pipeFile[1024];
	sprintf (pipeFile, "/dev/fd/%d", pipefd[1]);
	pipeFile[1023] = '\0';

	Key * parentKey = keyNew (TEST_ROOT_KEY, KEY_VALUE, pipeFile, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mmapstorage");

	KeySet * ks = simpleTestKeySet ();
	// truncate inside mmap plugin should fail here, since the pipe cannot be truncated
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet did not detect error with the file");

	keyDel (parentKey);
	ksDel (ks);
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

/* -- DynArray Tests -------------------------------------------------------------------------------------------------------------------- */

int cmpfunc (const void * a, const void * b)
{
	return (*(int *) a - *(int *) b);
}

static void testDynArray (void)
{
	size_t testData[] = { 8466, 2651, 6624, 9575, 4628, 9361, 417,  8932, 4570, 343,  1866, 3135, 6617, 344,  9419, 2094, 5623,
			      4920, 2209, 8037, 8437, 7955, 5575, 8355, 1133, 6527, 8543, 3338, 1772, 2278, 7446, 8834, 7728, 665,
			      8519, 6079, 5060, 7429, 3843, 6923, 4073, 2245, 2784, 6620, 2887, 8497, 9360, 5752, 3195, 538,  1491,
			      8087, 8378, 5746, 4961, 5499, 8050, 2138, 1196, 1860, 4372, 6553, 4530, 8828, 4017, 9934, 3,    6274,
			      4405, 5021, 3416, 854,  4635, 9902, 5383, 7947, 5210, 8242, 1928, 3792, 7234, 759,  6571, 9514, 8451,
			      918,  9958, 1577, 96,   8644, 6815, 5584, 8585, 1252, 808,  5695, 910,  4157, 701,  77 };

	DynArray * dynArray = elektraMmapDynArrayNew ();

	for (size_t i = 0; i < 100; ++i)
	{
		elektraMmapDynArrayFindOrInsert ((Key *) testData[i], dynArray);
	}

	qsort (testData, 100, sizeof (size_t), cmpfunc);

	int error = 0;
	for (size_t i = 0; i < 100; ++i)
	{
		if (testData[i] != (size_t) dynArray->keyArray[i])
		{
			++error;
		}
	}

	succeed_if (error == 0, "dynArray does not sort array properly");

	elektraMmapDynArrayDelete (dynArray);
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testDynArray ();

	const char * tmpFile = elektraFilename ();

	// call once before clearStorage, to test non existent file
	test_mmap_get_set (tmpFile);

	clearStorage (tmpFile);
	test_mmap_truncated_file (tmpFile);

	test_mmap_wrong_magic_number (tmpFile);

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

	clearStorage (tmpFile);
	test_mmap_ksDupFun (tmpFile, ksDup);

	clearStorage (tmpFile);
	test_mmap_ksDupFun (tmpFile, ksDeepDup);

	clearStorage (tmpFile);
	test_mmap_ksCopy (tmpFile);

	clearStorage (tmpFile);
	test_mmap_unlink_dirty_plugindata (tmpFile);

	test_mmap_open_pipe ();

	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
