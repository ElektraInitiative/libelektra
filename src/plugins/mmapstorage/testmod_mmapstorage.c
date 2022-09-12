/**
 * @file
 *
 * @brief Tests for mmapstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#define _XOPEN_SOURCE 600

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <fcntl.h> // fcntl(), open()
#include <stdio.h> // fopen(), fileno()
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>  // mmap()
#include <sys/stat.h>  // stat(), chmod()
#include <sys/types.h> // ftruncate ()
#include <sys/wait.h>  // waitpit()
#include <unistd.h>    // ftruncate(), pipe(), fork()

#include <kdbconfig.h>
#include <kdbprivate.h>

#include <tests_plugin.h>

#include "dynarray.h"
#include "internal.h"
#include "mmapstorage.h"

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define KEY_NAME_LENGTH 1000
#define NUM_DIR 10
#define NUM_KEY 100

#define TEST_ROOT_KEY "user:/tests/mmapstorage"

/* -- KeySet test data ------------------------------------------------------------------------------------------------------------------ */

static ElektraKeyset * simpleTestKeySet (void)
{
	return elektraKeysetNew (10, elektraKeyNew ("user:/tests/mmapstorage/simpleKey", ELEKTRA_KEY_VALUE, "root key", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/mmapstorage/simpleKey/a", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/mmapstorage/simpleKey/b", ELEKTRA_KEY_VALUE, "b value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static ElektraKeyset * metaTestKeySet (void)
{
	return elektraKeysetNew (10,
		      elektraKeyNew ("user:/tests/mmapstorage", ELEKTRA_KEY_VALUE, "root key", ELEKTRA_KEY_META, "a",
			      "b aksdjfh aksjdhf aklsjdhf aksljdhf aklsjdhf aksljdhf ", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/mmapstorage/a", ELEKTRA_KEY_VALUE, "a value", ELEKTRA_KEY_META, "ab",
			      "cd oiahsdkfhga sdjkfhgsuzdgf kashgdf aszgdf uashdf ", ELEKTRA_KEY_END),
		      elektraKeyNew ("user:/tests/mmapstorage/b", ELEKTRA_KEY_VALUE, "b value", ELEKTRA_KEY_META, "longer val",
			      "here some even more with ugly €@\\1¹²³¼ chars", ELEKTRA_KEY_END),
		      ELEKTRA_KS_END);
}

typedef struct _binaryTest
{
	size_t A_size;
	ssize_t B_ssize;
} BinaryTest;
static BinaryTest binaryTestA = { .A_size = SIZE_MAX, .B_ssize = -1 };

static ElektraKeyset * otherMetaTestKeySet (void)
{
	return elektraKeysetNew (
		10, elektraKeyNew ("user:/tests/mmapstorage", ELEKTRA_KEY_VALUE, "root key", ELEKTRA_KEY_META, "e", "other meta root", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/mmapstorage/f", ELEKTRA_KEY_VALUE, "f value", ELEKTRA_KEY_META, "foo", "some other key in the other meta keyset",
			ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/mmapstorage/i", ELEKTRA_KEY_VALUE, "i value", ELEKTRA_KEY_META, "i is quite valuable", "happy data is happy :)",
			ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/mmapstorage/j/k/l", ELEKTRA_KEY_VALUE, "jkl value", ELEKTRA_KEY_META, "where is everyone?", "don't panic", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/mmapstorage/m", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (BinaryTest), ELEKTRA_KEY_VALUE, &(binaryTestA), ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
}

static ElektraKeyset * largeTestKeySet (void)
{
	int i, j;
	char name[KEY_NAME_LENGTH + 1];
	char value[] = "data";
	ElektraKeyset * large = elektraKeysetNew (NUM_KEY * NUM_DIR, ELEKTRA_KS_END);

	for (i = 0; i < NUM_DIR; i++)
	{
		snprintf (name, KEY_NAME_LENGTH, "%s/%s%d", TEST_ROOT_KEY, "dir", i);
		elektraKeysetAppendKey (large, elektraKeyNew (name, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END));
		for (j = 0; j < NUM_KEY; j++)
		{
			snprintf (name, KEY_NAME_LENGTH, "%s/%s%d/%s%d", TEST_ROOT_KEY, "dir", i, "key", j);
			elektraKeysetAppendKey (large, elektraKeyNew (name, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END));
		}
	}
	return large;
}

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_mmap_get_set_empty (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_set (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	elektraKeysetDel (ks);
	ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = simpleTestKeySet ();
	compare_keyset (expected, ks);
	compare_keyset (ks, expected);

	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_set_get_global (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	plugin->global = 0;
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	plugin->global = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);
	elektraKeysetDel (plugin->global);

	ks = metaTestKeySet ();
	plugin->global = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);
	elektraKeysetDel (plugin->global);

	plugin->global = elektraKeysetNew (0, ELEKTRA_KS_END);
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = simpleTestKeySet ();
	compare_keyset (expected, plugin->global);
	compare_keyset (plugin->global, expected);

	elektraKeysetDel (plugin->global);
	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_global_after_reopen (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	plugin->global = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected_global = simpleTestKeySet ();
	compare_keyset (expected_global, plugin->global);
	compare_keyset (plugin->global, expected_global);

	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (expected, ks);
	compare_keyset (ks, expected);

	elektraKeysetDel (expected_global);
	elektraKeysetDel (plugin->global);
	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_set_get_global_metadata (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");

	ElektraKeyset * ks = metaTestKeySet ();
	plugin->global = otherMetaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);
	elektraKeysetDel (plugin->global);

	plugin->global = elektraKeysetNew (0, ELEKTRA_KS_END);
	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected_global = otherMetaTestKeySet ();
	compare_keyset (expected_global, plugin->global);
	compare_keyset (plugin->global, expected_global);
	elektraKeysetDel (expected_global);

	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (expected, ks);
	compare_keyset (ks, expected);
	elektraKeysetDel (expected);

	elektraKeysetDel (plugin->global);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_truncated_file (const char * tmpFile)
{
	// first write a mmap file
	{
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = simpleTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
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
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet did not detect truncated file");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

static void test_mmap_wrong_magic_number (const char * tmpFile)
{
	// first write a mmap file
	{
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = simpleTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
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
		ELEKTRA_LOG_WARNING ("error mapping file %s\nmmapSize: " ELEKTRA_STAT_ST_SIZE_F, tmpFile, sbuf.st_size);
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
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "kdbGet did not detect wrong magic number");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

static void test_mmap_wrong_format_version (const char * tmpFile)
{
	// first write a mmap file
	{
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = simpleTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}

	// set wrong version number in mmap header
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
		ELEKTRA_LOG_WARNING ("error mapping file %s\nmmapSize: " ELEKTRA_STAT_ST_SIZE_F, tmpFile, sbuf.st_size);
		yield_error ("mmap() error");
		return;
	}
	if (fp)
	{
		fclose (fp);
	}

	MmapHeader * mmapHeader = (MmapHeader *) mappedRegion;
	mmapHeader->formatVersion = ELEKTRA_MMAP_FORMAT_VERSION + 1;

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

	// wrong format version should be detected now
	{
		// we expect an error here
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "kdbGet did not detect wrong format version");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

static void test_mmap_wrong_magic_keyset (const char * tmpFile)
{
	// first write a mmap file
	{
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = simpleTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}

	// now manipulate magic keyset inside the mapped region
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
		ELEKTRA_LOG_WARNING ("error mapping file %s\nmmapSize: " ELEKTRA_STAT_ST_SIZE_F, tmpFile, sbuf.st_size);
		yield_error ("mmap() error");
		return;
	}
	if (fp)
	{
		fclose (fp);
	}

	ElektraKeyset * magicKs = (ElektraKeyset *) (mappedRegion + sizeof (MmapHeader));
	magicKs->size = 1234; // magic keyset contains SIZE_MAX here

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

	// manipulated magic keyset should be detected now
	{
		// we expect an error here
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
			    "kdbGet did not detect wrong magic keyset");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

static void test_mmap_set_get (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	ElektraKeyset * expected = simpleTestKeySet ();
	compare_keyset (expected, returned);

	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_get_after_reopen (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = simpleTestKeySet ();
	compare_keyset (expected, returned);
	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_set_get_large_keyset (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = largeTestKeySet ();
	ElektraKeyset * expected = elektraKeysetDeepDup (ks);

	const char * name = "user:/tests/mmapstorage/dir7/key3";
	ElektraKey * found = elektraKeysetLookupByName (ks, name, ELEKTRA_KDB_O_OPMPHM);
	if (!found)
	{
		yield_error ("Key not found.")
	}

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);

	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	compare_keyset (expected, returned);

	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_ks_copy (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * returned = simpleTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	elektraKeysetDel (returned);

	returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = simpleTestKeySet ();
	compare_keyset (expected, returned);

	ElektraKeyset * copiedKs = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetCopy (copiedKs, returned);
	compare_keyset (expected, copiedKs);

	elektraKeysetDel (copiedKs);
	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_empty_after_clear (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	succeed_if (elektraKeysetGetSize (returned) == 0, "KeySet not empty after clear (or nullptr)");

	elektraKeysetDel (returned);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_mmap_meta (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = metaTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (expected, returned);

	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_meta_get_after_reopen (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (expected, ks);

	elektraKeysetDel (expected);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_filter_meta (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = metaTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * current;
	elektraKeysetRewind (returned);
	while ((current = elektraKeysetNext (returned)) != 0)
	{
		succeed_if (current->meta != 0, "key had no metadata, but metadata was expected");
		elektraKeysetClear (current->meta);
	}

	succeed_if (plugin->kdbSet (plugin, returned, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (returned);

	returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	elektraKeysetRewind (returned);
	while ((current = elektraKeysetNext (returned)) != 0)
	{
		succeed_if (current->meta == 0, "key had metadata, but metadata was expected to be filtered");
	}

	elektraKeysetDel (returned);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_metacopy (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = metaTestKeySet ();

	ElektraKey * shareMeta = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetMeta (shareMeta, "sharedmeta", "shared meta key test");

	ElektraKey * current;
	elektraKeysetRewind (ks);
	while ((current = elektraKeysetNext (ks)) != 0)
	{
		elektraKeyCopyMeta (current, shareMeta, "sharedmeta");
	}
	ElektraKeyset * expected = elektraKeysetDeepDup (ks);


	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");


	compare_keyset (expected, returned);

	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	elektraKeyDel (shareMeta);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ks_copy_with_meta (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = metaTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);

	ElektraKeyset * returned = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");

	ElektraKeyset * expected = metaTestKeySet ();
	compare_keyset (expected, returned);

	ElektraKeyset * copiedKs = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetCopy (copiedKs, returned);
	compare_keyset (expected, copiedKs);

	elektraKeysetDel (copiedKs);
	elektraKeysetDel (expected);
	elektraKeysetDel (returned);

	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
static void test_mmap_opmphm (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = largeTestKeySet ();

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetDel (ks);

	ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	const char * name = "user:/tests/mmapstorage/dir7/key3";
	ElektraKey * found = elektraKeysetLookupByName (ks, name, ELEKTRA_KDB_O_OPMPHM);

	if (!found)
	{
		yield_error ("Key not found.")
	}

	// write and re-read keyset with OPMPHM structures
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	elektraKeysetClear (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	// try triggering memleaks by copies
	ElektraKeyset * copy = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKeysetCopy (copy, ks);
	elektraKeysetDel (copy);
	ElektraKeyset * dup = elektraKeysetDup (ks);
	elektraKeysetDel (dup);
	ElektraKeyset * deepDup = elektraKeysetDeepDup (ks);
	elektraKeysetDel (deepDup);

	ElektraKeyset * returned = largeTestKeySet ();
	// this lookup forces OPMPHM structures into the keyset
	// to test the OPMPHM cleanup functions inside mmapstorage
	ElektraKey * generateOpmphm = elektraKeysetLookupByName (ks, name, ELEKTRA_KDB_O_OPMPHM);
	if (!generateOpmphm)
	{
		yield_error ("Key not found.")
	}
	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	elektraKeysetDel (returned);
	returned = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, returned, parentKey) == 1, "kdbGet was not successful");
	succeed_if (returned->opmphm != 0, "opmphm not stored properly");

	found = elektraKeysetLookupByName (returned, name, ELEKTRA_KDB_O_OPMPHM);
	if (!found)
	{
		yield_error ("Key not found.")
	}

	// force re-generate the OPMPHM, tests cleanup and re-allocation after mmap
	// i.e. tests if we broke the standard cleanup functions with our mmap flags
	ElektraKey * key = elektraKeyNew ("user:/tests/mmapstorage/breakOpmphm", ELEKTRA_KEY_VALUE, "bad key", ELEKTRA_KEY_META, "e", "other meta root", ELEKTRA_KEY_END);
	elektraKeysetAppendKey (returned, key);
	found = elektraKeysetLookupByName (returned, name, ELEKTRA_KDB_O_OPMPHM);
	if (!found)
	{
		yield_error ("Key not found.")
	}

	elektraKeysetDel (returned);
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
#endif

static void test_mmap_ksDupFun (const char * tmpFile, ElektraKeyset * copyFunction (const ElektraKeyset * source))
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if ((ks->flags & ELEKTRA_KS_FLAG_MMAP_ARRAY) == ELEKTRA_KS_FLAG_MMAP_ARRAY, "KeySet array not in mmap");

	ElektraKeyset * dupKs = copyFunction (ks);
	compare_keyset (dupKs, ks);
	compare_keyset (ks, dupKs);

	elektraKeysetDel (dupKs);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_ksCopy (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");
	succeed_if ((ks->flags & ELEKTRA_KS_FLAG_MMAP_ARRAY) == ELEKTRA_KS_FLAG_MMAP_ARRAY, "KeySet array not in mmap");

	ElektraKeyset * copyKs = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (elektraKeysetCopy (copyKs, ks) == 1)
	{
		compare_keyset (copyKs, ks);
		compare_keyset (ks, copyKs);
	}
	else
	{
		yield_error ("ksCopy failed");
	}

	elektraKeysetDel (copyKs);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_open_pipe (void)
{
	// try writing to a non-regular file, we simply use a pipe here
	int pipefd[2];
	if (pipe (pipefd) != 0)
	{
		yield_error ("pipe() error");
	}
	char pipeFile[1024];
	snprintf (pipeFile, 1024, "/dev/fd/%d", pipefd[1]);
	pipeFile[1023] = '\0';

	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, pipeFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");

	ElektraKeyset * ks = simpleTestKeySet ();
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet could write to pipe, but should not");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_bad_file_permissions (const char * tmpFile)
{
	// try writing to a file with bad permissions
	if (getuid () == 0 || geteuid () == 0)
	{
		printf ("Skipping file permission test for root.\n");
		return;
	}

	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	FILE * fp;
	if ((fp = fopen (tmpFile, "r+")) == 0)
	{
		yield_error ("error opening file");
	}

	struct stat sbuf;
	if (stat (tmpFile, &sbuf) == -1)
	{
		yield_error ("stat() error");
	}
	fclose (fp);

	if (chmod (tmpFile, 0) != 0)
	{
		yield_error ("chmod() failed");
	}

	// open() call should fail because file permissions were wrong
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet did not detect bad file permissions");

	if (chmod (tmpFile, sbuf.st_mode) != 0)
	{
		yield_error ("chmod() failed");
	}

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_mmap_unlink (const char * tmpFile)
{
	// test file unlinking by overwriting config file while mapped
	int parentPipe[2];
	int childPipe[2];
	if (pipe (parentPipe) != 0 || pipe (childPipe) != 0)
	{
		yield_error ("pipe() error");
	}

	pid_t pid;
	char buf;
	pid = fork ();

	if (pid == -1)
	{
		yield_error ("fork() error");
		return;
	}
	else if (pid == 0)
	{
		// child: open a config file and leave it mapped
		int devnull = open ("/dev/null", O_RDWR);
		if (devnull == -1) _Exit (EXIT_FAILURE);

		// redirect any communication on standard file descriptors to /dev/null
		close (STDIN_FILENO);
		close (STDOUT_FILENO);
		close (STDERR_FILENO);
		if (dup (devnull) == -1) _Exit (EXIT_FAILURE);
		if (dup (devnull) == -1) _Exit (EXIT_FAILURE);
		if (dup (devnull) == -1) _Exit (EXIT_FAILURE);
		close (childPipe[0]);
		close (parentPipe[1]);

		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = simpleTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

		if (write (childPipe[1], "a", 1) != 1) _Exit (EXIT_FAILURE); // signal parent that we are ready
		close (childPipe[1]);
		if (read (parentPipe[0], &buf, 1) != 1) _Exit (EXIT_FAILURE); // wait for parent
		close (parentPipe[0]);

		ElektraKeyset * expected = simpleTestKeySet ();
		compare_keyset (expected, ks);
		compare_keyset (ks, expected);
		elektraKeysetDel (expected);
		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();

		_Exit (EXIT_SUCCESS);
	}
	else
	{
		// parent: try and destroy the file that the child has mapped
		close (childPipe[1]);
		close (parentPipe[0]);
		if (read (childPipe[0], &buf, 1) != 1) _Exit (EXIT_FAILURE); // wait for child
		close (childPipe[0]);

		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage");

		ElektraKeyset * ks = metaTestKeySet ();
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");
		if (write (parentPipe[1], "a", 1) != 1) _Exit (EXIT_FAILURE); // signal child that we are done
		close (parentPipe[1]);

		int status;
		waitpid (pid, &status, 0);
		if (status != 0) yield_error ("child process did not exit successfully.");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

static void clearStorage (const char * tmpFile)
{
	ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mmapstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

/* -- DynArray Tests -------------------------------------------------------------------------------------------------------------------- */

static int cmpfunc (const void * a, const void * b)
{
	return (*(int *) a - *(int *) b);
}

static void testDynArray (void)
{
	size_t testData[] = { 8466, 2651, 6624, 9575, 4628, 9361, 417,	8932, 4570, 343,  1866, 3135, 6617, 344,  9419, 2094, 5623,
			      4920, 2209, 8037, 8437, 7955, 5575, 8355, 1133, 6527, 8543, 3338, 1772, 2278, 7446, 8834, 7728, 665,
			      8519, 6079, 5060, 7429, 3843, 6923, 4073, 2245, 2784, 6620, 2887, 8497, 9360, 5752, 3195, 538,  1491,
			      8087, 8378, 5746, 4961, 5499, 8050, 2138, 1196, 1860, 4372, 6553, 4530, 8828, 4017, 9934, 3,    6274,
			      4405, 5021, 3416, 854,  4635, 9902, 5383, 7947, 5210, 8242, 1928, 3792, 7234, 759,  6571, 9514, 8451,
			      918,  9958, 1577, 96,   8644, 6815, 5584, 8585, 1252, 808,  5695, 910,  4157, 701,  77 };

	DynArray * dynArray = ELEKTRA_PLUGIN_FUNCTION (dynArrayNew) ();

	for (size_t i = 0; i < 100; ++i)
	{
		ELEKTRA_PLUGIN_FUNCTION (dynArrayFindOrInsert) ((ElektraKey *) testData[i], dynArray);
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

	ELEKTRA_PLUGIN_FUNCTION (dynArrayDelete) (dynArray);
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	testDynArray ();

	const char * tmpFile = elektraFilename ();
	printf ("%s\n", tmpFile);

	// call once before clearStorage, to test non existent file
	test_mmap_get_set (tmpFile);

	test_mmap_set_get_global (tmpFile);
	test_mmap_get_global_after_reopen (tmpFile);
	test_mmap_set_get_global_metadata (tmpFile);

	clearStorage (tmpFile);
	test_mmap_truncated_file (tmpFile);
	test_mmap_wrong_magic_number (tmpFile);
	test_mmap_wrong_format_version (tmpFile);
	test_mmap_wrong_magic_keyset (tmpFile);

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
	test_mmap_filter_meta (tmpFile);

	clearStorage (tmpFile);
	test_mmap_ks_copy_with_meta (tmpFile);

#ifdef ELEKTRA_ENABLE_OPTIMIZATIONS
	clearStorage (tmpFile);
	test_mmap_opmphm (tmpFile);
#endif

	clearStorage (tmpFile);
	test_mmap_ksDupFun (tmpFile, elektraKeysetDup);

	clearStorage (tmpFile);
	test_mmap_ksDupFun (tmpFile, elektraKeysetDeepDup);

	clearStorage (tmpFile);
	test_mmap_ksCopy (tmpFile);

	test_mmap_open_pipe ();
	test_mmap_bad_file_permissions (tmpFile);

	test_mmap_unlink (tmpFile);

	printf ("\ntestmod_mmapstorage RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
