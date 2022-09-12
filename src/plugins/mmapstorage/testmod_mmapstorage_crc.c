/**
 * @file
 *
 * @brief Tests for mmapstorage_crc plugin variant
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#define _XOPEN_SOURCE 600

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <stdio.h>    // fopen()
#include <sys/mman.h> // mmap()
#include <sys/stat.h> // stat(), chmod()

#include <tests_plugin.h>

#include "internal.h"
#include "mmapstorage.h"

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define TEST_ROOT_KEY "user:/tests/mmapstorage_crc"

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_mmap_crc_no_checksum (const char * tmpFile)
{
	// regression test: write mmap file without checksum (=0L), then read with checksum
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
	// the following fails if the internal CRC on/off flag was not set
	{
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage_crc");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

static void test_mmap_crc_wrong_checksum (const char * tmpFile)
{
	// first write a mmap file
	{
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage_crc");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "kdbSet was not successful");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}

	// set wrong checksum in mmap header
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
	mmapHeader->checksum++;

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

	// wrong checksum should be detected now
	{
		// we expect an error here
		ElektraKey * parentKey = elektraKeyNew (TEST_ROOT_KEY, ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
		ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
		PLUGIN_OPEN ("mmapstorage_crc");

		ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
		succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet did not detect wrong checksum");

		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		PLUGIN_CLOSE ();
	}
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("MMAPSTORAGE CRC     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	const char * tmpFile = elektraFilename ();
	test_mmap_crc_no_checksum (tmpFile);
	test_mmap_crc_wrong_checksum (tmpFile);

	printf ("\ntestmod_mmapstorage_crc RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
