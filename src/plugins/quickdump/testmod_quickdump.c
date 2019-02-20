/**
 * @file
 *
 * @brief Tests for quickdump plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#include <stdio.h>
#include <unistd.h>

#include "quickdump/test.quickdump.h"

static int compare_binary_files (const char * filename1, const char * filename2)
{
	FILE * f1 = fopen (filename1, "rb");
	FILE * f2 = fopen (filename2, "rb");

	int result = 0;

	int c1, c2;
	while (result == 0 && (c1 = fgetc (f1)) != EOF && (c2 = fgetc (f2)) != EOF)
	{
		result = c1 - c2;
	}

	if (result == 0)
	{
		int end1 = fgetc (f1) == EOF && feof (f1) != 0;
		int end2 = fgetc (f2) == EOF && feof (f2) != 0;

		result = end1 - end2;
	}

	fclose (f1);
	fclose (f2);

	return result;
}

static void test_basics (void)
{
	printf ("test basics");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("quickdump/test.quickdump"));
	char * outfile = elektraStrDup (srcdir_file ("quickdump/test.quickdump.out"));

	{
		Key * getKey = keyNew ("dir/tests/bench", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		KeySet * expected = test_quickdump_expected ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		Key * k1 = ksLookupByName (ks, "dir/tests/bench/__112", 0);
		Key * k8 = ksLookupByName (ks, "dir/tests/bench/__911", 0);
		succeed_if (keyGetMeta (k1, "meta/_35") == keyGetMeta (k8, "meta/_35"), "copy meta failed");

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("dir/tests/bench", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

		succeed_if (compare_binary_files (infile, outfile) == 0, "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (outfile);
	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("QUICKDUMP     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_quickdump");

	return nbError;
}
