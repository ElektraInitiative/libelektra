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

#include "test.quickdump.h"

static void create_test_file (const char * filename, const unsigned char * content, size_t size)
{
	FILE * f = fopen (filename, "wb");
	fwrite (content, sizeof (unsigned char), size, f);
	fclose (f);
}

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
	char cwd[PATH_MAX];
	printf ("test basics %s\n", getcwd (cwd, PATH_MAX));

	create_test_file ("test.quickdump", test_quickdump, test_quickdump_len);

	KeySet * ks = ksNew (0, KS_END);
	{
		Key * getKey = keyNew ("dir/tests/bench", KEY_VALUE, "test.quickdump", KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("dir/tests/bench", KEY_VALUE, "test.quickdump.2", KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}
	ksDel (ks);

	succeed_if (compare_binary_files ("test.quickdump", "test.quickdump.2") == 0, "files differ");

	remove ("test.quickdump");
	remove ("test.quickdump.2");
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
