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

#include <elektra/type/types.h>
#include <internal/kdb/config.h>

#include <tests_plugin.h>

#include <stdio.h>
#include <unistd.h>

#include "./quickdump/test.quickdump.h"

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

static int check_binary_file (const char * filename, const unsigned char * data, size_t dataSize)
{
	FILE * file = fopen (filename, "rb");

	int result = 0;

	int c;
	size_t pos = 0;
	while (result == 0 && (c = fgetc (file)) != EOF && pos < dataSize)
	{
		result = c - (int) data[pos++];
	}

	if (result == 0)
	{
		int end1 = fgetc (file) == EOF && feof (file) != 0;
		int end2 = pos >= dataSize;

		result = end1 - end2;
	}

	fclose (file);

	return result;
}

static void test_basics (void)
{
	printf ("test basics\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("quickdump/test.quickdump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("dir:/tests/bench", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		KeySet * expected = test_quickdump_expected ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		Key * k1 = ksLookupByName (ks, "dir:/tests/bench/__112", 0);
		Key * k8 = ksLookupByName (ks, "dir:/tests/bench/__911", 0);
		succeed_if (keyGetMeta (k1, "meta/_35") == keyGetMeta (k8, "meta/_35"), "copy meta failed");

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("dir:/tests/bench", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_binary_files (infile, outfile) == 0, "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_noParent (void)
{
	printf ("test noparent\n");

	KeySet * input = ksNew (2, keyNew ("/", KEY_VALUE, "value", KEY_END), keyNew ("/a", KEY_VALUE, "value1", KEY_END), KS_END);
	KeySet * expected = ksNew (2, keyNew ("dir:/tests/bench", KEY_VALUE, "value", KEY_END),
				   keyNew ("dir:/tests/bench/a", KEY_VALUE, "value1", KEY_END), KS_END);
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * setKey = keyNew ("dir:/tests/bench", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (1, keyNew ("user:/noparent", KEY_END), KS_END);
		PLUGIN_OPEN ("quickdump");

		succeed_if (plugin->kdbSet (plugin, input, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (check_binary_file (outfile, test_quickdump_noParent_data, test_quickdump_noParent_dataSize) == 0,
			    "files differ");

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * getKey = keyNew ("dir:/tests/bench", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");


		KeySet * actual = ksNew (0, KS_END);
		succeed_if (plugin->kdbGet (plugin, actual, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, actual);

		ksDel (actual);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	remove (outfile);

	elektraFree (outfile);
	ksDel (input);
	ksDel (expected);
}

static void test_parentKeyValue (void)
{
	printf ("test parent key value\n");

	KeySet * expected = ksNew (1, keyNew ("dir:/tests/bench", KEY_VALUE, "value", KEY_END), KS_END);
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * setKey = keyNew ("dir:/tests/bench", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");

		succeed_if (plugin->kdbSet (plugin, expected, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS,
			    "call to kdbSet was not successful");

		succeed_if (check_binary_file (outfile, test_quickdump_parentKeyValue_data, test_quickdump_parentKeyValue_dataSize) == 0,
			    "files differ");

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * getKey = keyNew ("dir:/tests/bench", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("quickdump");


		KeySet * actual = ksNew (0, KS_END);
		succeed_if (plugin->kdbGet (plugin, actual, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, actual);

		ksDel (actual);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	remove (outfile);

	elektraFree (outfile);
	ksDel (expected);
}

#include "./varint.c"

static void test_varint (void)
{
	printf ("test varint\n");

	kdb_unsigned_long_long_t testNumbers[] = {
		0x0000000000000000, 0x0000000000000001, 0x0000000000000002, 0x0000000000000003, 0x0000000000000004, 0x0000000000000005,
		0x0000000000000008, 0x0000000000000009, 0x0000000000000010, 0x0000000000000011, 0x0000000000000020, 0x0000000000000021,
		0x000000000000007F, 0x0000000000000080, 0x0000000000000081, 0x0000000000000100, 0x0000000000000101, 0x0000000000000200,
		0x0000000000000201, 0x0000000000000400, 0x0000000000000401, 0x0000000000000800, 0x0000000000000801, 0x0000000000001000,
		0x0000000000001001, 0x0000000000002000, 0x0000000000002001, 0x0000000000003FFF, 0x0000000000004000, 0x0000000000004001,
		0x0000000000008000, 0x0000000000008001, 0x0000000000010000, 0x0000000000010001, 0x0000000000020000, 0x0000000000020001,
		0x0000000000040000, 0x0000000000040001, 0x0000000000080000, 0x0000000000080001, 0x0000000000100000, 0x0000000000100001,
		0x00000000001FFFFF, 0x0000000000200000, 0x0000000000200001, 0x0000000000400000, 0x0000000000400001, 0x0000000000800000,
		0x0000000000800001, 0x0000000001000000, 0x0000000001000001, 0x0000000002000000, 0x0000000002000001, 0x0000000004000000,
		0x0000000004000001, 0x0000000008000000, 0x0000000008000001, 0x000000000FFFFFFF, 0x0000000010000000, 0x0000000010000001,
		0x0000000020000000, 0x0000000020000001, 0x0000000040000000, 0x0000000040000001, 0x0000000080000000, 0x0000000080000001,
		0x0000000100000000, 0x0000000100000001, 0x0000000200000000, 0x0000000200000001, 0x0000000400000000, 0x0000000400000001,
		0x00000007FFFFFFFF, 0x0000000800000000, 0x0000000800000001, 0x0000001000000000, 0x0000001000000001, 0x0000002000000000,
		0x0000002000000001, 0x0000004000000000, 0x0000004000000001, 0x0000008000000000, 0x0000008000000001, 0x0000010000000000,
		0x0000010000000001, 0x0000020000000000, 0x0000020000000001, 0x000003FFFFFFFFFF, 0x0000040000000000, 0x0000040000000001,
		0x0000080000000000, 0x0000080000000001, 0x0000100000000000, 0x0000100000000001, 0x0000200000000000, 0x0000200000000001,
		0x0000400000000000, 0x0000400000000001, 0x0000800000000000, 0x0000800000000001, 0x0001000000000000, 0x0001000000000001,
		0x0001FFFFFFFFFFFF, 0x0002000000000000, 0x0002000000000001, 0x0004000000000000, 0x0004000000000001, 0x0008000000000000,
		0x0010000000000000, 0x0010000000000001, 0x0020000000000000, 0x0020000000000001, 0x0040000000000000, 0x0080000000000000,
		0x0080000000000001, 0x00FFFFFFFFFFFFFF, 0x0100000000000000, 0x0100000000000001, 0x0200000000000000, 0x0200000000000001,
		0x0400000000000000, 0x0400000000000001, 0x0800000000000000, 0x0800000000000001, 0x1000000000000000, 0x1000000000000001,
		0x2000000000000000, 0x2000000000000001, 0x4000000000000000, 0x4000000000000001, 0x8000000000000000, 0x8000000000000001,
		0xFFFFFFFFFFFFFFFF
	};
	size_t testNumbersCount = sizeof (testNumbers) / sizeof (kdb_unsigned_long_long_t);

	char errorBuf[128];
	for (size_t i = 0; i < testNumbersCount; ++i)
	{
		FILE * f = fopen (elektraFilename (), "wb");
		succeed_if (varintWrite (f, testNumbers[i]), "write error");
		fclose (f);

		f = fopen (elektraFilename (), "rb");
		kdb_unsigned_long_long_t result = 0;
		succeed_if (varintRead (f, &result), "read error");
		fclose (f);

		snprintf (errorBuf, sizeof (errorBuf), "conversion for %" PRIX64 " wrong, got %" PRIX64, testNumbers[i], result);
		succeed_if (testNumbers[i] == result, errorBuf);
	}
}

int main (int argc, char ** argv)
{
	printf ("QUICKDUMP     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_varint ();

	test_basics ();
	test_noParent ();
	test_parentKeyValue ();

	print_result ("testmod_quickdump");

	return nbError;
}
