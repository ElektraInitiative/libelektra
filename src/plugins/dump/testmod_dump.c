/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#include "testdata_v1.h"

static void test_oneValue (void)
{
	printf ("test oneValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/one_value.dump"));
	char * outfile = elektraStrDup (srcdir_file ("dump/v1/one_value.dump.out"));

	{
		Key * getKey = keyNew ("user/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_v1_oneValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (infile, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_twoValue (void)
{
	printf ("test twoValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/two_value.dump"));
	char * outfile = elektraStrDup (srcdir_file ("dump/v1/two_value.dump.out"));

	{
		Key * getKey = keyNew ("user/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_v1_twoValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (infile, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_threeValue (void)
{
	printf ("test threeValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/three_value.dump"));
	char * outfile = elektraStrDup (srcdir_file ("dump/v1/three_value.dump.out"));

	{
		Key * getKey = keyNew ("user/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_v1_threeValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (infile, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_againTwoValue (void)
{
	printf ("test againTwoValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/again_two_value.dump"));
	char * outfile = elektraStrDup (srcdir_file ("dump/v1/again_two_value.dump.out"));

	{
		Key * getKey = keyNew ("user/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_v1_againTwoValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (infile, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_metaData (void)
{
	printf ("test metaData\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/meta_data.dump"));
	char * outfile = elektraStrDup (srcdir_file ("dump/v1/meta_data.dump.out"));

	{
		Key * getKey = keyNew ("user/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_v1_metaData ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (infile, outfile), "files differ");
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
	printf ("DUMP       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_oneValue ();
	test_twoValue ();
	test_threeValue ();
	test_againTwoValue ();
	test_metaData ();

	print_result ("testmod_dump");

	return nbError;
}
