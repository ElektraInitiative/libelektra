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

#include <internal/config.h>

#include <tests_plugin.h>

#include "./testdata.h"

static void test_v1_oneValue (void)
{
	printf ("test v1 oneValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/one_value.dump"));
	char * v2file = elektraStrDup (srcdir_file ("dump/v2/one_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_oneValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (v2file, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (v2file);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_v1_twoValue (void)
{
	printf ("test v1 twoValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/two_value.dump"));
	char * v2file = elektraStrDup (srcdir_file ("dump/v2/two_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_twoValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (v2file, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (v2file);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_v1_threeValue (void)
{
	printf ("test v1 threeValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/three_value.dump"));
	char * v2file = elektraStrDup (srcdir_file ("dump/v2/three_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_threeValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (v2file, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (v2file);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_v1_againTwoValue (void)
{
	printf ("test v1 againTwoValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/again_two_value.dump"));
	char * v2file = elektraStrDup (srcdir_file ("dump/v2/again_two_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_againTwoValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (v2file, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (v2file);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_v1_metaData (void)
{
	printf ("test v1 metaData\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v1/meta_data.dump"));
	char * v2file = elektraStrDup (srcdir_file ("dump/v2/meta_data.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_metaData ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		succeed_if (plugin->kdbSet (plugin, ks, setKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");

		succeed_if (compare_line_files (v2file, outfile), "files differ");
		remove (outfile);

		keyDel (setKey);
		PLUGIN_CLOSE ();
	}

	elektraFree (infile);
	elektraFree (v2file);
	elektraFree (outfile);
	ksDel (ks);
}

static void test_v2_oneValue (void)
{
	printf ("test v2 oneValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/one_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_oneValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

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

static void test_v2_twoValue (void)
{
	printf ("test v2 twoValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/two_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_twoValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

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

static void test_v2_threeValue (void)
{
	printf ("test v2 threeValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/three_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_threeValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

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

static void test_v2_againTwoValue (void)
{
	printf ("test v2 againTwoValue\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/again_two_value.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_againTwoValue ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

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

static void test_v2_metaData (void)
{
	printf ("test v2 metaData\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/meta_data.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("user:/tests/script", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_metaData ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("user:/tests/script", KEY_VALUE, outfile, KEY_END);

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

static void test_v2_fullnames (void)
{
	printf ("test v2 fullnames\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/fullnames.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("system:/elektra/mountpoints", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (1, keyNew ("user:/fullname", KEY_END), KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_demo ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("system:/elektra/mountpoints", KEY_VALUE, outfile, KEY_END);

		KeySet * conf = ksNew (1, keyNew ("user:/fullname", KEY_END), KS_END);
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

static void test_v2_demo (void)
{
	printf ("test v2 demo\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/demo.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("system:/elektra/mountpoints", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_demo ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("system:/elektra/mountpoints", KEY_VALUE, outfile, KEY_END);

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

static void test_v2_demo_root (void)
{
	printf ("test v2 demo\n");

	KeySet * ks = ksNew (0, KS_END);
	char * infile = elektraStrDup (srcdir_file ("dump/v2/demo.dump"));
	char * outfile = elektraStrDup (elektraFilename ());

	{
		Key * getKey = keyNew ("system:/", KEY_VALUE, infile, KEY_END);

		KeySet * conf = ksNew (0, KS_END);
		PLUGIN_OPEN ("dump");

		KeySet * expected = testdata_demo_root ();

		succeed_if (plugin->kdbGet (plugin, ks, getKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");
		compare_keyset (expected, ks);

		ksDel (expected);

		keyDel (getKey);
		PLUGIN_CLOSE ();
	}

	{
		Key * setKey = keyNew ("system:/", KEY_VALUE, outfile, KEY_END);

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

	test_v1_oneValue ();
	test_v1_twoValue ();
	test_v1_threeValue ();
	test_v1_againTwoValue ();
	test_v1_metaData ();

	test_v2_oneValue ();
	test_v2_twoValue ();
	test_v2_threeValue ();
	test_v2_againTwoValue ();
	test_v2_metaData ();
	test_v2_fullnames ();
	test_v2_demo ();
	test_v2_demo_root ();

	print_result ("testmod_dump");

	return nbError;
}
