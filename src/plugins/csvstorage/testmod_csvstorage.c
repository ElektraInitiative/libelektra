/**
 * @file
 *
 * @brief Tests for csvstorage plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <elektra/kdbease.h>
#include <elektra/kdbhelper.h>

#include <tests_plugin.h>

static void testread (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END),
			       keyNew ("system:/header", KEY_VALUE, "colname", KEY_END), KS_END);
	PLUGIN_OPEN ("csvstorage");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key;
	key = ksLookupByName (ks, "user:/tests/csvstorage/#1/col1", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (keyString (key), "l1c1") == 0, "wrong key");
	key = ksLookupByName (ks, "user:/tests/csvstorage/#2/col2", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (keyString (key), "l2c2") == 0, "wrong key");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}
static void testreadfixcolcount (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf =
		ksNew (20, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END), keyNew ("system:/header", KEY_VALUE, "colname", KEY_END),
		       keyNew ("system:/columns", KEY_VALUE, "4", KEY_END), KS_END);
	PLUGIN_OPEN ("csvstorage");
	KeySet * ks = ksNew (0, KS_END);
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == (-1), "call to kdbGet was successful but shouldn't have been");
	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}
static void testreadwriteinvalid (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END), KS_END);
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!output_warnings (parentKey), "no warnings in kdbGet");
	keySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "error: wrote invalid data");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void testwriteinvalidheader (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END),
			       keyNew ("system:/header", KEY_VALUE, "colname", KEY_END), KS_END);

	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!output_warnings (parentKey), "no warnings in kdbGet");
	keySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "error: wrote invalid data");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testwritevalidemptycol (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (20, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END),
			       keyNew ("system:/header", KEY_VALUE, "colname", KEY_END), KS_END);

	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	keySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "error: couldn't write data");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void testSetColnames (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf =
		ksNew (20, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END), keyNew ("system:/header", KEY_VALUE, "colname", KEY_END),
		       keyNew ("system:/columns", KEY_VALUE, "2", KEY_END), keyNew ("system:/columns/names", KEY_VALUE, "", KEY_END),
		       keyNew ("system:/columns/names/#0", KEY_VALUE, "col0Name", KEY_END),
		       keyNew ("system:/columns/names/#1", KEY_VALUE, "col1Name", KEY_END), KS_END);
	PLUGIN_OPEN ("csvstorage");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	Key * key;
	key = ksLookupByName (ks, "user:/tests/csvstorage/#1/col0Name", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (keyString (key), "l1c1") == 0, "wrong key");
	key = ksLookupByName (ks, "user:/tests/csvstorage/#2/col1Name", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (keyString (key), "l2c2") == 0, "wrong key");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void testreadwritecomplicated (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END),
			       keyNew ("system:/header", KEY_VALUE, "colname", KEY_END), KS_END);
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/csvstorage/#1/col3", KDB_O_NONE)), "l1;c3"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/csvstorage/#1/col4", KDB_O_NONE)), "l1\"\"c4"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/csvstorage/#2/col3", KDB_O_NONE)), "l2\nc3"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/csvstorage/#4/col4", KDB_O_NONE)), "l4\"\"c4"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/csvstorage/#5/col3", KDB_O_NONE)), "l5\"\"\nc3"),
		    "key value doesn't match expected value");

	keySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "error: wrote invalid data");
	succeed_if (compare_line_files (srcdir_file (file), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testreadunescapedDQuote (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END),
			       keyNew ("system:/header", KEY_VALUE, "colname", KEY_END), KS_END);
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!output_warnings (parentKey), "no warnings in kdbGet");
	succeed_if (!strcmp (keyString (ksLookupByName (ks, "user:/tests/csvstorage/#3/col3", KDB_O_NONE)), "l3\"c3"),
		    "key value doesn't match expected value");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testexportmissing (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END), keyNew ("system:/export", KEY_VALUE, "", KEY_END),
			       keyNew ("system:/export/a", KEY_VALUE, "", KEY_END), KS_END);
	KeySet * ks = ksNew (10, keyNew ("user:/tests/csvstorage/#0", KEY_VALUE, "", KEY_END),
			     keyNew ("user:/tests/csvstorage/#0/a", KEY_VALUE, "a1", KEY_END),
			     keyNew ("user:/tests/csvstorage/#0/b", KEY_VALUE, "b", KEY_END),
			     keyNew ("user:/tests/csvstorage/#1", KEY_VALUE, "", KEY_END),
			     keyNew ("user:/tests/csvstorage/#1/a", KEY_VALUE, "a2", KEY_END), KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (file), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testKeyMetaKeyIsSet (const char * file)
{
	Key * parentKey = keyNew ("user:/tests/csvstorage", KEY_VALUE, srcdir_file (file), KEY_END);
	KeySet * conf = ksNew (10, keyNew ("system:/delimiter", KEY_VALUE, ";", KEY_END), keyNew ("system:/export", KEY_VALUE, "", KEY_END),
			       keyNew ("system:/export/a", KEY_VALUE, "", KEY_END), KS_END);
	PLUGIN_OPEN ("csvstorage");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	Key * key;
	key = ksLookupByName (ks, "user:/tests/csvstorage/#0", 0);
	succeed_if (keyGetMeta (key, "array") != 0, "metakey not found");
	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("CSVSTORAGE     TESTS\n");
	printf ("====================\n\n");
	printf ("Some tests report warnings because of invalid CSV files\n");

	init (argc, argv);

	testread ("csvstorage/valid.csv");
	testread ("csvstorage/validDos.csv");
	testreadfixcolcount ("csvstorage/valid.csv");
	testreadwriteinvalid ("csvstorage/invalid_columns.csv");
	testwriteinvalidheader ("csvstorage/invalid_columns_header2.csv");
	testwritevalidemptycol ("csvstorage/valid_empty_col.csv");
	testSetColnames ("csvstorage/valid.csv");
	testreadwritecomplicated ("csvstorage/complicated.csv");
	testreadunescapedDQuote ("csvstorage/unescapedQuote.csv");
	testexportmissing ("csvstorage/exporttest.csv");
	testKeyMetaKeyIsSet ("csvstorage/metakey.csv");
	print_result ("testmod_csvstorage");

	return nbError;
}
