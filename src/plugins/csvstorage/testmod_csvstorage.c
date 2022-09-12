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

#include <kdbease.h>
#include <kdbhelper.h>

#include <tests_plugin.h>

static void testread (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key;
	key = elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#1/col1", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (elektraKeyString (key), "l1c1") == 0, "wrong key");
	key = elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#2/col2", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (elektraKeyString (key), "l2c2") == 0, "wrong key");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}
static void testreadfixcolcount (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		elektraKeysetNew (20, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END), elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/columns", ELEKTRA_KEY_VALUE, "4", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == (-1), "call to kdbGet was successful but shouldn't have been");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}
static void testreadwriteinvalid (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!output_warnings (parentKey), "no warnings in kdbGet");
	elektraKeySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "error: wrote invalid data");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void testwriteinvalidheader (const char * file)
{

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!output_warnings (parentKey), "no warnings in kdbGet");
	elektraKeySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == (-1), "error: wrote invalid data");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testwritevalidemptycol (const char * file)
{

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (20, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	elektraKeySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "error: couldn't write data");
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}
static void testSetColnames (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf =
		elektraKeysetNew (20, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END), elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/columns", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END), elektraKeyNew ("system:/columns/names", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/columns/names/#0", ELEKTRA_KEY_VALUE, "col0Name", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/columns/names/#1", ELEKTRA_KEY_VALUE, "col1Name", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");
	ElektraKey * key;
	key = elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#1/col0Name", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (elektraKeyString (key), "l1c1") == 0, "wrong key");
	key = elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#2/col1Name", 0);
	exit_if_fail (key, "key not found");
	succeed_if (strcmp (elektraKeyString (key), "l2c2") == 0, "wrong key");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);

	PLUGIN_CLOSE ();
}

static void testreadwritecomplicated (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!strcmp (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#1/col3", ELEKTRA_KDB_O_NONE)), "l1;c3"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#1/col4", ELEKTRA_KDB_O_NONE)), "l1\"\"c4"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#2/col3", ELEKTRA_KDB_O_NONE)), "l2\nc3"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#4/col4", ELEKTRA_KDB_O_NONE)), "l4\"\"c4"),
		    "key value doesn't match expected value");
	succeed_if (!strcmp (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#5/col3", ELEKTRA_KDB_O_NONE)), "l5\"\"\nc3"),
		    "key value doesn't match expected value");

	elektraKeySetString (parentKey, elektraFilename ());
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "error: wrote invalid data");
	succeed_if (compare_line_files (srcdir_file (file), elektraKeyString (parentKey)), "files do not match as expected");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testreadunescapedDQuote (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, srcdir_file (file), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/header", ELEKTRA_KEY_VALUE, "colname", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) > 0, "call to kdbGet was not successful");
	succeed_if (!output_warnings (parentKey), "no warnings in kdbGet");
	succeed_if (!strcmp (elektraKeyString (elektraKeysetLookupByName (ks, "user:/tests/csvstorage/#3/col3", ELEKTRA_KDB_O_NONE)), "l3\"c3"),
		    "key value doesn't match expected value");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void testexportmissing (const char * file)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/csvstorage", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (10, elektraKeyNew ("system:/delimiter", ELEKTRA_KEY_VALUE, ";", ELEKTRA_KEY_END), elektraKeyNew ("system:/export", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/export/a", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew ("user:/tests/csvstorage/#0", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/csvstorage/#0/a", ELEKTRA_KEY_VALUE, "a1", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/csvstorage/#0/b", ELEKTRA_KEY_VALUE, "b", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/csvstorage/#1", ELEKTRA_KEY_VALUE, "", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/csvstorage/#1/a", ELEKTRA_KEY_VALUE, "a2", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("csvstorage");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "call to kdbSet was not successful");
	succeed_if (compare_line_files (srcdir_file (file), elektraKeyString (parentKey)), "files do not match as expected");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
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
	print_result ("testmod_csvstorage");

	return nbError;
}
