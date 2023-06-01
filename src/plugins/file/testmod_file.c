/**
 * @file
 *
 * @brief Tests for file plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <internal/config.h>

#include <tests_plugin.h>


void testReadSingleLine (const char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/file", KEY_VALUE, srcdir_file (fileName), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("file");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	const Key * key = ksLookupByName (ks, "user:/tests/file", KDB_O_NONE);
	exit_if_fail (key, "key not found");

	succeed_if (!strcmp ("this is a single line testfile\n", keyString (key)), "read single line data doesn't match expected string");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void testReadMultiLine (const char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/file", KEY_VALUE, srcdir_file (fileName), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("file");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	const Key * key = ksLookupByName (ks, "user:/tests/file", KDB_O_NONE);
	exit_if_fail (key, "key not found");

	succeed_if (!strcmp ("\nthis\n\n\tis a\n   multi line test-\nfile\n\n", keyString (key)),
		    "read multiline data doesn't match expected string");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


void testWriteSingleLine (const char * compareTo)
{
	Key * parentKey = keyNew ("user:/tests/file", KEY_VALUE, elektraFilename (), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("file");

	KeySet * ks = ksNew (3, keyNew ("user:/tests/file", KEY_VALUE, "this is a single line testfile\n", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (compare_line_files (srcdir_file (compareTo), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}


void testWriteMultiLine (const char * compareTo)
{
	Key * parentKey = keyNew ("user:/tests/file", KEY_VALUE, elektraFilename (), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("file");

	KeySet * ks = ksNew (3, keyNew ("user:/tests/file", KEY_VALUE, "\nthis\n\n\tis a\n   multi line test-\nfile\n\n", KEY_END), KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (compare_line_files (srcdir_file (compareTo), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

void testRoundTrip (const char * fileName)
{
	Key * parentKey = keyNew ("user:/tests/file", KEY_VALUE, srcdir_file (fileName), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("file");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) >= 1, "call to kdbGet was not successful");

	keySetString (parentKey, elektraFilename ());

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), "files do not match as expected");

	ksDel (ks);
	keyDel (parentKey);

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("FILE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testReadSingleLine ("file/singleline");
	testReadMultiLine ("file/multiline");
	testWriteSingleLine ("file/singleline");
	testWriteMultiLine ("file/multiline");
	testRoundTrip ("file/multiline");

	print_result ("testmod_file");

	return nbError;
}
