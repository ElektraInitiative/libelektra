/**
 * @file
 *
 * @brief Tests for filecheck plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <internal/kdb/config.h>

#include <tests_plugin.h>

static void testBom (const char * filename, int reject, int expected)
{
	Key * parentKey = keyNew ("user:/tests/filecheck", KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = ksNew (10, keyNew ("system:/reject/bom", KEY_END), KS_END);
	}
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	ksDel (ks);
	keyDel (parentKey);
}

static void testNull (const char * filename, int reject, int expected)
{
	Key * parentKey = keyNew ("user:/tests/filecheck", KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = ksNew (10, keyNew ("system:/reject/null", KEY_END), KS_END);
	}
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	ksDel (ks);
	keyDel (parentKey);
}

static void testLEConsistency (const char * filename, int reject, int expected)
{
	Key * parentKey = keyNew ("user:/tests/filecheck", KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = ksNew (10, keyNew ("system:/check/lineending", KEY_END), KS_END);
	}
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	ksDel (ks);
	keyDel (parentKey);
}

static void testLEcrlf (const char * filename, int reject, int expected)
{
	Key * parentKey = keyNew ("user:/tests/filecheck", KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = ksNew (10, keyNew ("system:/check/lineending", KEY_END),
			      keyNew ("system:/valid/lineending", KEY_VALUE, "CRLF", KEY_END), KS_END);
	}
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	ksDel (ks);
	keyDel (parentKey);
}

static void testEncoding (const char * filename, int reject, int expected)
{
	Key * parentKey = keyNew ("user:/tests/filecheck", KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = ksNew (10, keyNew ("system:/check/encoding", KEY_END),
			      keyNew ("system:/valid/encoding", KEY_VALUE, "UTF-8", KEY_END), KS_END);
	}
	KeySet * ks = ksNew (0, KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	ksDel (ks);
	keyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("FILECHECK	   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testBom ("filecheck/BOMFILE", 0, 1);
	testBom ("filecheck/BOMFILE", 1, (-1));
	testBom ("filecheck/NOBOMFILE", 0, 1);
	testBom ("filecheck/NOBOMFILE", 1, 1);
	testNull ("filecheck/NULLBYTE", 0, 1);
	testNull ("filecheck/NULLBYTE", 1, (-1));
	testNull ("filecheck/NONULLBYTE", 0, 1);
	testNull ("filecheck/NONULLBYTE", 1, 1);
	testLEConsistency ("filecheck/inconsistent", 0, 1);
	testLEConsistency ("filecheck/inconsistent", 1, (-1));
	testLEcrlf ("filecheck/valid1", 1, 1);
	testLEcrlf ("filecheck/invalid", 1, (-1));

	testEncoding ("filecheck/utf.txt", 1, 1);
	testEncoding ("filecheck/iso.txt", 1, (-1));

	print_result ("testmod_filecheck");

	return nbError;
}
