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

#include <kdbconfig.h>

#include <tests_plugin.h>

static void testBom (const char * filename, int reject, int expected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/filecheck", ELEKTRA_KEY_VALUE, srcdir_file (filename), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = elektraKeysetNew (10, elektraKeyNew ("system:/reject/bom", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
}

static void testNull (const char * filename, int reject, int expected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/filecheck", ELEKTRA_KEY_VALUE, srcdir_file (filename), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = elektraKeysetNew (10, elektraKeyNew ("system:/reject/null", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
}

static void testLEConsistency (const char * filename, int reject, int expected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/filecheck", ELEKTRA_KEY_VALUE, srcdir_file (filename), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = elektraKeysetNew (10, elektraKeyNew ("system:/check/lineending", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
}

static void testLEcrlf (const char * filename, int reject, int expected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/filecheck", ELEKTRA_KEY_VALUE, srcdir_file (filename), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = elektraKeysetNew (10, elektraKeyNew ("system:/check/lineending", ELEKTRA_KEY_END),
			      elektraKeyNew ("system:/valid/lineending", ELEKTRA_KEY_VALUE, "CRLF", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
}

static void testEncoding (const char * filename, int reject, int expected)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/filecheck", ELEKTRA_KEY_VALUE, srcdir_file (filename), ELEKTRA_KEY_END);
	ElektraKeyset * conf;
	if (!reject)
	{
		conf = NULL;
	}
	else
	{
		conf = elektraKeysetNew (10, elektraKeyNew ("system:/check/encoding", ELEKTRA_KEY_END),
			      elektraKeyNew ("system:/valid/encoding", ELEKTRA_KEY_VALUE, "UTF-8", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("filecheck");
	int ret = plugin->kdbGet (plugin, ks, parentKey);
	succeed_if (ret == expected, "kdbGet failed");
	PLUGIN_CLOSE ();
	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
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
