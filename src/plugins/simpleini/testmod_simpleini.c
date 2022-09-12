/**
 * @file
 *
 * @brief Tests for simpleini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>


static void test_readFormat (const char * format, const char * fileContent, int numKeys, const char ** keys, const char ** values)
{
	const char * tmpFile = elektraFilename ();
	FILE * fh = fopen (tmpFile, "w");
	if (fh)
	{
		fputs (fileContent, fh);
		fclose (fh);
	}

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/simpleini", ELEKTRA_KEY_VALUE, tmpFile, ELEKTRA_KEY_END);
	ElektraKeyset * conf = 0;
	if (format)
	{
		conf = elektraKeysetNew (1, elektraKeyNew ("system:/format", ELEKTRA_KEY_VALUE, format, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	}
	else
	{
		conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	}

	PLUGIN_OPEN ("simpleini");

	ElektraKeyset * ks = elektraKeysetNew (numKeys, ELEKTRA_KS_END);
	ElektraKey * key = 0;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	ElektraKey * lookup = 0;
	for (int i = 0; i < numKeys; i++)
	{
		lookup = elektraKeyNew ("user:/tests/simpleini", ELEKTRA_KEY_END);
		elektraKeyAddBaseName (lookup, keys[i]);
		printf ("testing key '%s'\n", elektraKeyBaseName (lookup));
		succeed_if ((key = elektraKeysetLookup (ks, lookup, 0)) != NULL, "key not found");
		succeed_if (strcmp (values[i], elektraKeyString (key)) == 0, "value of key did not match");
		elektraKeyDel (lookup);
	}

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_formatNotAccepted (const char * format)
{
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/simpleini", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (1, elektraKeyNew ("system:/format", ELEKTRA_KEY_VALUE, format, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	PLUGIN_OPEN ("simpleini");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != 1, "kdbGet was successful for an invalid format");

	elektraKeysetDel (ks);
	elektraKeyDel (parentKey);
	PLUGIN_CLOSE ();
}


static void test_readDefaultFormat (void)
{
	const char * expected_keys[] = { "key1", "key2", "key3", "key4" };
	const char * expected_values[] = { "value1", "value2", "value3", "value4 " };

	test_readFormat (NULL, // format
			 "key1 = value1\n"
			 " key2 = value2\n"
			 "key3  = value3\n"
			 "key4 = value4 \n",
			 4, expected_keys, expected_values);
}

static void test_readFormat_wo_space (void)
{

	const char * expected_keys[] = { "key1", "key2", "key3", "key4" };
	const char * expected_values[] = { "value1", "value2", "value3", " value4 " };

	test_readFormat ("%=%", // format
			 "key1=value1\n"
			 " key2=value2\n"
			 "key3 =value3\n"
			 "key4= value4 \n",
			 4, expected_keys, expected_values);
}

static void test_readFormat_special1 (void)
{

	const char * expected_keys[] = { "key1", "key2", "key3", "key4" };
	const char * expected_values[] = { "value1", "value2", "value3", " value4 " };

	test_readFormat ("% :=_%", // format
			 "key1 :=_value1\n"
			 " key2 :=_value2\n"
			 "key3  :=_value3\n"
			 "key4 :=_ value4 \n",
			 4, expected_keys, expected_values);
}


int main (int argc, char ** argv)
{
	printf ("SIMPLEINI    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_readDefaultFormat ();
	test_readFormat_wo_space ();
	test_readFormat_special1 ();
	test_formatNotAccepted ("%");
	test_formatNotAccepted ("");
	test_formatNotAccepted ("%%%");
	test_formatNotAccepted ("%% %");
	test_formatNotAccepted ("%% %%");


	print_result ("testmod_simpleini");

	return nbError;
}
