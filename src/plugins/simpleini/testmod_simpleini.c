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

#include <internal/config.h>

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

	Key * parentKey = keyNew ("user:/tests/simpleini", KEY_VALUE, tmpFile, KEY_END);
	KeySet * conf = 0;
	if (format)
	{
		conf = ksNew (1, keyNew ("system:/format", KEY_VALUE, format, KEY_END), KS_END);
	}
	else
	{
		conf = ksNew (0, KS_END);
	}

	PLUGIN_OPEN ("simpleini");

	KeySet * ks = ksNew (numKeys, KS_END);
	Key * key = 0;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet was not successful");

	Key * lookup = 0;
	for (int i = 0; i < numKeys; i++)
	{
		lookup = keyNew ("user:/tests/simpleini", KEY_END);
		keyAddBaseName (lookup, keys[i]);
		printf ("testing key '%s'\n", keyBaseName (lookup));
		succeed_if ((key = ksLookup (ks, lookup, 0)) != NULL, "key not found");
		succeed_if (strcmp (values[i], keyString (key)) == 0, "value of key did not match");
		keyDel (lookup);
	}

	ksDel (ks);
	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

static void test_formatNotAccepted (const char * format)
{
	Key * parentKey = keyNew ("user:/tests/simpleini", KEY_VALUE, elektraFilename (), KEY_END);
	KeySet * conf = ksNew (1, keyNew ("system:/format", KEY_VALUE, format, KEY_END), KS_END);
	PLUGIN_OPEN ("simpleini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) != 1, "kdbGet was successful for an invalid format");

	ksDel (ks);
	keyDel (parentKey);
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
