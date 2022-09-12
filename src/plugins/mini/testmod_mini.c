/**
 * @file
 *
 * @brief Tests for mini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <tests_plugin.h>

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define MAX_LENGTH_TEXT 500

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_basics (void)
{
	printf ("• Test basic functionality of plugin\n");

	ElektraKey * parentKey = keyNew ("system:/elektra/modules/mini", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mini");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_get (void)
{
	char const * const fileName = "mini/read.ini";
	printf ("• Parse file “%s”\n", fileName);

	char const * const prefix = "user:/mini/tests/read";
	ElektraKey * parentKey = keyNew (prefix, ELEKTRA_KEY_VALUE, srcdir_file (fileName), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mini");

	ElektraKeyset * keySet = ksNew (0, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to open or parse file");
	succeed_if (output_error (parentKey), "Received unexpected error while reading the configuration");

	char keyValues[][2][50] = {
		{ "keyWithoutLeadingWhitespace", "valueWithLeadingWhiteSpace" },
		{ "keyWithLeadingWhitespace", "valueWithoutLeadingWhiteSpace" },
		{ "keyNoSpace", "valueNoSpace" },
		{ "wide", "open 	 spaces" },
		{ "key containing space", "value" },
		{ "empty", "" },
		{ "esc\\/a\\/ped/level1/level2", "🌻" },
	};
	ElektraKey * key;
	char text[MAX_LENGTH_TEXT];
	for (size_t pair = 0; pair < sizeof (keyValues) / sizeof (keyValues[0]); pair++)
	{
		ElektraKey * reference = keyNew (prefix, ELEKTRA_KEY_VALUE, keyValues[pair][1], ELEKTRA_KEY_END);
		keyAddName (reference, keyValues[pair][0]);
		key = ksLookupByName (keySet, keyName (reference), ELEKTRA_KDB_O_NONE);

		snprintf (text, MAX_LENGTH_TEXT, "key “%.100s” not found", keyName (reference));
		exit_if_fail (key, text);

		succeed_if_same_string (keyString (key), keyString (reference));
		keyDel (reference);
	}

	keyDel (parentKey);
	ksDel (keySet);
	PLUGIN_CLOSE ();
}

static void test_set (void)
{
	printf ("• Write configuration data\n");

	char const * const fileName = "mini/write.ini";
	char const * const prefix = "user:/mini/tests/write";

	ElektraKey * parentKey = keyNew (prefix, ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("mini");

	char keyValues[][2][50] = {
		{ "key", "value" }, { "space", "wide open	 spaces" }, { "empty", "" }, { "esc\\/aped/level1/", "🐌" }
	};
	char text[MAX_LENGTH_TEXT];
	ElektraKeyset * keySet = ksNew (0, ELEKTRA_KS_END);
	for (size_t pair = 0; pair < sizeof (keyValues) / sizeof (keyValues[0]); pair++)
	{
		char * name = keyValues[pair][0];
		char * value = keyValues[pair][1];
		snprintf (text, MAX_LENGTH_TEXT, "%s/%s", prefix, name);
		ksAppendKey (keySet, keyNew (text, ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END));
	}

	succeed_if (plugin->kdbSet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Unable to write to file");
	succeed_if (output_error (parentKey), "Received unexpected error while writing the configuration");
	succeed_if (output_warnings (parentKey), "Received unexpected warning while writing the configuration");

	snprintf (text, MAX_LENGTH_TEXT, "Output of plugin stored in “%s” does not match the expected output stored in “%s”",
		  keyString (parentKey), srcdir_file (fileName));
	succeed_if (compare_line_files (srcdir_file (fileName), keyString (parentKey)), text);

	keyDel (parentKey);
	ksDel (keySet);
	PLUGIN_CLOSE ();
}

/* -- Main ------------------------------------------------------------------------------------------------------------------------------ */

int main (int argc, char ** argv)
{
	printf ("mINI Tests 🚙\n");
	printf ("==============\n\n");

	init (argc, argv);

	test_basics ();
	test_get ();
	test_set ();

	print_result ("testmod_mini");

	return nbError;
}
