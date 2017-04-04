/**
 * @file
 *
 * @brief Tests for mini plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include "values.h"

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define MAX_LENGTH_TEXT 200

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_basics ()
{
	printf ("• Test basic functionality of plugin\n");

	Key * parentKey = keyNew ("system/elektra/modules/mini", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mini");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == KEYSET_MODIFIED, "Could not retrieve plugin contract");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_get_simple ()
{
	char const * const fileName = "Examples/simple.ini";
	char const * const prefix = "user/mini/tests/read";

	printf ("• Parse file “%s”\n", fileName);

	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("mini");

	KeySet * keySet = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == KEYSET_MODIFIED, "Unable to open or parse file");
	succeed_if (output_error (parentKey), "Received unexpected error while reading the configuration");
	succeed_if (output_warnings (parentKey), "Received unexpected warning while reading the configuration");

	char keyValues[][2][50] = {
		{ "keyWithoutLeadingWhitespace", "valueWithLeadingWhiteSpace" },
		{ "keyWithLeadingWhitespace", "valueWithoutLeadingWhiteSpace" },
		{ "keyNoSpace", "valueNoSpace" },
		{ "wide", "open 	 spaces" },
	};
	Key * key;

	char text[MAX_LENGTH_TEXT];

	for (size_t pair = 0; pair < sizeof (keyValues) / sizeof (keyValues[0]); pair++)
	{
		char * name = keyValues[pair][0];
		char * value = keyValues[pair][1];
		snprintf (text, MAX_LENGTH_TEXT, "%s/%s", prefix, name);
		key = ksLookupByName (keySet, text, KDB_O_NONE);

		snprintf (text, MAX_LENGTH_TEXT, "Key “%s” not found", name);
		exit_if_fail (key, text);

		succeed_if_same_string (keyString (key), value);
	}

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
	test_get_simple ();

	printf ("\nResults: %d Test%s done — %d error%s.\n", nbTest, nbTest != 1 ? "s" : "", nbError, nbError != 1 ? "s" : "");

	return nbError;
}
