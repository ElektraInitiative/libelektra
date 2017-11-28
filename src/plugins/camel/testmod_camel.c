/**
 * @file
 *
 * @brief Tests for camel plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

/* -- Imports --------------------------------------------------------------------------------------------------------------------------- */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

/* -- Macros ---------------------------------------------------------------------------------------------------------------------------- */

#define MAX_LENGTH_TEXT 500

/* -- Functions ------------------------------------------------------------------------------------------------------------------------- */

static void test_basics (void)
{
	printf ("• Test basic functionality of plugin\n");

	Key * parentKey = keyNew ("system/elektra/modules/camel", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("camel");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_get (void)
{
	char const * const fileName = "camel/simple.yaml";
	printf ("• Parse file “%s”\n", fileName);

	char const * const prefix = "user/camel/tests/read";
	Key * parentKey = keyNew (prefix, KEY_VALUE, srcdir_file (fileName), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("camel");

	KeySet * keySet = ksNew (0, KS_END);

	int status = plugin->kdbGet (plugin, keySet, parentKey);

	succeed_if (status == ELEKTRA_PLUGIN_STATUS_SUCCESS || status == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Unable to open or parse file");
	succeed_if (output_error (parentKey), "Received unexpected error while reading the configuration");

	char keyValues[][2][50] = {
		{ "hello", "world" },
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

// ========
// = Main =
// ========

int main (int argc, char ** argv)
{
	printf ("🐪 Camel Tests\n");
	printf ("===============\n\n");

	init (argc, argv);

	test_basics ();
	test_get ();

	print_result ("testmod_camel");

	return nbError;
}
