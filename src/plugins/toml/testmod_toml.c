/**
 * @file
 *
 * @brief Tests for the toml plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdbprivate.h>
#include <tests.h>
#include <tests_plugin.h>

#include "toml.h"

#define PREFIX "user/tests/toml-read"

void testRead (const char * filename, KeySet * expected)
{
	Key * parentKey = keyNew (PREFIX, KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	Key * root = ksLookupByName (expected, PREFIX, KDB_O_POP);
	if (root != NULL)
	{
		if (strcmp (keyString (root), "@CONFIG_FILEPATH@") == 0)
		{
			keySetString (root, srcdir_file (filename));
			ksAppendKey (expected, root);
		}
	}
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	compare_keyset (expected, ks);

	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	init (argc, argv);

	{
#include "toml/basic.h"
		testRead ("toml/basic.toml", expected);
		ksDel (expected);
	}

	{
#include "toml/array.h"
		testRead ("toml/array.toml", expected);
		ksDel (expected);
	}
	{
#include "toml/table_array_basic.h"
		testRead ("toml/table_array_basic.toml", expected);
		ksDel (expected);
    }

	print_result ("testmod_toml");
	return nbError;
}
