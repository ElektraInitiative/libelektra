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


void testRead (const char * filename, KeySet * expected)
{
	Key * parentKey = keyNew ("user/tests/toml-read", KEY_VALUE, srcdir_file (filename), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("toml");
	KeySet * ks = ksNew (0, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbGet failed");

	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	init (argc, argv);

    // TODO: proper testing with expectations
	// testRead ("toml/basic.toml", NULL);
    // testRead ("toml/table.toml", NULL);
    // testRead ("toml/array.toml", NULL);
    testRead ("toml/table_array_basic.toml", NULL);

	print_result ("testmod_toml");
	return nbError;
}
