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
	// printf ("################################################\n");
	// printf ("############ testRead (%s)\n", filename);
	// printf ("################################################\n");
    printf("Reading '%s'\n", filename);
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

    /*ksRewind (ks);
    Key * key = ksNext(ks);
    while (key != NULL) {
        printf("Key: '%s'\t->\t'%s'", keyName(key), keyString(key));

        keyRewindMeta (key);
        Key * meta = keyNextMeta (key);
        while (meta != NULL) {
            printf("\n\tMeta: '%s'\t->\t'%s'", keyName(meta), keyString(meta));
            meta = keyNextMeta (key);
        }
        key = ksNext(ks);
        printf("\n");
    }*/

	PLUGIN_CLOSE ();
	ksDel (expected);
}


int main (int argc, char ** argv)
{
	init (argc, argv);

	testRead ("toml/basic.toml",
#include "toml/basic.h"
	);

	testRead ("toml/array.toml",
#include "toml/array.h"
	);
	
    testRead ("toml/simple_table.toml",
#include "toml/simple_table.h"
	);

	testRead ("toml/table_array.toml",
#include "toml/table_array.h"
	);

	testRead ("toml/table_array_nested.toml",
#include "toml/table_array_nested.h"
	);

    testRead ("toml/table_array_table_mixed.toml",
#include "toml/table_array_table_mixed.h"
    );

	testRead ("toml/inline_table.toml",
#include "toml/inline_table.h"
	);

    testRead ("toml/inline_table_empty.toml",
#include "toml/inline_table_empty.h"
    );

    testRead ("toml/inline_table_multiline_values.toml",
#include "toml/inline_table_multiline_values.h"
    );
    
    testRead ("toml/comment.toml",
#include "toml/comment.h"
    );

	print_result ("testmod_toml");
	return nbError;
}
