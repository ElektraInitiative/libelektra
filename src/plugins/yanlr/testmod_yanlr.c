/**
 * @file
 *
 * @brief Tests for yanlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <stdlib.h>

#include <tests_plugin.h>

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define INIT_PLUGIN(parent, filepath)                                                                                                      \
	Key * parentKey = keyNew (parent, KEY_VALUE, filepath, KEY_END);                                                                   \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("yanlr")

#define INIT_PLUGIN_GET(parent, filepath, errorMessage)                                                                                    \
	INIT_PLUGIN (parent, filepath);                                                                                                    \
	KeySet * keySet = ksNew (0, KS_END);                                                                                               \
	int status = plugin->kdbGet (plugin, keySet, parentKey);                                                                           \
	succeed_if (status == ELEKTRA_PLUGIN_STATUS_SUCCESS || status == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, errorMessage)

#define CLOSE_PLUGIN()                                                                                                                     \
	keyDel (parentKey);                                                                                                                \
	ksDel (keySet);                                                                                                                    \
	PLUGIN_CLOSE ()

// -- Functions  ---------------------------------------------------------------------------------------------------------------------------

static void test_contract (void)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]")))
#endif
{
	printf ("• Retrieve plugin contract\n");

	INIT_PLUGIN_GET ("system/elektra/modules/yanlr", "", "Could not retrieve plugin contract");
	CLOSE_PLUGIN ();
}

static void test_read (char const * const filepath, KeySet * const expected)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	printf ("• Retrieve data from file “%s”\n", filepath);

	INIT_PLUGIN_GET ("user/tests/yanlr", srcdir_file (filepath), "Unable to open or parse file");

	// We replace the value of the parent key of expected keyset, if the header file specifies the value @CONFIG_FILEPATH@.
	// We could also do that via CMake, but the current solution should be easier for now.
	Key * root = ksLookupByName (expected, "user/tests/yanlr", KDB_O_POP);
	if (root)
	{
		if (elektraStrCmp (keyString (root), "@CONFIG_FILEPATH@") == 0) keySetString (root, srcdir_file (filepath));
		ksAppendKey (expected, root);
	}

	compare_keyset (keySet, expected);

	ksDel (expected);
	CLOSE_PLUGIN ();
}

static void test_empty (void)
{
	test_read ("yanlr/null.yaml",
#include "yanlr/null.h"
	);
	test_read ("yanlr/comment.yaml",
#include "yanlr/null.h"
	);
}

static void test_scalar (void)
{
	test_read ("yanlr/plain_scalar-word_chars.yaml",
#include "yanlr/plain_scalar-word_chars.h"
	);
	test_read ("yanlr/plain_scalar-word_chars_space.yaml",
#include "yanlr/plain_scalar-word_chars_space.h"
	);
	test_read ("yanlr/single_quoted_scalar.yaml",
#include "yanlr/single_quoted_scalar.h"
	);
	test_read ("yanlr/double_quoted_scalar.yaml",
#include "yanlr/double_quoted_scalar.h"
	);
}

static void test_list (void)
{
	test_read ("yanlr/list-plain_scalars.yaml",
#include "yanlr/list-plain_scalars.h"
	);
	test_read ("yanlr/list-list_map-mixed_scalars.yaml",
#include "yanlr/list-list_map-mixed_scalars.h"
	);
}

static void test_map (void)
{
	test_read ("yanlr/map-null.yaml",
#include "yanlr/map-null.h"
	);
	test_read ("yanlr/map-plain_scalar.yaml",
#include "yanlr/map-plain_scalar.h"
	);
	test_read ("yanlr/map-plain_scalars.yaml",
#include "yanlr/map-plain_scalars.h"
	);
	test_read ("yanlr/map-list-plain_scalars.yaml",
#include "yanlr/map-list-plain_scalars.h"
	);
	test_read ("yanlr/map-map-plain_scalars.yaml",
#include "yanlr/map-map-plain_scalars.h"
	);
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char ** argv)
{
	printf ("🐜 YANLR     TESTS\n");
	printf ("===================\n\n");

	init (argc, argv);

	test_contract ();

	test_empty ();
	test_scalar ();
	test_map ();
	test_list ();

	print_result ("testmod_yanlr");

	return nbError;
}
