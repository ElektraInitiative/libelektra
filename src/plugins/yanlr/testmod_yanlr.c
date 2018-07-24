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

	compare_keyset (keySet, expected);

	ksDel (expected);
	CLOSE_PLUGIN ();
}

static void test_empty ()
{
	test_read ("yanlr/null.yaml",
#include "yanlr/null.h"
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

	print_result ("testmod_yanlr");

	return nbError;
}
