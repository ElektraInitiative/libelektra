/**
 * @file
 *
 * @brief Tests for yamlsmith plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <tests_plugin.h>

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define INIT_PLUGIN(parent, errorMessage)                                                                                                  \
	Key * parentKey = keyNew (parent, KEY_END);                                                                                        \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("yamlsmith")                                                                                                          \
	KeySet * keySet = ksNew (0, KS_END);                                                                                               \
	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, errorMessage)

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

	INIT_PLUGIN ("system:/elektra/modules/yamlsmith", "Could not retrieve plugin contract");
	CLOSE_PLUGIN ();
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char ** argv)
{
	printf ("⚒ YAMLSMITH     TESTS\n");
	printf ("=======================\n\n");

	init (argc, argv);

	test_contract ();

	print_result ("testmod_yamlsmith");

	return nbError;
}
