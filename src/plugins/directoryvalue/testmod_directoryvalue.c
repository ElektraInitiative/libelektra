/**
 * @file
 *
 * @brief Tests for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#define INIT_PLUGIN(parent)                                                                                                                \
	Key * parentKey = keyNew (parent, KEY_END);                                                                                        \
	KeySet * conf = ksNew (0, KS_END);                                                                                                 \
	PLUGIN_OPEN ("directoryvalue");                                                                                                    \
	KeySet * keySet = ksNew (0, KS_END)

#define CLOSE_PLUGIN()                                                                                                                     \
	keyDel (parentKey);                                                                                                                \
	ksDel (keySet);                                                                                                                    \
	PLUGIN_CLOSE ()

static void test_contract (void)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]")))
#endif
{
	printf ("• Retrieve plugin contract\n");

	INIT_PLUGIN ("system/elektra/modules/directoryvalue");
	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "Could not retrieve plugin contract");
	CLOSE_PLUGIN ();
}

int main (int argc, char ** argv)
{
	printf ("Directory Value Tests\n");
	printf ("=====================\n\n");

	init (argc, argv);

	test_contract ();

	print_result ("testmod_directoryvalue");

	return nbError;
}
