/**
 * @file
 *
 * @brief Tests for antlr plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics (void)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]")))
#endif
{
	printf ("• Test basic functionality\n");

	Key * parentKey = keyNew ("user/tests/antlr", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("antlr");

	KeySet * keySet = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbGet was not successful");
	succeed_if (plugin->kdbSet (plugin, keySet, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	keyDel (parentKey);
	ksDel (keySet);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("🐜 ANTLR     TESTS\n");
	printf ("===================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_antlr");

	return nbError;
}
