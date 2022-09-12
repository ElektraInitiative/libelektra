/**
 * @file
 *
 * @brief Tests for iterate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	ElektraKey * parentKey = keyNew ("user:/tests/iterate", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("iterate");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 0, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 0, "call to kdbSet was not successful");

	// clang-format off
	ksAppendKey (ks, keyNew("user:/tests/iterate/key",
				ELEKTRA_KEY_META, "iterate", "has",
				ELEKTRA_KEY_END));
	// clang-format on

	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	ksRewind (ks);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (plugin->kdbError (plugin, ks, parentKey) == 1, "call to kdbError was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("ITERATE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_iterate");

	return nbError;
}
