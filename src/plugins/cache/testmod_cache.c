/**
 * @file
 *
 * @brief Tests for cache plugin
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

	Key * parentKey = keyNew ("user/tests/cache", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("cache");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "call to kdbGet was successful, but file should not exist yet");

	// no global keyset, so not caching anything
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}


int main (int argc, char ** argv)
{
	printf ("CACHE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();

	print_result ("testmod_cache");

	return nbError;
}
