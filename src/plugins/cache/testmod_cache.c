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

#include <kdb.h>
#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/cache", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("cache");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "call to kdbGet was successful, but file should not exist yet");

	// no global keyset, so not caching anything
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();
}

static void test_cacheNonBackendKeys (void)
{
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * key = elektraKeyNew ("user:/tests/cache", ELEKTRA_KEY_END);
	ElektraKdb * handle = elektraKdbOpen (NULL, key);

	// the key should be in the keyset, but should not be cached
	ElektraKey * doNotCache = elektraKeyNew ("user:/tests/cache/somekey", ELEKTRA_KEY_END);
	elektraKeysetAppendKey (conf, doNotCache);
	elektraKdbGet (handle, conf, key);
	ElektraKey * result = elektraKeysetLookupByName (conf, "user:/tests/cache/somekey", 0);
	succeed_if (result != 0, "key is missing from keyset");

	// the cached key should not have been persisted, so it is not in the fresh keyset
	ElektraKeyset * freshConf = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKdbGet (handle, freshConf, key);
	ElektraKey * freshResult = elektraKeysetLookupByName (freshConf, "user:/tests/cache/somekey", 0);
	succeed_if (freshResult == 0, "key was persisted/cached, even though it was not committed");
	elektraKeysetDel (freshConf);

	elektraKeyDel (key);
	elektraKeysetDel (conf);
	elektraKdbClose (handle, 0);
}


int main (int argc, char ** argv)
{
	printf ("CACHE     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_cacheNonBackendKeys ();

	print_result ("testmod_cache");

	return nbError;
}
