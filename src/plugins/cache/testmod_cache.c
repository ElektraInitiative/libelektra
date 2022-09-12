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

	ElektraKey * parentKey = keyNew ("user:/tests/cache", ELEKTRA_KEY_END);
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("cache");

	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR,
		    "call to kdbGet was successful, but file should not exist yet");

	// no global keyset, so not caching anything
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "call to kdbSet was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_cacheNonBackendKeys (void)
{
	ElektraKeyset * conf = ksNew (0, ELEKTRA_KS_END);

	ElektraKey * key = keyNew ("user:/tests/cache", ELEKTRA_KEY_END);
	ElektraKdb * handle = kdbOpen (NULL, key);

	// the key should be in the keyset, but should not be cached
	ElektraKey * doNotCache = keyNew ("user:/tests/cache/somekey", ELEKTRA_KEY_END);
	ksAppendKey (conf, doNotCache);
	kdbGet (handle, conf, key);
	ElektraKey * result = ksLookupByName (conf, "user:/tests/cache/somekey", 0);
	succeed_if (result != 0, "key is missing from keyset");

	// the cached key should not have been persisted, so it is not in the fresh keyset
	ElektraKeyset * freshConf = ksNew (0, ELEKTRA_KS_END);
	kdbGet (handle, freshConf, key);
	ElektraKey * freshResult = ksLookupByName (freshConf, "user:/tests/cache/somekey", 0);
	succeed_if (freshResult == 0, "key was persisted/cached, even though it was not committed");
	ksDel (freshConf);

	keyDel (key);
	ksDel (conf);
	kdbClose (handle, 0);
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
