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

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/kdb/kdb.h>
#include <internal/kdb/config.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("user:/tests/cache", KEY_END);
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

static void test_cacheNonBackendKeys (void)
{
	KeySet * conf = ksNew (0, KS_END);

	Key * key = keyNew ("user:/tests/cache", KEY_END);
	KDB * handle = kdbOpen (NULL, key);

	// the key should be in the keyset, but should not be cached
	Key * doNotCache = keyNew ("user:/tests/cache/somekey", KEY_END);
	ksAppendKey (conf, doNotCache);
	kdbGet (handle, conf, key);
	Key * result = ksLookupByName (conf, "user:/tests/cache/somekey", 0);
	succeed_if (result != 0, "key is missing from keyset");

	// the cached key should not have been persisted, so it is not in the fresh keyset
	KeySet * freshConf = ksNew (0, KS_END);
	kdbGet (handle, freshConf, key);
	Key * freshResult = ksLookupByName (freshConf, "user:/tests/cache/somekey", 0);
	succeed_if (freshResult == 0, "key was persisted/cached, even though it was not committed");
	ksDel (freshConf);

	ksDel (conf);
	kdbClose (handle, key);
	keyDel (key);
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
