/**
 * @file
 *
 * @brief Tests for list plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void doTest (void)
{
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/list/to/be/cut/key1", ELEKTRA_KEY_END), elektraKeyNew ("user:/tests/list/to/be/cut/key2", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/list/to/be/cut/meta1", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/list/to/be/cut/meta2", ELEKTRA_KEY_VALUE, "meta?", ELEKTRA_KEY_META, "convert/metaname", "amimetanow?",
				     ELEKTRA_KEY_META, "convert/append", "previous", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf = elektraKeysetNew (
		20, elektraKeyNew ("user:/placements", ELEKTRA_KEY_END), elektraKeyNew ("user:/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/plugins/#0", ELEKTRA_KEY_VALUE, "rename", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#0/config", ELEKTRA_KEY_END),
		// Configure "rename" plugin directly to cut a part of the key name
		elektraKeyNew ("user:/plugins/#0/config/cut", ELEKTRA_KEY_VALUE, "to/be/cut", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#0/placements", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/plugins/#0/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/plugins/#0/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/plugins/#1", ELEKTRA_KEY_VALUE, "keytometa", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#1/placements", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/plugins/#1/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		elektraKeyNew ("user:/plugins/#1/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END),
		// For all plugins, but only used by "rename" plugin
		elektraKeyNew ("user:/config", ELEKTRA_KEY_END), elektraKeyNew ("user:/config/toupper", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/list", ELEKTRA_KEY_END);
	elektraKeysetAppendKey (ks, parentKey);
	PLUGIN_OPEN ("list");

	Plugin * check = elektraPluginOpen ("keytometa", modules, elektraKeysetNew (5, ELEKTRA_KS_END), parentKey);
	if (!check)
	{
		printf ("Abort test case, keytometa is missing");
		goto end;
	}
	elektraPluginClose (check, 0);

	check = elektraPluginOpen ("rename", modules, elektraKeysetNew (5, ELEKTRA_KS_END), parentKey);
	if (!check)
	{
		printf ("Abort test case, rename is missing");
		goto end;
	}
	elektraPluginClose (check, 0);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbget failed");
	const ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/list/META1", 0);
	succeed_if (key, "key not found");
	const ElektraKey * meta = elektraKeyGetMeta (key, "amimetanow?");
	succeed_if (meta, "metakey not found");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "kdbset failed");
	key = elektraKeysetLookupByName (ks, "user:/tests/list/to/be/cut/meta1", 0);
	succeed_if (key, "key not found");

end:
	PLUGIN_CLOSE ();
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("LIST     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	doTest ();

	print_result ("testmod_list");

	return nbError;
}
