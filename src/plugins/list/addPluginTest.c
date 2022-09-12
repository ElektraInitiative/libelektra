#include <kdb.h>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void * getFunction (Plugin * plugin, const char * name)
{
	ElektraKeyset * exports = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * pk = elektraKeyNew ("system:/elektra/modules", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (pk, plugin->name);
	plugin->kdbGet (plugin, exports, pk);
	elektraKeysetRewind (exports);
	elektraKeyAddBaseName (pk, "exports");
	elektraKeyAddBaseName (pk, name);
	return elektraKeyValue (elektraKeysetLookup (exports, pk, 0));
}

int main (int argc, char const * argv[])
{
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * parentKey = elektraKeyNew ("user:/tests/list", ELEKTRA_KEY_END);
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew ("user:/tests/list/to/be/cut/key1", ELEKTRA_KEY_END), elektraKeyNew ("user:/tests/list/to/be/cut/key2", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/list/to/be/cut/meta1", ELEKTRA_KEY_END),
			     elektraKeyNew ("user:/tests/list/to/be/cut/meta2", ELEKTRA_KEY_VALUE, "meta?", ELEKTRA_KEY_META, "convert/metaname", "amimetanow?",
				     ELEKTRA_KEY_META, "convert/append", "previous", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf =
		elektraKeysetNew (20, elektraKeyNew ("user:/placements", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage postgetstorage", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		       elektraKeyNew ("system:/cut", ELEKTRA_KEY_VALUE, "to/be/cut", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#0", ELEKTRA_KEY_VALUE, "rename", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#0/placements", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#0/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#0/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#1", ELEKTRA_KEY_VALUE, "keytometa", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#1/placements", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#1/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#1/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraModulesInit (modules, 0);
	Plugin * list = elektraPluginOpen ("list", modules, conf, errorKey);
	getFunction (list, "addPlugin");
	ElektraKeyset * exports = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * pk = elektraKeyNew ("system:/elektra/modules/list", ELEKTRA_KEY_END);
	list->kdbGet (list, exports, pk);
	elektraKeysetRewind (exports);
	int rc = list->kdbGet (list, ks, parentKey);
	typedef int (*addPlugin) (Plugin *, void *);
	addPlugin addPtr = *(addPlugin **) getFunction (list, "addPlugin");
	typedef int (*editPlugin) (Plugin *, void *);
	addPlugin editPtr = *(editPlugin **) getFunction (list, "editPlugin");

	ElektraKeyset * appendPlugin =
		elektraKeysetNew (20, elektraKeyNew ("user:/plugins", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#3", ELEKTRA_KEY_VALUE, "timeofday", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#3/placements", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/plugins/#3/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage postgetstorage", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	addPtr (list, appendPlugin);
	rc = list->kdbGet (list, ks, parentKey);
	if (rc != 1)
	{
		fprintf (stderr, "kdb get failed\n");
		return;
	}
	const ElektraKey * key = elektraKeysetLookupByName (ks, "user:/tests/list/meta1", 0);
	if (!key)
	{
		fprintf (stderr, "failed1\n");
		return;
	}
	const ElektraKey * meta = elektraKeyGetMeta (key, "amimetanow?");
	if (!meta)
	{
		fprintf (stderr, "failed2\n");
		return;
	}
	ElektraKeyset * delPlugin = elektraKeysetNew (20, elektraKeyNew ("user:/plugins", ELEKTRA_KEY_END), elektraKeyNew ("user:/plugins/#3", ELEKTRA_KEY_END),
				    elektraKeyNew ("user:/plugins/#3/placements", ELEKTRA_KEY_END),
				    elektraKeyNew ("user:/plugins/#3/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage postgetstorage", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	editPtr (list, delPlugin);
	rc = list->kdbGet (list, ks, parentKey);
	rc = list->kdbGet (list, ks, parentKey);

	elektraKeysetRewind (modules);


	return 0;
}
