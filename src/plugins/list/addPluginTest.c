#include <kdb.h>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void * getFunction (Plugin * plugin, const char * name)
{
	ElektraKeyset * exports = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * pk = keyNew ("system:/elektra/modules", ELEKTRA_KEY_END);
	keyAddBaseName (pk, plugin->name);
	plugin->kdbGet (plugin, exports, pk);
	ksRewind (exports);
	keyAddBaseName (pk, "exports");
	keyAddBaseName (pk, name);
	return keyValue (ksLookup (exports, pk, 0));
}

int main (int argc, char const * argv[])
{
	ElektraKeyset * modules = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * parentKey = keyNew ("user:/tests/list", ELEKTRA_KEY_END);
	ElektraKeyset * ks = ksNew (5, keyNew ("user:/tests/list/to/be/cut/key1", ELEKTRA_KEY_END), keyNew ("user:/tests/list/to/be/cut/key2", ELEKTRA_KEY_END),
			     keyNew ("user:/tests/list/to/be/cut/meta1", ELEKTRA_KEY_END),
			     keyNew ("user:/tests/list/to/be/cut/meta2", ELEKTRA_KEY_VALUE, "meta?", ELEKTRA_KEY_META, "convert/metaname", "amimetanow?",
				     ELEKTRA_KEY_META, "convert/append", "previous", ELEKTRA_KEY_END),
			     ELEKTRA_KS_END);
	ElektraKeyset * conf =
		ksNew (20, keyNew ("user:/placements", ELEKTRA_KEY_END),
		       keyNew ("user:/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage postgetstorage", ELEKTRA_KEY_END),
		       keyNew ("user:/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		       keyNew ("system:/cut", ELEKTRA_KEY_VALUE, "to/be/cut", ELEKTRA_KEY_END), keyNew ("user:/plugins", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#0", ELEKTRA_KEY_VALUE, "rename", ELEKTRA_KEY_END), keyNew ("user:/plugins/#0/placements", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#0/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#0/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#1", ELEKTRA_KEY_VALUE, "keytometa", ELEKTRA_KEY_END), keyNew ("user:/plugins/#1/placements", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#1/placements/set", ELEKTRA_KEY_VALUE, "presetstorage", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#1/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKey * errorKey = keyNew ("/", ELEKTRA_KEY_END);
	elektraModulesInit (modules, 0);
	Plugin * list = elektraPluginOpen ("list", modules, conf, errorKey);
	getFunction (list, "addPlugin");
	ElektraKeyset * exports = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * pk = keyNew ("system:/elektra/modules/list", ELEKTRA_KEY_END);
	list->kdbGet (list, exports, pk);
	ksRewind (exports);
	int rc = list->kdbGet (list, ks, parentKey);
	typedef int (*addPlugin) (Plugin *, void *);
	addPlugin addPtr = *(addPlugin **) getFunction (list, "addPlugin");
	typedef int (*editPlugin) (Plugin *, void *);
	addPlugin editPtr = *(editPlugin **) getFunction (list, "editPlugin");

	ElektraKeyset * appendPlugin =
		ksNew (20, keyNew ("user:/plugins", ELEKTRA_KEY_END), keyNew ("user:/plugins/#3", ELEKTRA_KEY_VALUE, "timeofday", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#3/placements", ELEKTRA_KEY_END),
		       keyNew ("user:/plugins/#3/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage postgetstorage", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	addPtr (list, appendPlugin);
	rc = list->kdbGet (list, ks, parentKey);
	if (rc != 1)
	{
		fprintf (stderr, "kdb get failed\n");
		return;
	}
	const ElektraKey * key = ksLookupByName (ks, "user:/tests/list/meta1", 0);
	if (!key)
	{
		fprintf (stderr, "failed1\n");
		return;
	}
	const ElektraKey * meta = keyGetMeta (key, "amimetanow?");
	if (!meta)
	{
		fprintf (stderr, "failed2\n");
		return;
	}
	ElektraKeyset * delPlugin = ksNew (20, keyNew ("user:/plugins", ELEKTRA_KEY_END), keyNew ("user:/plugins/#3", ELEKTRA_KEY_END),
				    keyNew ("user:/plugins/#3/placements", ELEKTRA_KEY_END),
				    keyNew ("user:/plugins/#3/placements/get", ELEKTRA_KEY_VALUE, "pregetstorage postgetstorage", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	editPtr (list, delPlugin);
	rc = list->kdbGet (list, ks, parentKey);
	rc = list->kdbGet (list, ks, parentKey);

	ksRewind (modules);


	return 0;
}
