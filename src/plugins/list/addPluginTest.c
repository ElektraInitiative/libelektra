#include <kdb.h>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <kdbprivate.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void * getFunction (Plugin * plugin, const char * name)
{
	KeySet * exports = ksNew (0, KS_END);
	Key * pk = keyNew ("system/elektra/modules", KEY_END);
	keyAddBaseName (pk, plugin->name);
	plugin->kdbGet (plugin, exports, pk);
	ksRewind (exports);
	keyAddBaseName (pk, "exports");
	keyAddBaseName (pk, name);
	return keyValue (ksLookup (exports, pk, 0));
}

int main (int argc, char const * argv[])
{
	KeySet * modules = ksNew (0, KS_END);
	Key * parentKey = keyNew ("user/tests/list", KEY_END);
	KeySet * ks = ksNew (5, keyNew ("user/tests/list/to/be/cut/key1", KEY_END), keyNew ("user/tests/list/to/be/cut/key2", KEY_END),
			     keyNew ("user/tests/list/to/be/cut/meta1", KEY_END),
			     keyNew ("user/tests/list/to/be/cut/meta2", KEY_VALUE, "meta?", KEY_META, "convert/metaname", "amimetanow?",
				     KEY_META, "convert/append", "previous", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (20, keyNew ("user/placements", KEY_END),
			       keyNew ("user/placements/get", KEY_VALUE, "pregetstorage postgetstorage", KEY_END),
			       keyNew ("user/placements/set", KEY_VALUE, "presetstorage", KEY_END),
			       keyNew ("system/cut", KEY_VALUE, "to/be/cut", KEY_END), keyNew ("user/plugins", KEY_END),
			       keyNew ("user/plugins/#0", KEY_VALUE, "rename", KEY_END), keyNew ("user/plugins/#0/placements", KEY_END),
			       keyNew ("user/plugins/#0/placements/set", KEY_VALUE, "presetstorage", KEY_END),
			       keyNew ("user/plugins/#0/placements/get", KEY_VALUE, "pregetstorage", KEY_END),
			       keyNew ("user/plugins/#1", KEY_VALUE, "keytometa", KEY_END), keyNew ("user/plugins/#1/placements", KEY_END),
			       keyNew ("user/plugins/#1/placements/set", KEY_VALUE, "presetstorage", KEY_END),
			       keyNew ("user/plugins/#1/placements/get", KEY_VALUE, "pregetstorage", KEY_END), KS_END);

	Key * errorKey = keyNew ("", KEY_END);
	elektraModulesInit (modules, 0);
	Plugin * list = elektraPluginOpen ("list", modules, conf, errorKey);
	getFunction (list, "addPlugin");
	KeySet * exports = ksNew (0, KS_END);
	Key * pk = keyNew ("system/elektra/modules/list", KEY_END);
	list->kdbGet (list, exports, pk);
	ksRewind (exports);
	int rc = list->kdbGet (list, ks, parentKey);
	typedef int (*addPlugin) (Plugin *, void *);
	addPlugin addPtr = *(addPlugin **)getFunction (list, "addPlugin");
	typedef int (*editPlugin) (Plugin *, void *);
	addPlugin editPtr = *(editPlugin **)getFunction (list, "editPlugin");

	KeySet * appendPlugin =
		ksNew (20, keyNew ("user/plugins", KEY_END), keyNew ("user/plugins/#3", KEY_VALUE, "timeofday", KEY_END),
		       keyNew ("user/plugins/#3/placements", KEY_END),
		       keyNew ("user/plugins/#3/placements/get", KEY_VALUE, "pregetstorage postgetstorage", KEY_END), KS_END);
	addPtr (list, appendPlugin);
	rc = list->kdbGet (list, ks, parentKey);
	if (rc != 1)
	{
		fprintf (stderr, "kdb get failed\n");
		return;
	}
	const Key * key = ksLookupByName (ks, "user/tests/list/meta1", 0);
	if (!key)
	{
		fprintf (stderr, "failed1\n");
		return;
	}
	const Key * meta = keyGetMeta (key, "amimetanow?");
	if (!meta)
	{
		fprintf (stderr, "failed2\n");
		return;
	}
	KeySet * delPlugin = ksNew (20, keyNew ("user/plugins", KEY_END), keyNew ("user/plugins/#3", KEY_END),
				    keyNew ("user/plugins/#3/placements", KEY_END),
				    keyNew ("user/plugins/#3/placements/get", KEY_VALUE, "pregetstorage postgetstorage", KEY_END), KS_END);
	editPtr (list, delPlugin);
	rc = list->kdbGet (list, ks, parentKey);
	rc = list->kdbGet (list, ks, parentKey);

	ksRewind (modules);


	return 0;
}
