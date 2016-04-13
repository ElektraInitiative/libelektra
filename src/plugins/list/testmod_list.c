/**
 * @file
 *
 * @brief Tests for list plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void doTest ()
{
	KeySet * ks = ksNew (5, keyNew ("user/tests/list/to/be/cut/key1", KEY_END), keyNew ("user/tests/list/to/be/cut/key2", KEY_END),
			     keyNew ("user/tests/list/to/be/cut/meta1", KEY_END),
			     keyNew ("user/tests/list/to/be/cut/meta2", KEY_VALUE, "meta?", KEY_META, "convert/metaname", "amimetanow?",
				     KEY_META, "convert/append", "previous", KEY_END),
			     KS_END);
	KeySet * conf = ksNew (20, keyNew ("user/placements", KEY_END), keyNew ("user/placements/get", KEY_VALUE, "pregetstorage", KEY_END),
			       keyNew ("user/placements/set", KEY_VALUE, "presetstorage", KEY_END),
			       keyNew ("system/cut", KEY_VALUE, "to/be/cut", KEY_END), keyNew ("user/plugins", KEY_END),
			       keyNew ("user/plugins/#0", KEY_VALUE, "rename", KEY_END), keyNew ("user/plugins/#0/placements", KEY_END),
			       keyNew ("user/plugins/#0/placements/set", KEY_VALUE, "presetstorage", KEY_END),
			       keyNew ("user/plugins/#0/placements/get", KEY_VALUE, "pregetstorage", KEY_END),
			       keyNew ("user/plugins/#1", KEY_VALUE, "keytometa", KEY_END), keyNew ("user/plugins/#1/placements", KEY_END),
			       keyNew ("user/plugins/#1/placements/set", KEY_VALUE, "presetstorage", KEY_END),
			       keyNew ("user/plugins/#1/placements/get", KEY_VALUE, "pregetstorage", KEY_END), KS_END);
	Key * parentKey = keyNew ("user/tests/list", KEY_END);
	PLUGIN_OPEN ("list");

	Plugin * check = elektraPluginOpen ("keytometa", modules, ksNew (5, KS_END), errorKey);
	if (!check)
	{
		printf ("Abort test case, keytometa is missing");
		goto end;
	}
	elektraPluginClose (check, 0);

	check = elektraPluginOpen ("rename", modules, ksNew (5, KS_END), errorKey);
	if (!check)
	{
		printf ("Abort test case, rename is missing");
		goto end;
	}
	elektraPluginClose (check, 0);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "kdbget failed");
	const Key * key = ksLookupByName (ks, "user/tests/list/meta1", 0);
	succeed_if (key, "key not found");
	const Key * meta = keyGetMeta (key, "amimetanow?");
	succeed_if (meta, "meta key not found");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 0, "kdbset failed");
	key = ksLookupByName (ks, "user/tests/list/to/be/cut/meta1", 0);
	succeed_if (key, "key not found");

end:
	PLUGIN_CLOSE ();
	keyDel (parentKey);
	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf ("LIST     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	doTest ();

	printf ("\ntestmod_list RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
