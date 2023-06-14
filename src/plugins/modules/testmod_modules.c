/**
 * @file
 *
 * @brief Tests for modules plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>


#include <internal/config.h>
#include <internal/macros/attributes.h>

#include <tests_plugin.h>

static int dummyGet (Plugin * plugin ELEKTRA_UNUSED, KeySet * ks, Key * parentKey ELEKTRA_UNUSED)
{
	ksAppendKey (ks, keyNew ("/foo", KEY_END));
	ksAppendKey (ks, keyNew ("/boo", KEY_VALUE, "123", KEY_END));
	ksAppendKey (ks, keyNew ("/bar", KEY_VALUE, "abc", KEY_END));
	ksAppendKey (ks, keyNew ("/baz", KEY_VALUE, "xyz", KEY_META, "meta", "test", KEY_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("system:/elektra/modules/dummy", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("modules");

	KeySet * expected = ksNew (0, KS_END);
	dummyGet (NULL, expected, NULL);

	Plugin * dummy = elektraCalloc (sizeof (struct _Plugin));
	dummy->kdbGet = dummyGet;
	dummy->refcounter = 1;

	KeySet * plugins = ksNew (1, keyNew ("system:/plugin", KEY_BINARY, KEY_SIZE, sizeof (dummy), KEY_VALUE, &dummy, KEY_END), KS_END);

	ElektraKdbPhase phase = ELEKTRA_KDB_GET_PHASE_STORAGE;
	plugin->global = ksNew (
		2, keyNew ("system:/elektra/kdb/backend/phase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase), KEY_VALUE, &phase, KEY_END),
		keyNew ("system:/elektra/kdb/backend/plugins", KEY_BINARY, KEY_SIZE, sizeof (KeySet *), KEY_VALUE, &plugins, KEY_END),
		KS_END);

	KeySet * definition = ksNew (0, KS_END);
	succeed_if (plugin->kdbInit (plugin, definition, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "kdbInit failed");
	ksDel (definition);

	KeySet * ks = ksNew (0, KS_END);


	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	compare_keyset (expected, ks);

	keyDel (parentKey);
	ksDel (ks);
	ksDel (expected);
	ksDel (plugin->global);
	ksDel (plugins);
	elektraFree (dummy);
	PLUGIN_CLOSE ();
}

static void test_wrongParent (void)
{
	printf ("test wrong parent\n");

	Key * parentKey = keyNew ("user:/tests/modules", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("modules");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "should not accept wrong parent");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("MODULES     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_wrongParent ();

	print_result ("testmod_modules");

	return nbError;
}
