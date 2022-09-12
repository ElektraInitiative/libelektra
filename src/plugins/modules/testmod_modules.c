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

#include <kdbprivate.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static int dummyGet (Plugin * plugin ELEKTRA_UNUSED, ElektraKeyset * ks, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	elektraKeysetAppend (ks, elektraKeysetNew (4, elektraKeyNew ("/foo", ELEKTRA_KEY_END), elektraKeyNew ("/boo", ELEKTRA_KEY_VALUE, "123", ELEKTRA_KEY_END),
			     elektraKeyNew ("/bar", ELEKTRA_KEY_VALUE, "abc", ELEKTRA_KEY_END),
			     elektraKeyNew ("/baz", ELEKTRA_KEY_VALUE, "xyz", ELEKTRA_KEY_META, "meta", "test", ELEKTRA_KEY_END), ELEKTRA_KS_END));
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

static void test_basics (void)
{
	printf ("test basics\n");

	ElektraKey * parentKey = elektraKeyNew ("system:/elektra/modules/dummy", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("modules");

	ElektraKeyset * expected = elektraKeysetNew (0, ELEKTRA_KS_END);
	dummyGet (NULL, expected, NULL);

	Plugin * dummy = elektraCalloc (sizeof (struct _Plugin));
	dummy->kdbGet = dummyGet;
	dummy->refcounter = 1;

	ElektraKeyset * definition =
		elektraKeysetNew (1, elektraKeyNew ("system:/plugin", ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (dummy), ELEKTRA_KEY_VALUE, &dummy, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	succeed_if (plugin->kdbInit (plugin, definition, parentKey) == ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "kdbInit failed");
	elektraKeysetDel (definition);

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	plugin->global = elektraKeysetNew (1, elektraKeyNew ("system:/elektra/kdb/backend/phase", ELEKTRA_KEY_VALUE, ELEKTRA_KDB_GET_PHASE_STORAGE, ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	compare_keyset (expected, ks);

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeysetDel (expected);
	elektraKeysetDel (plugin->global);
	PLUGIN_CLOSE ();
}

static void test_wrongParent (void)
{
	printf ("test wrong parent\n");

	ElektraKey * parentKey = elektraKeyNew ("user:/tests/modules", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("modules");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "should not accept wrong parent");

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
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
