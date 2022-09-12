/**
 * @file
 *
 * @brief Tests for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>
#include <kdbversion.h>

#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	ElektraKey * parentKey = elektraKeyNew ("system:/elektra/version", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("version");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	plugin->global = elektraKeysetNew (1, elektraKeyNew ("system:/elektra/kdb/backend/phase", ELEKTRA_KEY_VALUE, ELEKTRA_KDB_GET_PHASE_STORAGE, ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");

	ElektraKeyset * expectedKs = elektraVersionKeySet ();
	succeed_if (elektraKeysetGetSize (ks) == elektraKeysetGetSize (expectedKs), "wrong number of keys returned");

	for (elektraCursor i = 0; i < elektraKeysetGetSize (ks); i++)
	{
		ElektraKey * cur = elektraKeysetAtCursor (ks, i);
		ElektraKey * expected = elektraKeysetAtCursor (expectedKs, i);

		succeed_if (strcmp (elektraKeyName (expected), elektraKeyName (cur)) == 0, "key with wrong name returned");
		succeed_if (strcmp (elektraKeyString (expected), elektraKeyString (cur)) == 0, "key with wrong value returned");

		succeed_if (elektraKeyGetMeta (cur, "restrict/write") != NULL, "missing restrict/write metadata");
		succeed_if (elektraKeyGetMeta (cur, "restrict/remove") != NULL, "missing restrict/remove metadata");
	}


	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeysetDel (plugin->global);
	PLUGIN_CLOSE ();
}

static void test_rename (void)
{
	printf ("test rename\n");

	ElektraKey * parentKey = elektraKeyNew ("user:/somewhere/else", ELEKTRA_KEY_END);
	size_t parentSize = elektraKeyGetNameSize (parentKey) - 1;
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("version");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	plugin->global = elektraKeysetNew (1, elektraKeyNew ("system:/elektra/kdb/backend/phase", ELEKTRA_KEY_VALUE, ELEKTRA_KDB_GET_PHASE_STORAGE, ELEKTRA_KEY_END), ELEKTRA_KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");

	ElektraKeyset * expectedKs = elektraVersionKeySet ();
	succeed_if (elektraKeysetGetSize (ks) == elektraKeysetGetSize (expectedKs), "wrong number of keys returned");

	for (elektraCursor i = 0; i < elektraKeysetGetSize (ks); i++)
	{
		ElektraKey * cur = elektraKeysetAtCursor (ks, i);
		ElektraKey * expected = elektraKeysetAtCursor (expectedKs, i);

		succeed_if (strncmp ("user:/somewhere/else", elektraKeyName (cur), parentSize) == 0, "key with wrong name returned");
		succeed_if (strcmp (elektraKeyName (expected) + sizeof ("system:/elektra/version") - 1, elektraKeyName (cur) + parentSize) == 0,
			    "key with wrong name returned");
		succeed_if (strcmp (elektraKeyString (expected), elektraKeyString (cur)) == 0, "key with wrong value returned");

		succeed_if (elektraKeyGetMeta (cur, "restrict/write") != NULL, "missing restrict/write metadata");
		succeed_if (elektraKeyGetMeta (cur, "restrict/remove") != NULL, "missing restrict/remove metadata");
	}


	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeysetDel (plugin->global);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("VERSION      TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_rename ();

	print_result ("testmod_version");

	return nbError;
}
