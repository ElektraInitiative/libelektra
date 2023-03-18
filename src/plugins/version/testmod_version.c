/**
 * @file
 *
 * @brief Tests for version plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "./version.h"

#include <stdlib.h>
#include <string.h>

#include <internal/kdb/config.h>
#include <tests_plugin.h>

static void test_basics (void)
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("system:/elektra/version", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("version");

	KeySet * ks = ksNew (0, KS_END);

	ElektraKdbPhase phase = ELEKTRA_KDB_GET_PHASE_STORAGE;
	plugin->global = ksNew (
		1, keyNew ("system:/elektra/kdb/backend/phase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase), KEY_VALUE, &phase, KEY_END),
		KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");

	KeySet * expectedKs = elektraVersionKeySet ();
	succeed_if (ksGetSize (ks) == ksGetSize (expectedKs), "wrong number of keys returned");

	for (elektraCursor i = 0; i < ksGetSize (ks); i++)
	{
		Key * cur = ksAtCursor (ks, i);
		Key * expected = ksAtCursor (expectedKs, i);

		succeed_if (strcmp (keyName (expected), keyName (cur)) == 0, "key with wrong name returned");
		succeed_if (strcmp (keyString (expected), keyString (cur)) == 0, "key with wrong value returned");

		succeed_if (keyGetMeta (cur, "restrict/write") != NULL, "missing restrict/write metadata");
		succeed_if (keyGetMeta (cur, "restrict/remove") != NULL, "missing restrict/remove metadata");
	}


	keyDel (parentKey);
	ksDel (expectedKs);
	ksDel (ks);
	ksDel (plugin->global);
	PLUGIN_CLOSE ();
}

static void test_rename (void)
{
	printf ("test rename\n");

	Key * parentKey = keyNew ("user:/somewhere/else", KEY_END);
	size_t parentSize = keyGetNameSize (parentKey) - 1;
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("version");

	KeySet * ks = ksNew (0, KS_END);

	ElektraKdbPhase phase = ELEKTRA_KDB_GET_PHASE_STORAGE;
	plugin->global = ksNew (
		1, keyNew ("system:/elektra/kdb/backend/phase", KEY_BINARY, KEY_SIZE, sizeof (ElektraKdbPhase), KEY_VALUE, &phase, KEY_END),
		KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");

	KeySet * expectedKs = elektraVersionKeySet ();
	succeed_if (ksGetSize (ks) == ksGetSize (expectedKs), "wrong number of keys returned");

	for (elektraCursor i = 0; i < ksGetSize (ks); i++)
	{
		Key * cur = ksAtCursor (ks, i);
		Key * expected = ksAtCursor (expectedKs, i);

		succeed_if (strncmp ("user:/somewhere/else", keyName (cur), parentSize) == 0, "key with wrong name returned");
		succeed_if (strcmp (keyName (expected) + sizeof ("system:/elektra/version") - 1, keyName (cur) + parentSize) == 0,
			    "key with wrong name returned");
		succeed_if (strcmp (keyString (expected), keyString (cur)) == 0, "key with wrong value returned");

		succeed_if (keyGetMeta (cur, "restrict/write") != NULL, "missing restrict/write metadata");
		succeed_if (keyGetMeta (cur, "restrict/remove") != NULL, "missing restrict/remove metadata");
	}


	keyDel (parentKey);
	ksDel (expectedKs);
	ksDel (ks);
	ksDel (plugin->global);
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
