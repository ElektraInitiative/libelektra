/**
 * @file
 *
 * @brief Tests for length plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdio.h>

#define PLUGIN_NAME "length"


static void test_length (void)
{
	Key * parentKey = keyNew ("user:/tests/length", KEY_VALUE, "", KEY_END);
	Key * k1 = keyNew ("user:/tests/length/valid1", KEY_VALUE, "value", KEY_META, "check/length", "10", KEY_END);
	Key * k2 = keyNew ("user:/tests/length/invalid1", KEY_VALUE, "waytoolongvalue", KEY_META, "check/length", "5", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KeySet * ks = ksNew (1, k1, KS_END);
	PLUGIN_OPEN ("length");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbGet failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "kdbSet failed");
	ksDel (ks);

	ks = ksNew (20, KS_END);
	ksAppendKey (ks, k2);
	ksRewind (ks);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbGet should have failed");
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR, "kdbSet should have failed");
	ksDel (ks);

	keyDel (parentKey);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("length     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);


	print_result ("testmod_length");

	test_length ();

	return nbError;
}
