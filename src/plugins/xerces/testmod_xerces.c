/**
 * @file
 *
 * @brief Tests for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_basics ()
{
	printf ("test basics\n");

	Key * parentKey = keyNew ("system/elektra/modules/xerces", KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (plugin->kdbError (plugin, ks, parentKey) == 1, "call to kdbError was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_simple ()
{
	printf ("test simple\n");

	Key * parentKey = keyNew ("/sw/elektra/tests/xerces/parent", KEY_VALUE, srcdir_file ("testdata/simple.xml"), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	KeySet * ks = ksNew (0, KS_END);
	Key * current;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces", 0), "xerces key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), "value of xerces") == 0, "value not correct");
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/fizz", 0), "fizz key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/fizz") == 0, "wrong name");

		Key * meta;
		succeed_if (meta = keyGetMeta (current, "buzz"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "buzz") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "fizzBuzz") == 0, "wrong metadata value");
		}
	}

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("XERCES    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_simple ();

	printf ("\ntestmod_xerces RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
