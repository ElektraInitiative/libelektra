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

	// succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	succeed_if (plugin->kdbError (plugin, ks, parentKey) == 1, "call to kdbError was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_simple_read ()
{
	printf ("test simple read\n");

	Key * parentKey = keyNew ("/sw/elektra/tests", KEY_VALUE, srcdir_file ("testdata/simple.xml"), KEY_END);
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

		const Key * meta;
		succeed_if (meta = keyGetMeta (current, "buzz"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "buzz") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "fizzBuzz") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/userKey", 0), "userKey key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/userKey") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), "withValue") == 0, "value not correct");

		const Key * meta;
		succeed_if (meta = keyGetMeta (current, "user"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "user") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "key") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/späciöl_-keüs1", 0), "späciöl_-keüs1 key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/späciöl_-keüs1") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), ">\"&<'") == 0, "value not correct");

		const Key * meta;
		succeed_if (meta = keyGetMeta (current, "attr"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "attr") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "\"") == 0, "wrong metadata value");
		}
		succeed_if (meta = keyGetMeta (current, "attr2"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "attr2") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "$%(){}``äüö²[/\\'>\"<'&") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/cdata", 0), "cdata key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/cdata") == 0, "wrong name");

		succeed_if (strcmp (keyValue (current), "this is some cdata text \"'<>&ä \"") == 0, "value not correct");

		const Key * meta;
		succeed_if (meta = keyGetMeta (current, "more-s_päcials"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "more-s_päcials") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "1 & 2 are < 3 \n") == 0, "wrong metadata value");
		}
	}

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();

	printf ("test simple read finished\n");
	fflush (stdout);
}

static void test_simple_write ()
{
	printf ("test simple write\n");
	fflush (stdout);

	Key * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("testdata/simple-gen.xml"), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	Key * keyWithMeta = keyNew ("/sw/elektra/tests/xerces/fizz", KEY_END);
	keySetMeta (keyWithMeta, "buzz", "fizzBuzz");
	KeySet * ks = ksNew (3, keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, "value of xerces", KEY_END),
			     keyNew ("user/sw/elektra/tests/xerces/userKey", KEY_VALUE, "withValue", KEY_END), keyWithMeta, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");
	// As compare_files does not take different formatting into account, compare using deserialization again
	KeySet * result = ksNew (2, KS_END);
	succeed_if (plugin->kdbGet (plugin, result, parentKey) == 1, "call to kdbGet was not successful");
	compare_keyset (ks, result);

	elektraUnlink (srcdir_file ("testdata/simple-gen.xml"));

	keyDel (parentKey);
	ksDel (ks);
	ksDel (result);
	PLUGIN_CLOSE ();

	printf ("test simple write finished\n");
	fflush (stdout);
}

int main (int argc, char ** argv)
{
	printf ("XERCES    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_basics ();
	test_simple_read ();
	test_simple_write ();

	printf ("\ntestmod_xerces RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}
