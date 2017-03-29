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

#define ELEKTRA_XERCES_ORIGINAL_ROOT_NAME "elektraXercesOriginalRootName"

static void test_basics ()
{
	printf ("test basics\n");
	fflush (stdout);

	Key * parentKey = keyNew ("system/elektra/modules/xerces", KEY_END);
	Key * invalidKey = keyNew (0, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	KeySet * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, invalidKey) == 0, "call to kdbGet was successful though the parentKey is invalid");

	// Will return 0 as we have no destination file set, we test serialization in detail later
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 0, "call to kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, invalidKey) == 0, "call to kdbSet was successful though the parentKey is invalid");

	succeed_if (plugin->kdbError (plugin, ks, parentKey) == 1, "call to kdbError was not successful");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	keyDel (invalidKey);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();

	printf ("test basics finished\n");
	fflush (stdout);
}

static void test_simple_read ()
{
	printf ("test simple read\n");
	fflush (stdout);

	Key * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/simple.xml"), KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	KeySet * ks = ksNew (0, KS_END);
	Key * current;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces", 0), "xerces key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), "  value of xerces  ") == 0, "value not correct");
		succeed_if (!keyGetMeta (current, "ELEKTRA_XERCES_ORIGINAL_ROOT_NAME"), "original root name metadata exists");
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

	Key * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/escaping-gen.xml"), KEY_END);
	keySetMeta (parentKey, ELEKTRA_XERCES_ORIGINAL_ROOT_NAME, "root");
	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	Key * root = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, "value of xerces", KEY_END);
	keySetMeta (root, ELEKTRA_XERCES_ORIGINAL_ROOT_NAME, "root");

	Key * keyWithMeta = keyNew ("/sw/elektra/tests/xerces/fizz", KEY_END);
	keySetMeta (keyWithMeta, "buzz", "fizzBuzz");

	Key * specialKeys = keyNew ("/sw/elektra/tests/xerces/späciöl_-keüs1", KEY_VALUE, ">\"&<'", KEY_END);
	keySetMeta (specialKeys, "attr", "\"");
	keySetMeta (specialKeys, "attr2", "$%(){}``äüö²[/\\'>&<'&");

	Key * moreSpecialKeys =
		keyNew ("/sw/elektra/tests/xerces/cdata", KEY_VALUE, "<![CDATA[this is some cdata text \"'<>&ä \"]]>", KEY_END);
	keySetMeta (moreSpecialKeys, "more-s_päcials", "1 & 2 are < 3 \n");

	KeySet * ks = ksNew (5, root, keyNew ("/sw/elektra/tests/xerces/userKey", KEY_VALUE, "withValue", KEY_END), keyWithMeta,
			     specialKeys, moreSpecialKeys, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 1, "call to kdbSet was not successful");

	compare_files ("xerces/escaping.xml");
	// Its also another good deserialization test
	Key * resultParentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/escaping.xml"), KEY_END);
	KeySet * result = ksNew (2, KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");
	compare_keyset (ks, result);

	elektraUnlink (srcdir_file ("xerces/escaping-gen.xml"));

	keyDel (parentKey);
	ksDel (ks);
	keyDel (resultParentKey);
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
