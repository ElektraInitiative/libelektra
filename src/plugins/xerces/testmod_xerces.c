/**
 * @file
 *
 * @brief Tests for xerces plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

#define ELEKTRA_XERCES_ORIGINAL_ROOT_NAME "xerces/rootname"

static void test_basics (void)
{
	printf ("test basics\n");
	fflush (stdout);

	ElektraKey * parentKey = keyNew ("system:/elektra/modules/xerces", KEY_END);
	ElektraKey * invalidKey = keyNew ("/", KEY_END);
	ElektraKeyset * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = ksNew (0, KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, invalidKey) == 0, "call to kdbGet was successful though the parentKey is invalid");

	// Will return 0 as we have no destination file set, we test serialization in detail later
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 0, "call to kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, invalidKey) == 0, "call to kdbSet was successful though the parentKey is invalid");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	keyDel (invalidKey);
	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();

	printf ("test basics finished\n");
	fflush (stdout);
}

static void test_simple_read (void)
{
	printf ("test simple read\n");
	fflush (stdout);

	ElektraKey * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/simple.xml"), KEY_END);
	ElektraKeyset * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = ksNew (0, KS_END);
	ElektraKey * current;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces", 0), "xerces key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), "  value of xerces  ") == 0, "value not correct");
		succeed_if (!keyGetMeta (current, "ELEKTRA_XERCES_ORIGINAL_ROOT_NAME"), "original root name metadata exists");
	}

	output_keyset (ks);

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/fizz", 0), "base key fizz not found");
	if (current)
	{
		const ElektraKey * meta;
		succeed_if (meta = keyGetMeta (current, "array"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/array") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "#2") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/fizz/#0", 0), "first fizz key not found");
	if (current)
	{
		const ElektraKey * meta;
		succeed_if (meta = keyGetMeta (current, "buzz"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/buzz") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "fizzBuzz") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/fizz/#1", 0), "second fizz key not found");
	if (current)
	{
		succeed_if (!keyGetMeta (current, "buzz"), "metadata exists");
		succeed_if (!keyGetMeta (current, "without"), "metadata exists");
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/fizz/#2", 0), "third fizz key not found");
	if (current)
	{
		const ElektraKey * meta;
		succeed_if (meta = keyGetMeta (current, "without"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/without") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "buzz") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/userKey", 0), "userKey key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/userKey") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), "withValue") == 0, "value not correct");

		const ElektraKey * meta;
		succeed_if (meta = keyGetMeta (current, "user"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/user") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "key") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/späciöl_-keüs1", 0), "späciöl_-keüs1 key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/späciöl_-keüs1") == 0, "wrong name");
		succeed_if (strcmp (keyValue (current), ">\"&<'") == 0, "value not correct");

		const ElektraKey * meta;
		succeed_if (meta = keyGetMeta (current, "attr"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/attr") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "\"") == 0, "wrong metadata value");
		}
		succeed_if (meta = keyGetMeta (current, "attr2"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/attr2") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "$%(){}``äüö²[/\\'>\"<'&") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = ksLookupByName (ks, "/sw/elektra/tests/xerces/cdata", 0), "cdata key not found");
	if (current)
	{
		succeed_if (strcmp (keyName (current), "/sw/elektra/tests/xerces/cdata") == 0, "wrong name");

		succeed_if (strcmp (keyValue (current), "this is some cdata text \"'<>&ä \"") == 0, "value not correct");

		const ElektraKey * meta;
		succeed_if (meta = keyGetMeta (current, "more-s_päcials"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (keyName (meta), "meta:/more-s_päcials") == 0, "wrong metadata name");
			succeed_if (strcmp (keyValue (meta), "1 & 2 are < 3 \n") == 0, "wrong metadata value");
		}
	}

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();

	printf ("test simple read finished\n");
	fflush (stdout);
}

static void test_simple_write (void)
{
	printf ("test simple write\n");
	fflush (stdout);

	ElektraKey * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, elektraFilename (), KEY_END);
	ElektraKeyset * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKey * root = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, "value of xerces", KEY_END);
	keySetMeta (root, ELEKTRA_XERCES_ORIGINAL_ROOT_NAME, "root");

	ElektraKey * keyWithMeta = keyNew ("/sw/elektra/tests/xerces/fizz", KEY_END);
	keySetMeta (keyWithMeta, "buzz", "fizzBuzz");

	ElektraKey * specialKeys = keyNew ("/sw/elektra/tests/xerces/späciöl_-keüs1", KEY_VALUE, ">\"&<'", KEY_END);
	keySetMeta (specialKeys, "attr", "\"");
	keySetMeta (specialKeys, "attr2", "$%(){}``äüö²[/\\'>&<'&");

	ElektraKey * moreSpecialKeys =
		keyNew ("/sw/elektra/tests/xerces/cdata", KEY_VALUE, "<![CDATA[this is some cdata text \"'<>&ä \"]]>", KEY_END);
	keySetMeta (moreSpecialKeys, "more-s_päcials", "1 & 2 are < 3 \n");

	ElektraKeyset * ks = ksNew (5, root, keyNew ("/sw/elektra/tests/xerces/userKey", KEY_VALUE, "withValue", KEY_END), keyWithMeta,
				    specialKeys, moreSpecialKeys, KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file ("xerces/escaping.xml"), keyString (parentKey)), "files do not match as expected");
	// Its also another good deserialization test
	ElektraKey * resultParentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/escaping.xml"), KEY_END);
	ElektraKeyset * result = ksNew (2, KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");
	compare_keyset (ks, result);

	elektraUnlink (keyString (parentKey));

	keyDel (parentKey);
	ksDel (ks);
	keyDel (resultParentKey);
	ksDel (result);
	PLUGIN_CLOSE ();

	printf ("test simple write finished\n");
	fflush (stdout);
}

static void test_maven_pom (void)
{
	printf ("test maven pom\n");
	fflush (stdout);

	ElektraKey * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/pom.xml"), KEY_END);
	ElektraKeyset * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = ksNew (64, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	// Its also another good deserialization test
	ElektraKey * serializationParentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, elektraFilename (), KEY_END);
	succeed_if (plugin->kdbSet (plugin, ks, serializationParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (serializationParentKey), "error in kdbSet");
	succeed_if (output_warnings (serializationParentKey), "warnings in kdbSet");

	ElektraKey * resultParentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, keyString (serializationParentKey), KEY_END);
	ElektraKeyset * result = ksNew (64, KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");
	succeed_if (66 == ksGetSize (ks), "pom file is expected to contain 66 keys");

	compare_keyset (ks, result); // Should be the same

	elektraUnlink (keyString (serializationParentKey));

	keyDel (parentKey);
	ksDel (ks);
	keyDel (serializationParentKey);
	keyDel (resultParentKey);
	ksDel (result);
	PLUGIN_CLOSE ();

	printf ("test maven pom finished\n");
	fflush (stdout);
}

static void test_jenkins_config (void)
{
	printf ("test jenkins config\n");
	fflush (stdout);

	ElektraKey * parentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, srcdir_file ("xerces/jenkins.xml"), KEY_END);
	ElektraKeyset * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = ksNew (64, KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	// Its also another good deserialization test
	ElektraKey * serializationParentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, elektraFilename (), KEY_END);
	succeed_if (plugin->kdbSet (plugin, ks, serializationParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (serializationParentKey), "error in kdbSet");
	succeed_if (output_warnings (serializationParentKey), "warnings in kdbSet");

	ElektraKey * resultParentKey = keyNew ("/sw/elektra/tests/xerces", KEY_VALUE, keyString (serializationParentKey), KEY_END);
	ElektraKeyset * result = ksNew (64, KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");

	ElektraKey * current;

	succeed_if (
		current = ksLookupByName (
			ks, "/sw/elektra/tests/xerces/temporaryOfflineCause/user/properties/jenkins.security.ApiTokenProperty/apiToken", 0),
		"failed to find apiToken key");
	succeed_if (strcmp (keyValue (current), "bee4ahGhOqua3ahzsai2Eef5quie5ohK/eiSe4eav+JhVlerBftAil8Ow5AejahBe9oiksKAlla/kk1/1=") == 0,
		    "api token is wrong");

	succeed_if (89 == ksGetSize (ks), "pom file is expected to contain 89 keys");

	compare_keyset (ks, result); // Should be the same

	elektraUnlink (keyString (serializationParentKey));

	keyDel (parentKey);
	ksDel (ks);
	keyDel (serializationParentKey);
	keyDel (resultParentKey);
	ksDel (result);
	PLUGIN_CLOSE ();

	printf ("test jenkins config finished\n");
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
	test_maven_pom ();
	test_jenkins_config ();

	print_result ("testmod_xerces");

	return nbError;
}
