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

	ElektraKey * parentKey = elektraKeyNew ("system:/elektra/modules/xerces", ELEKTRA_KEY_END);
	ElektraKey * invalidKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "call to kdbOpen was not successful");

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, invalidKey) == 0, "call to kdbGet was successful though the parentKey is invalid");

	// Will return 0 as we have no destination file set, we test serialization in detail later
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == 0, "call to kdbSet was not successful");
	succeed_if (plugin->kdbGet (plugin, ks, invalidKey) == 0, "call to kdbSet was successful though the parentKey is invalid");

	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "call to kdbClose was not successful");

	elektraKeyDel (invalidKey);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();

	printf ("test basics finished\n");
	fflush (stdout);
}

static void test_simple_read (void)
{
	printf ("test simple read\n");
	fflush (stdout);

	ElektraKey * parentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, srcdir_file ("xerces/simple.xml"), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * current;

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces", 0), "xerces key not found");
	if (current)
	{
		succeed_if (strcmp (elektraKeyName (current), "/sw/elektra/tests/xerces") == 0, "wrong name");
		succeed_if (strcmp (elektraKeyValue (current), "  value of xerces  ") == 0, "value not correct");
		succeed_if (!elektraKeyGetMeta (current, "ELEKTRA_XERCES_ORIGINAL_ROOT_NAME"), "original root name metadata exists");
	}

	output_keyset (ks);

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/fizz", 0), "base key fizz not found");
	if (current)
	{
		const ElektraKey * meta;
		succeed_if (meta = elektraKeyGetMeta (current, "array"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/array") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "#2") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/fizz/#0", 0), "first fizz key not found");
	if (current)
	{
		const ElektraKey * meta;
		succeed_if (meta = elektraKeyGetMeta (current, "buzz"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/buzz") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "fizzBuzz") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/fizz/#1", 0), "second fizz key not found");
	if (current)
	{
		succeed_if (!elektraKeyGetMeta (current, "buzz"), "metadata exists");
		succeed_if (!elektraKeyGetMeta (current, "without"), "metadata exists");
	}

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/fizz/#2", 0), "third fizz key not found");
	if (current)
	{
		const ElektraKey * meta;
		succeed_if (meta = elektraKeyGetMeta (current, "without"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/without") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "buzz") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/userKey", 0), "userKey key not found");
	if (current)
	{
		succeed_if (strcmp (elektraKeyName (current), "/sw/elektra/tests/xerces/userKey") == 0, "wrong name");
		succeed_if (strcmp (elektraKeyValue (current), "withValue") == 0, "value not correct");

		const ElektraKey * meta;
		succeed_if (meta = elektraKeyGetMeta (current, "user"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/user") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "key") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/späciöl_-keüs1", 0), "späciöl_-keüs1 key not found");
	if (current)
	{
		succeed_if (strcmp (elektraKeyName (current), "/sw/elektra/tests/xerces/späciöl_-keüs1") == 0, "wrong name");
		succeed_if (strcmp (elektraKeyValue (current), ">\"&<'") == 0, "value not correct");

		const ElektraKey * meta;
		succeed_if (meta = elektraKeyGetMeta (current, "attr"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/attr") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "\"") == 0, "wrong metadata value");
		}
		succeed_if (meta = elektraKeyGetMeta (current, "attr2"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/attr2") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "$%(){}``äüö²[/\\'>\"<'&") == 0, "wrong metadata value");
		}
	}

	succeed_if (current = elektraKeysetLookupByName (ks, "/sw/elektra/tests/xerces/cdata", 0), "cdata key not found");
	if (current)
	{
		succeed_if (strcmp (elektraKeyName (current), "/sw/elektra/tests/xerces/cdata") == 0, "wrong name");

		succeed_if (strcmp (elektraKeyValue (current), "this is some cdata text \"'<>&ä \"") == 0, "value not correct");

		const ElektraKey * meta;
		succeed_if (meta = elektraKeyGetMeta (current, "more-s_päcials"), "no metadata exists");
		if (meta)
		{
			succeed_if (strcmp (elektraKeyName (meta), "meta:/more-s_päcials") == 0, "wrong metadata name");
			succeed_if (strcmp (elektraKeyValue (meta), "1 & 2 are < 3 \n") == 0, "wrong metadata value");
		}
	}

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	PLUGIN_CLOSE ();

	printf ("test simple read finished\n");
	fflush (stdout);
}

static void test_simple_write (void)
{
	printf ("test simple write\n");
	fflush (stdout);

	ElektraKey * parentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKey * root = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, "value of xerces", ELEKTRA_KEY_END);
	elektraKeySetMeta (root, ELEKTRA_XERCES_ORIGINAL_ROOT_NAME, "root");

	ElektraKey * keyWithMeta = elektraKeyNew ("/sw/elektra/tests/xerces/fizz", ELEKTRA_KEY_END);
	elektraKeySetMeta (keyWithMeta, "buzz", "fizzBuzz");

	ElektraKey * specialKeys = elektraKeyNew ("/sw/elektra/tests/xerces/späciöl_-keüs1", ELEKTRA_KEY_VALUE, ">\"&<'", ELEKTRA_KEY_END);
	elektraKeySetMeta (specialKeys, "attr", "\"");
	elektraKeySetMeta (specialKeys, "attr2", "$%(){}``äüö²[/\\'>&<'&");

	ElektraKey * moreSpecialKeys =
		elektraKeyNew ("/sw/elektra/tests/xerces/cdata", ELEKTRA_KEY_VALUE, "<![CDATA[this is some cdata text \"'<>&ä \"]]>", ELEKTRA_KEY_END);
	elektraKeySetMeta (moreSpecialKeys, "more-s_päcials", "1 & 2 are < 3 \n");

	ElektraKeyset * ks = elektraKeysetNew (5, root, elektraKeyNew ("/sw/elektra/tests/xerces/userKey", ELEKTRA_KEY_VALUE, "withValue", ELEKTRA_KEY_END), keyWithMeta,
				    specialKeys, moreSpecialKeys, ELEKTRA_KS_END);
	succeed_if (plugin->kdbSet (plugin, ks, parentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (parentKey), "error in kdbSet");
	succeed_if (output_warnings (parentKey), "warnings in kdbSet");

	succeed_if (compare_line_files (srcdir_file ("xerces/escaping.xml"), elektraKeyString (parentKey)), "files do not match as expected");
	// Its also another good deserialization test
	ElektraKey * resultParentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, srcdir_file ("xerces/escaping.xml"), ELEKTRA_KEY_END);
	ElektraKeyset * result = elektraKeysetNew (2, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");
	compare_keyset (ks, result);

	elektraUnlink (elektraKeyString (parentKey));

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeyDel (resultParentKey);
	elektraKeysetDel (result);
	PLUGIN_CLOSE ();

	printf ("test simple write finished\n");
	fflush (stdout);
}

static void test_maven_pom (void)
{
	printf ("test maven pom\n");
	fflush (stdout);

	ElektraKey * parentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, srcdir_file ("xerces/pom.xml"), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = elektraKeysetNew (64, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	// Its also another good deserialization test
	ElektraKey * serializationParentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	succeed_if (plugin->kdbSet (plugin, ks, serializationParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (serializationParentKey), "error in kdbSet");
	succeed_if (output_warnings (serializationParentKey), "warnings in kdbSet");

	ElektraKey * resultParentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, elektraKeyString (serializationParentKey), ELEKTRA_KEY_END);
	ElektraKeyset * result = elektraKeysetNew (64, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");
	succeed_if (66 == elektraKeysetGetSize (ks), "pom file is expected to contain 66 keys");

	compare_keyset (ks, result); // Should be the same

	elektraUnlink (elektraKeyString (serializationParentKey));

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeyDel (serializationParentKey);
	elektraKeyDel (resultParentKey);
	elektraKeysetDel (result);
	PLUGIN_CLOSE ();

	printf ("test maven pom finished\n");
	fflush (stdout);
}

static void test_jenkins_config (void)
{
	printf ("test jenkins config\n");
	fflush (stdout);

	ElektraKey * parentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, srcdir_file ("xerces/jenkins.xml"), ELEKTRA_KEY_END);
	ElektraKeyset * conf = elektraKeysetNew (0, ELEKTRA_KS_END);
	PLUGIN_OPEN ("xerces");

	ElektraKeyset * ks = elektraKeysetNew (64, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == 1, "call to kdbGet was not successful");

	// Its also another good deserialization test
	ElektraKey * serializationParentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, elektraFilename (), ELEKTRA_KEY_END);
	succeed_if (plugin->kdbSet (plugin, ks, serializationParentKey) >= 1, "call to kdbSet was not successful");
	succeed_if (output_error (serializationParentKey), "error in kdbSet");
	succeed_if (output_warnings (serializationParentKey), "warnings in kdbSet");

	ElektraKey * resultParentKey = elektraKeyNew ("/sw/elektra/tests/xerces", ELEKTRA_KEY_VALUE, elektraKeyString (serializationParentKey), ELEKTRA_KEY_END);
	ElektraKeyset * result = elektraKeysetNew (64, ELEKTRA_KS_END);
	succeed_if (plugin->kdbGet (plugin, result, resultParentKey) == 1, "call to kdbGet was not successful");

	ElektraKey * current;

	succeed_if (
		current = elektraKeysetLookupByName (
			ks, "/sw/elektra/tests/xerces/temporaryOfflineCause/user/properties/jenkins.security.ApiTokenProperty/apiToken", 0),
		"failed to find apiToken key");
	succeed_if (strcmp (elektraKeyValue (current), "bee4ahGhOqua3ahzsai2Eef5quie5ohK/eiSe4eav+JhVlerBftAil8Ow5AejahBe9oiksKAlla/kk1/1=") == 0,
		    "api token is wrong");

	succeed_if (89 == elektraKeysetGetSize (ks), "pom file is expected to contain 89 keys");

	compare_keyset (ks, result); // Should be the same

	elektraUnlink (elektraKeyString (serializationParentKey));

	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);
	elektraKeyDel (serializationParentKey);
	elektraKeyDel (resultParentKey);
	elektraKeysetDel (result);
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
