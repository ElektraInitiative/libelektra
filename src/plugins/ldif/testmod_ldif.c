/**
 * @file
 *
 * @brief Tests for ldif plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h>

#include <tests_plugin.h>

static void test_base (const char * fileContent, int numKeys, const char ** keys, const char ** values)
{
	const char * tmpFile = elektraFilename ();
	FILE * fh = fopen (tmpFile, "w");
	if (fh)
	{
		fputs (fileContent, fh);
		fclose (fh);
	}

	Key * parentKey = keyNew ("user:/tests/ldif", KEY_VALUE, tmpFile, KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	Key * key;

	PLUGIN_OPEN ("ldif");

	KeySet * ks = ksNew (numKeys, KS_END);

	succeed_if (plugin->kdbGet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbGet was not successful");

	Key * lookup;
	for (int i = 0; i < numKeys; i++)
	{
		lookup = keyNew ("user:/tests/ldif", KEY_END);
		keyAddName (lookup, keys[i]);
		printf ("testing key '%s'\n", keys[i]);
		succeed_if ((key = ksLookup (ks, lookup, 0)) != NULL, "key not found");
		succeed_if (strcmp (values[i], keyString (key)) == 0, "value of key did not match");
		keyDel (lookup);
	}

	keyDel (parentKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

static void test_read (void)
{
	printf ("test read\n");

	const char * expected_keys[] = { "dc=org/dc=libelektra/ou=developer/uid=test/dn", "dc=org/dc=libelektra/ou=developer/uid=test/uid",
					 "dc=org/dc=libelektra/ou=developer/uid=test/key2",
					 "dc=org/dc=libelektra/ou=developer/uid=test/key3" };
	const char * expected_values[] = { "uid=test,ou=developer,dc=libelektra,dc=org", "value1", "value2", "value3" };

	test_base (
		"dn: uid=test,ou=developer,dc=libelektra,dc=org\n"
		"uid: value1\n"
		"key2: value2\n"
		"key3: value3\n",
		4, expected_keys, expected_values);
}

static void test_readOnlyKey (void)
{
	printf ("test readOnlyKey\n");

	const char * expected_keys[] = {
		"dc=org/dc=libelektra/ou=developer/uid=test/dn",
	};
	const char * expected_values[] = {
		"uid=test,ou=developer,dc=libelektra,dc=org",
	};

	test_base ("dn: uid=test,ou=developer,dc=libelektra,dc=org\n", 1, expected_keys, expected_values);
}

static void test_writeOnlyKey (void)
{
	printf ("test write\n");

	Key * parentKey = keyNew ("user:/tests/ldif", KEY_VALUE, elektraFilename (), KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	PLUGIN_OPEN ("ldif");

	Key * devKey = keyNew ("user:/tests/ldif/dc=org/dc=libelektra/ou=developer/uid=test/dn", KEY_VALUE,
			       "uid=test,ou=developer,dc=libelektra,dc=org", KEY_END);
	KeySet * ks = ksNew (10, devKey, KS_END);

	succeed_if (plugin->kdbSet (plugin, ks, parentKey) == ELEKTRA_PLUGIN_STATUS_SUCCESS, "call to kdbSet was not successful");
	keyDel (parentKey);
	keyDel (devKey);
	ksDel (ks);
	PLUGIN_CLOSE ();
}

int main (int argc, char ** argv)
{
	printf ("LDIF     TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_read ();
	test_readOnlyKey ();
	test_writeOnlyKey ();

	print_result ("testmod_ldif");

	return nbError;
}
