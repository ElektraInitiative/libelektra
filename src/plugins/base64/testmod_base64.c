/**
 * @file
 *
 * @brief test suite for the fcrypt plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>

#include "base64.h"

static KeySet * newPluginConfiguration()
{
	return ksNew (0, KS_END);
}

static void test_init ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (ELEKTRA_PLUGIN_NAME, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, ELEKTRA_PLUGIN_NAME), "got wrong name");

		KeySet * config = elektraPluginGetConfig (plugin);
		succeed_if (config != 0, "there should be a config");

		succeed_if (plugin->kdbGet != 0, "no get pointer");
		succeed_if (plugin->kdbSet != 0, "no set pointer");

		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_base64_encoding ()
{
	// test vectors are defined in RFC4648
	// see https://www.ietf.org/rfc/rfc4648.txt
	char * t1 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"", 0);
	succeed_if (t1, "can not allocate string for encoding result");
	if (t1)
	{
		succeed_if (strcmp (t1, "") == 0, "encoding result from test vector 1 does not match");
		elektraFree (t1);
	}

	char * t2 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"f", 1);
	succeed_if (t2, "can not allocate string for encoding result");
	if (t2)
	{
		succeed_if (strcmp (t2, "Zg==") == 0, "encoding result from test vector 2 does not match");
		elektraFree (t2);
	}

	char * t3 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"fo", 2);
	succeed_if (t3, "can not allocate string for encoding result");
	if (t3)
	{
		succeed_if (strcmp (t3, "Zm8=") == 0, "encoding result from test vector 3 does not match");
		elektraFree (t3);
	}

	char * t4 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"foo", 3);
	succeed_if (t4, "can not allocate string for encoding result");
	if (t4)
	{
		succeed_if (strcmp (t4, "Zm9v") == 0, "encoding result from test vector 4 does not match");
		elektraFree (t4);
	}

	char * t5 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"foob", 4);
	succeed_if (t5, "can not allocate string for encoding result");
	if (t1)
	{
		succeed_if (strcmp (t5, "Zm9vYg==") == 0, "encoding result from test vector 5 does not match");
		elektraFree (t5);
	}

	char * t6 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"fooba", 5);
	succeed_if (t6, "can not allocate string for encoding result");
	if (t6)
	{
		succeed_if (strcmp (t6, "Zm9vYmE=") == 0, "encoding result from test vector 6 does not match");
		elektraFree (t6);
	}

	char * t7 = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME, base64Encode) ((kdb_octet_t*)"foobar", 6);
	succeed_if (t7, "can not allocate string for encoding result");
	if (t7)
	{
		succeed_if (strcmp (t7, "Zm9vYmFy") == 0, "encoding result from test vector 7 does not match");
		elektraFree (t7);
	}
}

static void test_base64_decoding ()
{
}

/*
static void test_file_operations ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (ELEKTRA_PLUGIN_NAME, modules, config, 0);
	succeed_if (plugin, "failed to open plugin handle");
	if (plugin)
	{
		KeySet * data = ksNew (0, KS_END);
		char * tmpFile = getTemporaryFileName ();
		if (tmpFile)
		{
			// prepare test file to be encrypted
			writeTestFile (tmpFile);
			keySetString (parentKey, tmpFile);

			// try to encrypt the file
			succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");
			succeed_if (isTestFileCorrect (tmpFile) == -1, "file content did not change during encryption");

			// try to decrypt the file again (simulating the pregetstorage call)
			succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get (pregetstorage) failed");
			succeed_if (isTestFileCorrect (tmpFile) == 1, "file content could not be restored during decryption");

			// a second call to kdb get (the postgetstorage call) should re-encrypt the file again
			succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get (postgetstorage) failed");
			succeed_if (isTestFileCorrect (tmpFile) == -1, "postgetstorage did not encrypt the file again");

			remove (tmpFile);
			elektraFree (tmpFile);
		}

		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}
*/

int main (int argc, char ** argv)
{
	printf ("FCRYPT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	// test the encoding and decoding process
	test_base64_encoding ();
	test_base64_decoding ();

	// test the plugin functionality
	test_init ();

	printf ("\n" ELEKTRA_PLUGIN_NAME " RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}
