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

#include <gpg.h>
#include <test_key.h>

#define PLUGIN_NAME "fcrypt"
#define TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"
#define TEST_FILE "fcrypt_testfile"

static const kdb_octet_t testContent[] = { 0x01, 0x02, 0xCA, 0xFE, 0xBA, 0xBE, 0x03, 0x04 };

/*
 * @brief create temporary file name
 * @returns allocated string holding the path to the temporary file. Must be freed by the caller.
 */
static char * getTemporaryFileName ()
{
	const size_t newFileAllocated = strlen (TEST_FILE) + 7;
	char * newFile = elektraMalloc (newFileAllocated);
	if (!newFile) return NULL;
	snprintf (newFile, newFileAllocated, "%sXXXXXX", TEST_FILE);
	mkstemp (newFile);
	return newFile;
}

static KeySet * newPluginConfiguration ()
{
	return ksNew (2, keyNew (ELEKTRA_CRYPTO_PARAM_GPG_KEY, KEY_VALUE, TEST_KEY_ID, KEY_END),
		      keyNew (ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, KEY_VALUE, "1", KEY_END), KS_END);
}

static void writeTestFile (const char * file)
{
	FILE * f = fopen (file, "wb");
	succeed_if (f, "can not write to temporary file");
	if (!f) return;
	succeed_if (fwrite (testContent, 1, sizeof (testContent), f) == sizeof (testContent), "test file preparation failed");
	fclose (f);
}

/**
 * @brief read in the content stored in file and compare it to the test vector.
 * @retval 1 if the file content is equal to the test vector.
 * @retval -1 if the file content is not equal to the test vector or an error occured.
 */
static int isTestFileCorrect (const char * file)
{
	int returnValue = -1;
	kdb_octet_t readBuffer[2 * sizeof (testContent)] = { 0 };
	size_t readCount = 0;

	FILE * f = fopen (file, "rb");
	succeed_if (f, "can not read from temporary file");
	if (!f) return -1;

	readCount = fread (readBuffer, 1, sizeof (readBuffer), f);
	succeed_if (readCount > 0, "temporary file is empty.");

	if (readCount == sizeof (testContent))
	{
		if (memcmp (readBuffer, testContent, sizeof (testContent)) == 0)
		{
			returnValue = 1;
		}
	}

	fclose (f);
	return returnValue;
}

static void test_init ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (PLUGIN_NAME, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, PLUGIN_NAME), "got wrong name");

		KeySet * config = elektraPluginGetConfig (plugin);
		succeed_if (config != 0, "there should be a config");

		succeed_if (plugin->kdbOpen != 0, "no open pointer");
		succeed_if (plugin->kdbClose != 0, "no close pointer");
		succeed_if (plugin->kdbGet != 0, "no get pointer");
		succeed_if (plugin->kdbSet != 0, "no set pointer");

		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_gpg ()
{
	// Plugin configuration
	KeySet * conf = newPluginConfiguration ();
	Key * errorKey = keyNew (0);

	// install the gpg key
	char * argv[] = { "", "-a", "--import", NULL };
	const size_t argc = 4;
	Key * msg = keyNew (0);
	keySetBinary (msg, test_key_asc, test_key_asc_len);

	succeed_if (CRYPTO_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msg, argv, argc) == 1, "failed to install the GPG test key");

	keyDel (msg);
	keyDel (errorKey);
	ksDel (conf);
}

static void test_file_operations ()
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (PLUGIN_NAME, modules, config, 0);
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

int main (int argc, char ** argv)
{
	printf ("FCRYPT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_gpg ();
	test_init ();
	test_file_operations ();

	printf ("\n" ELEKTRA_PLUGIN_NAME " RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}
