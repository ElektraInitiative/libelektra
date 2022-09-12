/**
 * @file
 *
 * @brief test suite for the fcrypt plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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

#include "../crypto/common_gpg_tests.c"
#include "../crypto/gpgagent_teardown.h"
#include "fcrypt.h"

#define PLUGIN_NAME "fcrypt"
#define TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"
#define TEST_FILE "fcrypt_testfile"
#define BUFFER_SIZE 2048

#define FAULTY_SIGNATURE_FILE                                                                                                              \
	"-----BEGIN PGP SIGNED MESSAGE-----\n\
Hash: SHA512\n\
\n\
test (modified)\n\
-----BEGIN PGP SIGNATURE-----\n\
\n\
iJwEAQEKAAYFAlmqZsMACgkQ2vY1sX8jDo2etAP/UA4s7e+SR38wa+AqQbWXKrPp\n\
i3hoYLPP9lIz5ypedFBlNjcJRjv47wvGc0Z2C1Q6pMtTNcI+is2X9zJNucv9aMeA\n\
nghsNiEgaIARzOFIe13QTevCg/HEFnq48gFSYNyeVgcsPmVP6tu3xWoEUkVEu6Vf\n\
XRrYPw+gFVq5zeOAI4A=\n\
=MjjB\n\
-----END PGP SIGNATURE-----\n"

static const kdb_octet_t testContent[] = { 0x01, 0x02, 0xCA, 0xFE, 0xBA, 0xBE, 0x03, 0x04 };

static ElektraKeyset * newPluginConfiguration (void)
{
	// clang-format off
	return elektraKeysetNew (3,
		elektraKeyNew (ELEKTRA_RECIPIENT_KEY, ELEKTRA_KEY_VALUE, TEST_KEY_ID, ELEKTRA_KEY_END),
		elektraKeyNew (ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
		elektraKeyNew (ELEKTRA_SIGNATURE_KEY, ELEKTRA_KEY_VALUE, TEST_KEY_ID, ELEKTRA_KEY_END),
		elektraKeyNew (ELEKTRA_FCRYPT_CONFIG_TEXTMODE, ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on
}

static ElektraKeyset * newPluginConfigurationWithTextmodeEnabled (void)
{
	// clang-format off
	return elektraKeysetNew (3,
		elektraKeyNew (ELEKTRA_RECIPIENT_KEY, ELEKTRA_KEY_VALUE, TEST_KEY_ID, ELEKTRA_KEY_END),
		elektraKeyNew (ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
		elektraKeyNew (ELEKTRA_SIGNATURE_KEY, ELEKTRA_KEY_VALUE, TEST_KEY_ID, ELEKTRA_KEY_END),
		elektraKeyNew (ELEKTRA_FCRYPT_CONFIG_TEXTMODE, ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on
}

static void writeTestFile (const char * file)
{
	FILE * f = fopen (file, "wb");
	succeed_if (f, "can not write to temporary file");
	if (!f) return;
	succeed_if (fwrite (testContent, 1, sizeof (testContent), f) == sizeof (testContent), "test file preparation failed");
	fclose (f);
}

static void writeFaultySignatureFile (const char * file)
{
	FILE * f = fopen (file, "w");
	succeed_if (f, "can not write to temporary file");
	if (!f) return;
	succeed_if (fwrite (FAULTY_SIGNATURE_FILE, 1, strlen (FAULTY_SIGNATURE_FILE), f) == strlen (FAULTY_SIGNATURE_FILE),
		    "test file preparation failed");
	fclose (f);
}

/**
 * @brief read in the content stored in file and compare it to the test vector.
 * @retval 1 if the file content is equal to the test vector.
 * @retval -1 if the file content is not equal to the test vector or an error occurred.
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

static void test_init (void)
{
	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (PLUGIN_NAME, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, PLUGIN_NAME), "got wrong name");

		ElektraKeyset * config = elektraPluginGetConfig (plugin);
		succeed_if (config != 0, "there should be a config");

		succeed_if (plugin->kdbOpen != 0, "no open pointer");
		succeed_if (plugin->kdbClose != 0, "no close pointer");
		succeed_if (plugin->kdbGet != 0, "no get pointer");
		succeed_if (plugin->kdbSet != 0, "no set pointer");

		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
	elektraKeyDel (parentKey);
}

static void test_gpg (void)
{
	// Plugin configuration
	ElektraKeyset * conf = newPluginConfiguration ();
	ElektraKey * errorKey = elektraKeyNew ("/", ELEKTRA_KEY_END);

	// install the gpg key
	char * argv[] = { "", "-a", "--import", NULL };
	const size_t argc = 4;
	ElektraKey * msg = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetBinary (msg, test_key_asc, test_key_asc_len);

	succeed_if (ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msg, argv, argc) == 1, "failed to install the GPG test key");

	elektraKeyDel (msg);
	elektraKeyDel (errorKey);
	elektraKeysetDel (conf);
}

static void test_file_crypto_operations (void)
{
	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (PLUGIN_NAME, modules, config, 0);
	succeed_if (plugin, "failed to open plugin handle");
	if (plugin)
	{
		ElektraKeyset * data = elektraKeysetNew (0, ELEKTRA_KS_END);
		const char * tmpFile = elektraFilename ();
		if (tmpFile)
		{
			// prepare test file to be encrypted
			writeTestFile (tmpFile);
			elektraKeySetString (parentKey, tmpFile);

			// try to encrypt the file
			succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");
			succeed_if (isTestFileCorrect (tmpFile) == -1, "file content did not change during encryption");

			// try to decrypt the file again (simulating the pregetstorage call)
			succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get (pregetstorage) failed");
			succeed_if (isTestFileCorrect (elektraKeyString (parentKey)) == 1, "file content could not be restored during decryption");

			// a second call to kdb get (the postgetstorage call) should re-encrypt the file again
			succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get (postgetstorage) failed");
			succeed_if (isTestFileCorrect (tmpFile) == -1, "postgetstorage did not encrypt the file again");

			remove (tmpFile);
		}

		elektraKeysetDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
	elektraKeyDel (parentKey);
}

static void test_file_signature_operations (void)
{
	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * config = newPluginConfiguration ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (PLUGIN_NAME, modules, config, 0);
	succeed_if (plugin, "failed to open plugin handle");
	if (plugin)
	{
		ElektraKeyset * data = elektraKeysetNew (0, ELEKTRA_KS_END);
		const char * tmpFile = elektraFilename ();
		if (tmpFile)
		{
			// prepare test file to be encrypted
			writeTestFile (tmpFile);
			elektraKeySetString (parentKey, tmpFile);

			// try to encrypt the file
			succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");
			succeed_if (isTestFileCorrect (tmpFile) == -1, "file content did not change during encryption");

			// try to decrypt/verify the file
			succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");

			remove (tmpFile);
		}

		elektraKeysetDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
	elektraKeyDel (parentKey);
}

static void test_file_faulty_signature (void)
{
	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * config = newPluginConfigurationWithTextmodeEnabled ();

	elektraModulesInit (modules, 0);
	plugin = elektraPluginOpen (PLUGIN_NAME, modules, config, 0);
	succeed_if (plugin, "failed to open plugin handle");
	if (plugin)
	{
		ElektraKeyset * data = elektraKeysetNew (0, ELEKTRA_KS_END);
		const char * tmpFile = elektraFilename ();
		if (tmpFile)
		{
			// prepare test file to be encrypted
			writeFaultySignatureFile (tmpFile);
			elektraKeySetString (parentKey, tmpFile);

			// try to decrypt/verify the file -- should fail
			succeed_if (plugin->kdbGet (plugin, data, parentKey) == -1, "kdb get succeeded on a faulty signature");

			remove (tmpFile);
		}

		elektraKeysetDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
	elektraKeyDel (parentKey);
}

int main (int argc, char ** argv)
{
	printf ("FCRYPT       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	if (!gpg_available (newPluginConfiguration ()))
	{
		printf ("The test was disabled because gpg could not be found on the system.\n");
		return nbError;
	}

	test_gpg ();
	test_init ();
	test_file_crypto_operations ();
	test_file_signature_operations ();
	test_file_faulty_signature ();
	test_teardown ();

	print_result (PLUGIN_NAME);
	return nbError;
}
