/**
 * @file
 *
 * @brief test suite for the crypto plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include "crypto.h"
#include "gpg.h"
#include "helper.h"
#include <kdb.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>

#include "common_gpg_tests.c"
#include "gpgagent_teardown.h"
#include "test_key.h"

#define PLUGIN_NAME "crypto"
#define TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"

static ElektraKeyset * newPluginConfiguration (void);

typedef int (*checkConfPtr) (ElektraKey *, ElektraKeyset *);

static const char strVal[] = "abcde";
static const char strValLong[] = "Oh loooooooooooooooooooong Johnson";
static const char strFullBlockSingle[] = "abcdefghijklmno";
static const char strFullBlockDouble[] = "I am root!!!!!!!!!!!!!!!!!!!!!?";
static const kdb_octet_t binVal[] = { 0x01, 0x02, 0x03, 0x04 };

static inline ssize_t MIN (ssize_t a, ssize_t b)
{
	return (a < b) ? a : b;
}

static int isMarkedForEncryption (const ElektraKey * k)
{
	const ElektraKey * metaEncrypt = elektraKeyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypt && strcmp (elektraKeyString (metaEncrypt), "1") == 0)
	{
		return 1;
	}
	return 0;
}

/**
 * @brief create a new KeySet holding sample data for encryption and decryption.
 */
static ElektraKeyset * newTestdataKeySet (void)
{
	ElektraKey * kUnchanged1 = elektraKeyNew ("user:/crypto/test/nochange", ELEKTRA_KEY_END);
	ElektraKey * kUnchanged2 = elektraKeyNew ("user:/crypto/test/nochange2", ELEKTRA_KEY_END);
	ElektraKey * kNull = elektraKeyNew ("user:/crypto/test/mynull", ELEKTRA_KEY_END);
	ElektraKey * kString = elektraKeyNew ("user:/crypto/test/mystring", ELEKTRA_KEY_END);
	ElektraKey * kStringLong = elektraKeyNew ("user:/crypto/test/myextralongstring", ELEKTRA_KEY_END);
	ElektraKey * kStringFullBlockSingle = elektraKeyNew ("user:/crypto/test/myfullblocksingle", ELEKTRA_KEY_END);
	ElektraKey * kStringFullBlockDouble = elektraKeyNew ("user:/crypto/test/myfullblockdouble", ELEKTRA_KEY_END);
	ElektraKey * kBin = elektraKeyNew ("user:/crypto/test/mybin", ELEKTRA_KEY_END);

	elektraKeySetString (kUnchanged1, strVal);

	elektraKeySetString (kUnchanged2, strVal);
	elektraKeySetMeta (kUnchanged2, ELEKTRA_CRYPTO_META_ENCRYPT, "0");

	elektraKeySetBinary (kNull, 0, 0);
	elektraKeySetMeta (kNull, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	elektraKeySetString (kString, strVal);
	elektraKeySetMeta (kString, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	elektraKeySetString (kStringLong, strValLong);
	elektraKeySetMeta (kStringLong, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	elektraKeySetString (kStringFullBlockSingle, strFullBlockSingle);
	elektraKeySetMeta (kStringFullBlockSingle, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	elektraKeySetString (kStringFullBlockDouble, strFullBlockDouble);
	elektraKeySetMeta (kStringFullBlockDouble, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	elektraKeySetBinary (kBin, binVal, sizeof (binVal));
	elektraKeySetMeta (kBin, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	return elektraKeysetNew (8, kUnchanged1, kUnchanged2, kNull, kString, kStringLong, kStringFullBlockSingle, kStringFullBlockDouble, kBin,
		      ELEKTRA_KS_END);
}

static inline void setPluginShutdown (ElektraKeyset * config)
{
	elektraKeysetAppendKey (config, elektraKeyNew (ELEKTRA_CRYPTO_PARAM_SHUTDOWN, ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END));
}

static ElektraKeyset * newPluginConfiguration (void)
{
	return elektraKeysetNew (2, elektraKeyNew (ELEKTRA_RECIPIENT_KEY, ELEKTRA_KEY_VALUE, TEST_KEY_ID, ELEKTRA_KEY_END),
		      elektraKeyNew (ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), ELEKTRA_KS_END);
}

static void test_init (const char * pluginName)
{
	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, pluginName), "got wrong name");

		ElektraKeyset * config = elektraPluginGetConfig (plugin);
		succeed_if (config != 0, "there should be a config");

		succeed_if (plugin->kdbOpen != 0, "no open pointer");
		succeed_if (plugin->kdbClose != 0, "no close pointer");
		succeed_if (plugin->kdbGet != 0, "no get pointer");
		succeed_if (plugin->kdbSet != 0, "no set pointer");

		// try re-opening the plugin
		succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "kdb close failed");
		succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "re-opening the plugin failed");
		succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "kdb close failed");

		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
	elektraKeyDel (parentKey);
}

static void test_incomplete_config (const char * pluginName)
{
	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * configKs = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		ElektraKeyset * data = newTestdataKeySet ();
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == -1, "kdb set succeeded with incomplete configuration");
		elektraKeysetDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	elektraKeysetDel (modules);
	elektraKeyDel (parentKey);
}

static void test_crypto_operations (const char * pluginName)
{
	union
	{
		checkConfPtr f;
		void * v;
	} conversation;

	Plugin * plugin = NULL;
	ElektraKey * parentKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
	ElektraKeyset * modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * config = newPluginConfiguration ();

	setPluginShutdown (config);

	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, config, 0);
	if (plugin)
	{
		ElektraKey * k;
		ElektraKeyset * data = newTestdataKeySet ();
		ElektraKeyset * original = elektraKeysetDup (data);

		// read and check the contract
		ElektraKeyset * contract = elektraKeysetNew (0, ELEKTRA_KS_END);
		ElektraKey * contractParent = elektraKeyNew ("system:/elektra/modules/" PLUGIN_NAME, ELEKTRA_KEY_END);
		succeed_if (plugin->kdbGet (plugin, contract, contractParent) == 1, "kdb get for contract failed");

		// run checkconf to generate the master password
		ElektraKey * function = elektraKeysetLookupByName (contract, "system:/elektra/modules/" PLUGIN_NAME "/exports/checkconf", 0);
		succeed_if (function, "no symbol exported for the checkconf function");
		if (function)
		{
			succeed_if (elektraKeyGetBinary (function, &conversation.v, sizeof (conversation)) == sizeof (conversation),
				    "type mismatch in function pointer to checkconf");
			succeed_if (conversation.f, "exported NULL pointer as checkconf function");

			if (conversation.f)
			{
				ElektraKeyset * pluginConfig = elektraPluginGetConfig (plugin);
				succeed_if (conversation.f (parentKey, pluginConfig) != -1, "checkconf call failed");
			}
		}

		elektraKeyDel (contractParent);
		elektraKeysetDel (contract);

		// test encryption with kdb set
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");

		// verify key set
		elektraKeysetRewind (data);
		while ((k = elektraKeysetNext (data)) != 0)
		{
			if (isMarkedForEncryption (k))
			{
				succeed_if (elektraKeyIsBinary (k), "Key value is not binary although it should have been encrypted");
				succeed_if (elektraKeyGetValueSize (k) > 0, "NULL Key must have encrypted metadata and can not have length 0");
				succeed_if (memcmp (elektraKeyValue (k), binVal, MIN (elektraKeyGetValueSize (k), (ssize_t) sizeof (binVal))),
					    "encryption failed");
				succeed_if (memcmp (elektraKeyValue (k), strVal, MIN (elektraKeyGetValueSize (k), (ssize_t) sizeof (strVal))),
					    "encryption failed");
				succeed_if (memcmp (elektraKeyValue (k), strValLong, MIN (elektraKeyGetValueSize (k), (ssize_t) sizeof (strValLong))),
					    "encryption failed");
			}
			else
			{
				succeed_if (!strcmp (elektraKeyString (k), strVal), "Key value changed without being marked for encryption");
			}
		}

		// test decryption with kdb get
		succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");
		compare_keyset (data, original);

		elektraKeysetDel (original);
		elektraKeysetDel (data);
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

int main (int argc, char ** argv)
{
	printf ("CRYPTO       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	if (gpg_available (newPluginConfiguration ()))
	{
		test_gpg ();
		test_init (PLUGIN_NAME);
		test_incomplete_config (PLUGIN_NAME);
		test_crypto_operations (PLUGIN_NAME);
		test_teardown ();
	}
	else
	{
		printf ("The test was disabled because gpg could not be found on the system.\n");
	}

	print_result (PLUGIN_NAME);
	return nbError;
}
