/**
 * @file
 *
 * @brief test suite for the crypto plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */
#include "./crypto.h"
#include "./gpg.h"
#include "./helper.h"

#include <elektra/core.h>
#include <elektra/core/errors.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/meta.h>
#include <elektra/plugin/plugin.h>

#include <internal/config.h>
#include <internal/plugin/struct.h>
#include <internal/pluginload/module.h>
#include <internal/utility/logger.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>

#include "./common_gpg_tests.c"
#include "./gpgagent_teardown.h"
#include "./test_key.h"

#define PLUGIN_NAME "crypto"
#define TEST_KEY_ID "DDEBEF9EE2DC931701338212DAF635B17F230E8D"

static KeySet * newPluginConfiguration (void);

typedef int (*checkConfPtr) (Key *, KeySet *);

static const char strVal[] = "abcde";
static const char strValLong[] = "Oh loooooooooooooooooooong Johnson";
static const char strFullBlockSingle[] = "abcdefghijklmno";
static const char strFullBlockDouble[] = "I am root!!!!!!!!!!!!!!!!!!!!!?";
static const kdb_octet_t binVal[] = { 0x01, 0x02, 0x03, 0x04 };

static inline ssize_t MIN (ssize_t a, ssize_t b)
{
	return (a < b) ? a : b;
}

static int isMarkedForEncryption (const Key * k)
{
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypt && strcmp (keyString (metaEncrypt), "1") == 0)
	{
		return 1;
	}
	return 0;
}

/**
 * @brief create a new KeySet holding sample data for encryption and decryption.
 */
static KeySet * newTestdataKeySet (void)
{
	Key * kUnchanged1 = keyNew ("user:/crypto/test/nochange", KEY_END);
	Key * kUnchanged2 = keyNew ("user:/crypto/test/nochange2", KEY_END);
	Key * kNull = keyNew ("user:/crypto/test/mynull", KEY_END);
	Key * kString = keyNew ("user:/crypto/test/mystring", KEY_END);
	Key * kStringLong = keyNew ("user:/crypto/test/myextralongstring", KEY_END);
	Key * kStringFullBlockSingle = keyNew ("user:/crypto/test/myfullblocksingle", KEY_END);
	Key * kStringFullBlockDouble = keyNew ("user:/crypto/test/myfullblockdouble", KEY_END);
	Key * kBin = keyNew ("user:/crypto/test/mybin", KEY_END);

	keySetString (kUnchanged1, strVal);

	keySetString (kUnchanged2, strVal);
	keySetMeta (kUnchanged2, ELEKTRA_CRYPTO_META_ENCRYPT, "0");

	keySetBinary (kNull, 0, 0);
	keySetMeta (kNull, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	keySetString (kString, strVal);
	keySetMeta (kString, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	keySetString (kStringLong, strValLong);
	keySetMeta (kStringLong, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	keySetString (kStringFullBlockSingle, strFullBlockSingle);
	keySetMeta (kStringFullBlockSingle, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	keySetString (kStringFullBlockDouble, strFullBlockDouble);
	keySetMeta (kStringFullBlockDouble, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	keySetBinary (kBin, binVal, sizeof (binVal));
	keySetMeta (kBin, ELEKTRA_CRYPTO_META_ENCRYPT, "1");

	return ksNew (8, kUnchanged1, kUnchanged2, kNull, kString, kStringLong, kStringFullBlockSingle, kStringFullBlockDouble, kBin,
		      KS_END);
}

static inline void setPluginShutdown (KeySet * config)
{
	ksAppendKey (config, keyNew (ELEKTRA_CRYPTO_PARAM_SHUTDOWN, KEY_VALUE, "1", KEY_END));
}

static KeySet * newPluginConfiguration (void)
{
	return ksNew (2, keyNew (ELEKTRA_RECIPIENT_KEY, KEY_VALUE, TEST_KEY_ID, KEY_END),
		      keyNew (ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, KEY_VALUE, "1", KEY_END), KS_END);
}

static void test_init (const char * pluginName)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system:/", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = newPluginConfiguration ();
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, pluginName), "got wrong name");

		KeySet * config = elektraPluginGetConfig (plugin);
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
	ksDel (modules);
	keyDel (parentKey);
}

static void test_incomplete_config (const char * pluginName)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system:/", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * configKs = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, configKs, 0);
	succeed_if (plugin != 0, "failed to open the plugin");
	if (plugin)
	{
		KeySet * data = newTestdataKeySet ();
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == -1, "kdb set succeeded with incomplete configuration");
		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_crypto_operations (const char * pluginName)
{
	union
	{
		checkConfPtr f;
		void * v;
	} conversation;

	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system:/", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	KeySet * config = newPluginConfiguration ();

	setPluginShutdown (config);

	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, config, 0);
	if (plugin)
	{
		KeySet * data = newTestdataKeySet ();
		KeySet * original = ksDup (data);

		// read and check the contract
		KeySet * contract = ksNew (0, KS_END);
		Key * contractParent = keyNew ("system:/elektra/modules/" PLUGIN_NAME, KEY_END);
		succeed_if (plugin->kdbGet (plugin, contract, contractParent) == 1, "kdb get for contract failed");

		// run checkconf to generate the master password
		Key * function = ksLookupByName (contract, "system:/elektra/modules/" PLUGIN_NAME "/exports/checkconf", 0);
		succeed_if (function, "no symbol exported for the checkconf function");
		if (function)
		{
			succeed_if (keyGetBinary (function, &conversation.v, sizeof (conversation)) == sizeof (conversation),
				    "type mismatch in function pointer to checkconf");
			succeed_if (conversation.f, "exported NULL pointer as checkconf function");

			if (conversation.f)
			{
				KeySet * pluginConfig = elektraPluginGetConfig (plugin);
				succeed_if (conversation.f (parentKey, pluginConfig) != -1, "checkconf call failed");
			}
		}

		keyDel (contractParent);
		ksDel (contract);

		// test encryption with kdb set
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");

		// verify key set
		for (elektraCursor it = 0; it < ksGetSize (data); ++it)
		{
			Key * k = ksAtCursor (data, it);
			if (isMarkedForEncryption (k))
			{
				succeed_if (keyIsBinary (k), "Key value is not binary although it should have been encrypted");
				succeed_if (keyGetValueSize (k) > 0, "NULL Key must have encrypted metadata and can not have length 0");
				succeed_if (memcmp (keyValue (k), binVal, MIN (keyGetValueSize (k), (ssize_t) sizeof (binVal))),
					    "encryption failed");
				succeed_if (memcmp (keyValue (k), strVal, MIN (keyGetValueSize (k), (ssize_t) sizeof (strVal))),
					    "encryption failed");
				succeed_if (memcmp (keyValue (k), strValLong, MIN (keyGetValueSize (k), (ssize_t) sizeof (strValLong))),
					    "encryption failed");
			}
			else
			{
				succeed_if (!strcmp (keyString (k), strVal), "Key value changed without being marked for encryption");
			}
		}

		// test decryption with kdb get
		succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");
		compare_keyset (data, original);

		ksDel (original);
		ksDel (data);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_gpg (void)
{
	// Plugin configuration
	KeySet * conf = newPluginConfiguration ();
	Key * errorKey = keyNew ("/", KEY_END);

	// install the gpg key
	char * argv[] = { "", "-a", "--import", NULL };
	const size_t argc = 4;
	Key * msg = keyNew ("/", KEY_END);
	keySetBinary (msg, test_key_asc, test_key_asc_len);

	succeed_if (ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msg, argv, argc) == 1, "failed to install the GPG test key");

	keyDel (msg);
	keyDel (errorKey);
	ksDel (conf);
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
