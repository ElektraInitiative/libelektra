/**
 * @file
 *
 * @brief test suite for the crypto plugin.
 * Contains shared functions for all compile variants.
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "crypto.h"
#include <kdb.h>
#include <kdbinternal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tests_internal.h>
#include <tests_plugin.h>

/*
 * The test vectors are taken from NIST SP 800-38A, section F.2.5 "CBC-AES256.Encrypt"
 * See <http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf> for further information.
 */
static const unsigned char key[] = { 0x60, 0x3d, 0xeb, 0x10, 0x15, 0xca, 0x71, 0xbe, 0x2b, 0x73, 0xae, 0xf0, 0x85, 0x7d, 0x77, 0x81,
				     0x1f, 0x35, 0x2c, 0x07, 0x3b, 0x61, 0x08, 0xd7, 0x2d, 0x98, 0x10, 0xa3, 0x09, 0x14, 0xdf, 0xf4 };

static const unsigned char iv[] = { 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f };


static const char strVal[] = "abcde";
static const kdb_octet_t binVal[] = { 0x01, 0x02, 0x03, 0x04 };

/**
 * @brief create new KeySet and add a working configuration to it.
 * @param shutdown set to 1 if the shutdown key should be appended.
 *                 If this key is set, the crypto library will be de cleaned up.
 */
static KeySet * newWorkingConfiguration (int shutdown)
{
	Key * configKey = keyNew ("user/crypto/key", KEY_END);
	keySetBinary (configKey, key, sizeof (key));

	Key * configIv = keyNew ("user/crypto/iv", KEY_END);
	keySetBinary (configIv, iv, sizeof (iv));

	if (shutdown)
	{
		Key * configShutdown = keyNew ("user/shutdown", KEY_END);
		keySetString (configShutdown, "1");
		return ksNew (3, configKey, configIv, configShutdown, KS_END);
	}

	return ksNew (2, configKey, configIv, KS_END);
}

/**
 * @brief create new KeySet and add an invalid configuration to it.
 *
 * The cryptographic key in the returned KeySet has an invalid size.
 */
static KeySet * newInvalidConfiguration ()
{
	const unsigned char wrongKey[] = { 0x01, 0x02, 0x03 };

	Key * configKey = keyNew ("user/crypto/key", KEY_END);
	keySetBinary (configKey, wrongKey, sizeof (wrongKey));

	Key * configIv = keyNew ("user/crypto/iv", KEY_END);
	keySetBinary (configIv, iv, sizeof (iv));

	return ksNew (2, configKey, configIv, KS_END);
}

/**
 * @brief create new KeySet and add an incomplete configuration to it.
 *
 * The required key "/crypto/key-derivation/key" is missing.
 */
static KeySet * newIncompleteConfiguration ()
{
	return ksNew (0, KS_END);
}

/**
 * @brief create a new KeySet holding sample data for encryption and decryption.
 */
static KeySet * newTestdataKeySet ()
{
	Key * kUnchanged1 = keyNew ("user/crypto/test/nochange", KEY_END);
	Key * kUnchanged2 = keyNew ("user/crypto/test/nochange2", KEY_END);
	Key * kNull = keyNew ("user/crypto/test/mynull", KEY_END);
	Key * kString = keyNew ("user/crypto/test/mystring", KEY_END);
	Key * kBin = keyNew ("user/crypto/test/mybin", KEY_END);

	keySetString (kUnchanged1, strVal);

	keySetString (kUnchanged2, strVal);
	keySetMeta (kUnchanged2, ELEKTRA_CRYPTO_META_ENCRYPT, "");

	keySetBinary (kNull, 0, 0);
	keySetMeta (kNull, ELEKTRA_CRYPTO_META_ENCRYPT, "X");

	keySetString (kString, strVal);
	keySetMeta (kString, ELEKTRA_CRYPTO_META_ENCRYPT, "X");

	keySetBinary (kBin, binVal, sizeof (binVal));
	keySetMeta (kBin, ELEKTRA_CRYPTO_META_ENCRYPT, "X");

	return ksNew (5, kUnchanged1, kUnchanged2, kNull, kString, kBin, KS_END);
}

static void test_init_internal (Plugin * plugin, Key * parentKey)
{
	KeySet * config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no close pointer");
	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");
	succeed_if (plugin->kdbError != 0, "no error pointer");

	// try re-opening the plugin
	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "kdb close failed");
	succeed_if (plugin->kdbOpen (plugin, parentKey) == 1, "re-opening the plugin failed");
	succeed_if (plugin->kdbClose (plugin, parentKey) == 1, "kdb close failed");
}

static void test_init (const char * pluginName)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, newWorkingConfiguration (0), 0);
	if (plugin)
	{
		succeed_if (!strcmp (plugin->name, pluginName), "got wrong name");
		test_init_internal (plugin, parentKey);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_config_errors_internal (const char * pluginName, KeySet * pluginConfig, int expectedResult, const char * message)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * data = newTestdataKeySet ();
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, pluginConfig, 0);
	if (plugin)
	{
		succeed_if (plugin->kdbSet (plugin, data, parentKey) == expectedResult, message);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	ksDel (data);
	keyDel (parentKey);
}

static void test_config_errors (const char * pluginName)
{
	test_config_errors_internal (pluginName, newWorkingConfiguration (0), 1, "kdbSet failed with valid config");
	test_config_errors_internal (pluginName, newInvalidConfiguration (), -1, "kdbSet succeeded with invalid config");
	test_config_errors_internal (pluginName, newIncompleteConfiguration (), -1, "kdbSet succeeded with incomplete config");
}

static void test_crypto_operations_internal (Plugin * plugin, Key * parentKey)
{
	Key * k;
	KeySet * original = newTestdataKeySet ();

	// encrypt data by calling kdbSet
	KeySet * data = newTestdataKeySet ();
	succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");

	// verify data changes
	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		const char * name = keyName (k);

		// verify that keys without the encrpytion meta-key value did not change
		if (strstr (name, "nochange"))
		{
			succeed_if (strcmp (strVal, keyString (k)) == 0, "value of non-marked key changed");
		}
	}

	// decrypt data by calling kdbGet
	succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");

	// now we expect the same keySet like before the encryption took place
	compare_keyset (data, original);

	ksDel (data);
	ksDel (original);
}

static void test_crypto_operations (const char * pluginName)
{
	Plugin * plugin = NULL;
	Key * parentKey = keyNew ("system", KEY_END);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, newWorkingConfiguration (1), 0);
	if (plugin)
	{
		test_crypto_operations_internal (plugin, parentKey);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}
