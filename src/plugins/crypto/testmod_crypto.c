/**
 * @file
 *
 * @brief test suite for the crypto plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <kdb.h>
#include <tests_plugin.h>
#include <tests_internal.h>
#include <kdbinternal.h>
#include "crypto.h"

/*
 * The test vectors are taken from NIST SP 800-38A, section F.2.5 "CBC-AES256.Encrypt"
 * See <http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf> for further information.
 */
static const unsigned char key[] =
{
	0x60, 0x3d, 0xeb, 0x10, 0x15, 0xca, 0x71, 0xbe,
	0x2b, 0x73, 0xae, 0xf0, 0x85, 0x7d, 0x77, 0x81,
	0x1f, 0x35, 0x2c, 0x07, 0x3b, 0x61, 0x08, 0xd7,
	0x2d, 0x98, 0x10, 0xa3, 0x09, 0x14, 0xdf, 0xf4
};

static const unsigned char iv[] =
{
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f
};


static const char strVal[] = "abcde";
static const kdb_octet_t binVal[] = { 0x01, 0x02, 0x03, 0x04 };

/**
 * @brief create new KeySet and add a working configuration to it.
 */
static KeySet *newWorkingConfiguration()
{
	Key *configKey = keyNew("user/crypto/key-derivation/key", KEY_END);
	keySetBinary(configKey, key, sizeof(key));

	Key *configIv = keyNew("user/crypto/key-derivation/iv", KEY_END);
	keySetBinary(configIv, iv, sizeof(iv));

	return ksNew(2,
		configKey,
		configIv,
		KS_END);
}

/**
 * @brief create new KeySet and add an invalid configuration to it.
 *
 * The cryptographic key in the returned KeySet has an invalid size.
 */
static KeySet *newInvalidConfiguration()
{
	const unsigned char wrongKey[] = { 0x01, 0x02, 0x03 };

	Key *configKey = keyNew("user/crypto/key-derivation/key", KEY_END);
	keySetBinary(configKey, wrongKey, sizeof(wrongKey));

	Key *configIv = keyNew("user/crypto/key-derivation/iv", KEY_END);
	keySetBinary(configIv, iv, sizeof(iv));

	return ksNew(2,
		configKey,
		configIv,
		KS_END);
}

/**
 * @brief create new KeySet and add an incomplete configuration to it.
 *
 * The required key "/crypto/key-derivation/key" is missing.
 */
static KeySet *newIncompleteConfiguration()
{
	return ksNew(0, KS_END);
}

/**
 * @brief create a new KeySet holding sample data for encryption and decryption.
 */
static KeySet *newTestdataKeySet()
{
	Key *keyUnchanged1 = keyNew("user/crypto/test/nochange", KEY_END);
	Key *keyUnchanged2 = keyNew("user/crypto/test/nochange2", KEY_END);
	Key *keyNull = keyNew("user/crypto/test/mynull", KEY_END);
	Key *keyString = keyNew("user/crypto/test/mystring", KEY_END);
	Key *keyBin = keyNew("user/crypto/test/mybin", KEY_END);

	keySetString(keyUnchanged1, strVal);

	keySetString(keyUnchanged2, strVal);
	keySetMeta(keyUnchanged2, ELEKTRA_CRYPTO_META_ENCRYPT, "");

	keySetBinary(keyNull, 0, 0);
	keySetMeta(keyNull, ELEKTRA_CRYPTO_META_ENCRYPT, "X");

	keySetString(keyString, strVal);
	keySetMeta(keyString, ELEKTRA_CRYPTO_META_ENCRYPT, "X");

	keySetBinary(keyBin, binVal, sizeof(binVal));
	keySetMeta(keyBin, ELEKTRA_CRYPTO_META_ENCRYPT, "X");

	return ksNew(5,
		keyUnchanged1,
		keyUnchanged2,
		keyNull,
		keyString,
		keyBin,
		KS_END);
}

static void test_init_internal(Plugin *plugin, Key *parentKey)
{
	KeySet *config = elektraPluginGetConfig (plugin);
	succeed_if (config != 0, "there should be a config");

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no close pointer");
	succeed_if (plugin->kdbGet != 0, "no get pointer");
	succeed_if (plugin->kdbSet != 0, "no set pointer");
	succeed_if (plugin->kdbError!= 0, "no error pointer");

	// try re-opening the plugin
	succeed_if (plugin->kdbClose(plugin, parentKey) == 1, "kdb close failed");
	succeed_if (plugin->kdbOpen(plugin, parentKey) == 1, "re-opening the plugin failed");
	succeed_if (plugin->kdbClose(plugin, parentKey) == 1, "kdb close failed");
}

static void test_init()
{
	Plugin *plugin = NULL;
	Key *parentKey = keyNew("system", KEY_END);
	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen ("crypto_gcrypt", modules, newWorkingConfiguration(), 0);
	if (plugin)
	{
		succeed_if (!strcmp(plugin->name, "crypto_gcrypt"), "got wrong name");
		test_init_internal (plugin, parentKey);
		elektraPluginClose(plugin, 0);
	}

	plugin = elektraPluginOpen ("crypto_openssl", modules, newWorkingConfiguration(), 0);
	if (plugin)
	{
		succeed_if (!strcmp(plugin->name, "crypto_openssl"), "got wrong name");
		test_init_internal (plugin, parentKey);
		elektraPluginClose(plugin, 0);
	}

	plugin = elektraPluginOpen ("crypto", modules, newWorkingConfiguration(), 0);
	exit_if_fail (plugin, "could not load crypto_openssl plugin");
	succeed_if (!strcmp(plugin->name, "crypto"), "got wrong name");
	test_init_internal (plugin, parentKey);
	elektraPluginClose(plugin, 0);

	elektraModulesClose(modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

static void test_config_errors_internal(const char *pluginName, KeySet *pluginConfig, int expectedResult, const char *message)
{
	Plugin *plugin = NULL;
	Key *parentKey = keyNew("system", KEY_END);
	KeySet *data = newTestdataKeySet ();
	KeySet *modules = ksNew(0, KS_END);
	elektraModulesInit (modules, 0);

	plugin = elektraPluginOpen (pluginName, modules, pluginConfig, 0);
	if (plugin)
	{
		succeed_if (plugin->kdbSet(plugin, data, parentKey) == expectedResult, message);
		elektraPluginClose(plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	ksDel (data);
	keyDel (parentKey);
}

static void test_config_errors()
{
	// gcrypt tests
	test_config_errors_internal ("crypto_gcrypt", newWorkingConfiguration(), 1, "kdbSet failed with valid config");
	test_config_errors_internal ("crypto_gcrypt", newInvalidConfiguration(), -1, "kdbSet succeeded with invalid config");
	test_config_errors_internal ("crypto_gcrypt", newIncompleteConfiguration(), -1, "kdbSet succeeded with incomplete config");

	// OpenSSL tests
	test_config_errors_internal ("crypto_openssl", newWorkingConfiguration(), 1, "kdbSet failed with valid config");
	test_config_errors_internal ("crypto_openssl", newInvalidConfiguration(), -1, "kdbSet succeeded with invalid config");
	test_config_errors_internal ("crypto_openssl", newIncompleteConfiguration(), -1, "kdbSet succeeded with incomplete config");
}

static void test_crypto_operations_internal(Plugin *plugin, Key *parentKey)
{
	Key *k;
	KeySet *original = newTestdataKeySet();

	// encrypt data by calling kdbSet
	KeySet *data = newTestdataKeySet ();
	succeed_if (plugin->kdbSet (plugin, data, parentKey) == 1, "kdb set failed");

	// verify data changes
	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		const char *name = keyName (k);

		// verify that keys without the encrpytion meta-key value did not change
		if (strstr (name, "nochange"))
		{
			succeed_if (strcmp(strVal, keyString (k)) == 0, "value of non-marked key changed");
		}
	}

	// decrypt data by calling kdbGet
	succeed_if (plugin->kdbGet (plugin, data, parentKey) == 1, "kdb get failed");

	// now we expect the same keySet like before the encryption took place
	compare_keyset (data, original);

	ksDel (data);
	ksDel (original);
}

static void test_crypto_operations()
{
	Plugin *plugin = NULL;
	Key *parentKey = keyNew ("system", KEY_END);
	KeySet *modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	// gcrypt tests
	plugin = elektraPluginOpen ("crypto_gcrypt", modules, newWorkingConfiguration(), 0);
	if (plugin)
	{
		test_crypto_operations_internal (plugin, parentKey);
		elektraPluginClose (plugin, 0);
	}

	// OpenSSL tests
	plugin = elektraPluginOpen ("crypto_openssl", modules, newWorkingConfiguration(), 0);
	if (plugin)
	{
		test_crypto_operations_internal (plugin, parentKey);
		elektraPluginClose (plugin, 0);
	}

	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (parentKey);
}

int main(int argc, char** argv)
{
	printf("CYPTO        TESTS\n");
	printf("==================\n\n");

	init(argc, argv);

	test_init();
	test_config_errors();
	test_crypto_operations();

	printf("\ntestmod_crypto RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}

