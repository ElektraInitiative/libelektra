/**
* @file
*
* @brief test suite for the crypto plugin
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <stdio.h>
#include <kdb.h>
#include <tests_plugin.h>
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


/**
 * @brief create new KeySet and add a working configuration to it.
 */
static void getWorkingConfiguration(KeySet **ks)
{
	Key *configKey = keyNew("proc/elektra/modules/crypto/key-derivation/key", KEY_END);
	keySetBinary(configKey, key, sizeof(key));

	Key *configIv = keyNew("proc/elektra/modules/crypto/key-derivation/iv", KEY_END);
	keySetBinary(configIv, iv, sizeof(iv));

	(*ks) = ksNew(2,
		configKey,
		configIv,
		KS_END);
}

/**
 * @brief create new KeySet and add an invalid configuration to it.
 *
 * The key in ks has an invalid size.
 */
static void getInvalidConfiguration(KeySet **ks)
{
	const unsigned char wrongKey[] = { 0x01, 0x02, 0x03 };

	Key *configKey = keyNew("proc/elektra/modules/crypto/key-derivation/key", KEY_END);
	keySetBinary(configKey, wrongKey, sizeof(wrongKey));

	Key *configIv = keyNew("proc/elektra/modules/crypto/key-derivation/iv", KEY_END);
	keySetBinary(configIv, iv, sizeof(iv));

	(*ks) = ksNew(2,
		configKey,
		configIv,
		KS_END);
}

/**
 * @brief create new KeySet and add an incomplete configuration to it.
 *
 * The required key "/elektra/modules/crypto/key-derivation/iv" is missing.
 */
static void getIncompleteConfiguration(KeySet **ks)
{
	Key *configKey = keyNew("proc/elektra/modules/crypto/key-derivation/key", KEY_END);
	keySetBinary(configKey, key, sizeof(key));

	(*ks) = ksNew(1,
		configKey,
		KS_END);
}

static void test_init()
{
	Key *errorKey = keyNew(KEY_END);
	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );
	keyDel(errorKey);
	elektraCryptoTeardown();
}

static void test_handle_init()
{
	KeySet *config;
	Key *errorKey = keyNew(KEY_END);
	elektraCryptoHandle *handle;

	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );

	// working config
	getWorkingConfiguration(&config);
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant key failed" );
	elektraCryptoHandleDestroy(handle);
	ksDel(config);

	// invalid key in config
	getInvalidConfiguration(&config);
	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == -1, "handle initialization with non-compliant key succeeded" );
	elektraCryptoHandleDestroy(handle);
	ksDel(config);

	// missing IV in config
	getIncompleteConfiguration(&config);
	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == -1, "handle initialization with incomplete configuration succeeded" );
	elektraCryptoHandleDestroy(handle);
	ksDel(config);

	elektraCryptoTeardown();
	keyDel(errorKey);
}

static void test_enc_and_dec_with_string()
{
	elektraCryptoHandle *handle;
	KeySet *config;
	Key *errorKey = keyNew(KEY_END);
	const char original[] = "Short";
	char content[64] = "";

	getWorkingConfiguration(&config);
	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );

	Key *k = keyNew("user/plugins/crypto/gcrypt/test-enc-dec-string", KEY_END);
	keySetString(k, original);

	// 1. encryption
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant config failed" );
	succeed_if( elektraCryptoEncrypt(handle, k, errorKey) == 1, "encryption failed" );
	elektraCryptoHandleDestroy(handle);

	// 2. decryption
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant config failed" );
	succeed_if( elektraCryptoDecrypt(handle, k, errorKey) == 1, "decryption failed" );
	elektraCryptoHandleDestroy(handle);

	// 3. check result
	succeed_if( keyIsString(k) == 1, "key is of non-string type");
	succeed_if( keyGetString(k, content, sizeof(content)) > 0, "could not retrieve the value of the key" );
	succeed_if( strcmp(original, content) == 0, "decrypted value differs from original");

	keyDel(k);
	keyDel(errorKey);
	ksDel(config);
	elektraCryptoTeardown();
}

static void test_enc_and_dec_with_binary()
{
	elektraCryptoHandle *handle;
	KeySet *config;
	Key *errorKey = keyNew(KEY_END);
	const unsigned char original[] = { 0x00, 0x01, 0x02, 0x03 };
	unsigned char content[64];
	unsigned long read = 0;

	Key *k = keyNew("user/plugins/crypto/gcrypt/test-enc-dec-bin", KEY_END);
	keySetBinary(k, original, sizeof(original));

	getWorkingConfiguration(&config);
	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );

	// 1. encrypt
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant config failed" );
	succeed_if( elektraCryptoEncrypt(handle, k, errorKey) == 1, "encryption failed" );
	elektraCryptoHandleDestroy(handle);

	// 2. decrypt
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant config failed" );
	succeed_if( elektraCryptoDecrypt(handle, k, errorKey) == 1, "decryption failed" );
	elektraCryptoHandleDestroy(handle);

	// 3. check result
	succeed_if( keyIsBinary(k) == 1, "key is of non-binary type");
	read = keyGetBinary(k, content, sizeof(content));
	succeed_if( read == sizeof(original), "decrypted value is of different length than original" );
	if(read == sizeof(original))
	{
		succeed_if( memcmp(original, content, read) == 0, "decrypted value differs from original");
	}

	keyDel(k);
	keyDel(errorKey);
	ksDel(config);
	elektraCryptoTeardown();
}

static void test_enc_and_dec_with_null()
{
	elektraCryptoHandle *handle;
	KeySet *config;
	Key *errorKey = keyNew(KEY_END);

	Key *k = keyNew("user/plugins/crypto/gcrypt/test-enc-dec-null", KEY_END);
	keySetBinary(k, 0, 0);
	succeed_if( keyGetValueSize(k) == 0, "key is not NULL");

	getWorkingConfiguration(&config);
	succeed_if( elektraCryptoInit(errorKey) == 1, "crypto initialization failed" );

	// 1. encrypt
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant config failed" );
	succeed_if( elektraCryptoEncrypt(handle, k, errorKey) == 1, "encryption failed" );
	elektraCryptoHandleDestroy(handle);

	// 2. decrypt
	succeed_if( elektraCryptoHandleCreate(&handle, config, errorKey) == 1, "handle initialization with compliant config failed" );
	succeed_if( elektraCryptoDecrypt(handle, k, errorKey) == 1, "decryption failed" );
	elektraCryptoHandleDestroy(handle);

	// 3. check result
	succeed_if( keyGetValueSize(k) == 0, "key is not NULL");

	keyDel(k);
	keyDel(errorKey);
	ksDel(config);
	elektraCryptoTeardown();
}

int main(int argc, char** argv)
{
	printf("CYPTO        TESTS\n");
	printf("==================\n\n");

	init(argc, argv);

	test_init();
	test_handle_init();
	test_enc_and_dec_with_string();
	test_enc_and_dec_with_binary();
	test_enc_and_dec_with_null();

	printf("\ntestmod_crypto RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);
	return nbError;
}

