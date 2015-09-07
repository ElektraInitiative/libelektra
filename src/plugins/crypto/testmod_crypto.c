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
#include "gcrypt_operations.h"

/*
 * The test vectors are taken from NIST SP 800-38A, section F.2.5 "CBC-AES256.Encrypt"
 * See <http://csrc.nist.gov/publications/nistpubs/800-38a/sp800-38a.pdf> for further information.
 */
const unsigned char key[] =
{
	0x60, 0x3d, 0xeb, 0x10, 0x15, 0xca, 0x71, 0xbe,
	0x2b, 0x73, 0xae, 0xf0, 0x85, 0x7d, 0x77, 0x81,
	0x1f, 0x35, 0x2c, 0x07, 0x3b, 0x61, 0x08, 0xd7,
	0x2d, 0x98, 0x10, 0xa3, 0x09, 0x14, 0xdf, 0xf4
};

const unsigned char iv[] =
{
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f
};

const unsigned char plainText[] =
{
	0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96,
	0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93, 0x17, 0x2a
};

const unsigned char expectedCipherText[] =
{
	0xf5, 0x8c, 0x4c, 0x04, 0xd6, 0xe5, 0xf1, 0xba,
	0x77, 0x9e, 0xab, 0xfb, 0x5f, 0x7b, 0xfb, 0xd6
};

/**
 * Helper function that returns zero (0) if the contents of the buffers b1 and b2 match
 * and non-zero otherwise.
 */
int cmp_buffers(const unsigned char *b1, size_t len1, const unsigned char* b2, size_t len2)
{
	unsigned int i;

	if (len1 != len2)
	{
		return 1;
	}

	for(i = 0; i < len1 && i < len2; i++)
	{
		if (b1[i] != b2[i])
		{
			return 1;
		}
	}
	return 0;
}

void test_gcrypt_init()
{
	succeed_if( elektraCryptoGcryInit() == ELEKTRA_CRYPTO_GCRY_OK, "libgcrypt initialization failed" );
}

void test_gcrypt_handle_init()
{
	const unsigned char shortKey[] = { 0xca, 0xfe };

	succeed_if( elektraCryptoGcrySetKeyIv(shortKey, sizeof(shortKey), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_NOK, "key initialization with non-compliant key succeeded" );
	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
}

void test_gcrypt_encryption()
{
	Key *k;
	unsigned char buffer[64];
	size_t len;

	k = keyNew("/user/plugins/crypto/gcrypt/test-encryption", KEY_END);
	keySetBinary(k, plainText, sizeof(plainText));

	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
	succeed_if( elektraCryptoGcryEncrypt(k) == ELEKTRA_CRYPTO_GCRY_OK, "encryption failed" );

	// compare encrypted value to the test vector
	len = keyGetBinary(k, buffer, sizeof(buffer));
	succeed_if( len > 0, "encrypted value has length 0");
	succeed_if( cmp_buffers(buffer, len, expectedCipherText, sizeof(expectedCipherText)) == 0, "ciphertext does not match the test vector" );

	keyDel(k);
}

void test_gcrypt_decryption()
{
	Key *k;
	unsigned char buffer[64];
	size_t len;

	k = keyNew("/user/plugins/crypto/gcrypt/test-decryption", KEY_END);
	keySetBinary(k, expectedCipherText, sizeof(expectedCipherText));

	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
	succeed_if( elektraCryptoGcryDecrypt(k) == ELEKTRA_CRYPTO_GCRY_OK, "decryption failed" );

	// compare the decrypted value to original plain text
	len = keyGetBinary(k, buffer, sizeof(buffer));
	succeed_if( len > 0, "decrypted value has length 0");
	succeed_if( cmp_buffers(buffer, len, plainText, sizeof(plainText)) == 0, "decrypted value does not match the test vector" );

	keyDel(k);
}

void test_gcrypt_padding_with_string()
{
	const char original[] = "Short";
	char content[64];
	Key *k = keyNew("/user/plugins/crypto/gcrypt/test-padding", KEY_END);
	keySetString(k, original);

	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
	succeed_if( elektraCryptoGcryEncrypt(k) == ELEKTRA_CRYPTO_GCRY_OK, "encryption failed" );
	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
	succeed_if( elektraCryptoGcryDecrypt(k) == ELEKTRA_CRYPTO_GCRY_OK, "decryption failed" );

	succeed_if( keyIsString(k) == 1, "key is of non-string type");
	succeed_if( keyGetString(k, content, sizeof(content)) > 0, "could not retrieve the value of the key" );
	succeed_if( strcmp(original, content) == 0, "decrypted value differs from original");

	keyDel(k);
}

void test_gcrypt_padding_with_binary()
{
	const unsigned char original[] = { 0x00, 0x01, 0x02, 0x03 };
	unsigned char content[64];
	unsigned long read = 0;
	Key *k = keyNew("/user/plugins/crypto/gcrypt/test-padding-bin", KEY_END);
	keySetBinary(k, original, sizeof(original));

	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
	succeed_if( elektraCryptoGcryEncrypt(k) == ELEKTRA_CRYPTO_GCRY_OK, "encryption failed" );
	succeed_if( elektraCryptoGcrySetKeyIv(key, sizeof(key), iv, sizeof(iv)) == ELEKTRA_CRYPTO_GCRY_OK, "key/IV initialization with compliant key failed" );
	succeed_if( elektraCryptoGcryDecrypt(k) == ELEKTRA_CRYPTO_GCRY_OK, "decryption failed" );

	succeed_if( keyIsBinary(k) == 1, "key is of non-binary type");
	read = keyGetBinary(k, content, sizeof(content));
	succeed_if( read == sizeof(original), "decrypted value is of different length than original" );
	succeed_if( cmp_buffers(original, sizeof(original), content, read) == 0, "decrypted value differs from original");

	keyDel(k);
}

int main(int argc, char** argv)
{
	printf("CYPTO        TESTS\n");
	printf("==================\n\n");

	init(argc, argv);

	test_gcrypt_init();
	test_gcrypt_handle_init();
	test_gcrypt_encryption();
	test_gcrypt_decryption();
	test_gcrypt_padding_with_string();
	test_gcrypt_padding_with_binary();

	elektraCryptoGcryClearKeyIv();

	printf("\ntestmod_crypto RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

