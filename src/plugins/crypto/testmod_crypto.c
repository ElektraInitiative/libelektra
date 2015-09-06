/*************************************************************************** 
 *           testmod_crypto.c  - Test suite for crypto
 *                  -------------------
 *  begin                : Mon Aug 31 10:32:51 CEST 2015
 *  copyright            : (C) 2015 by Peter Nirschl
 *  email                : peter.nirschl@gmail.com
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

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

void test_gcrypt_padding_function1()
{
	unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] = "Salty dog";
	const unsigned char expected[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x53, 0x61, 0x6c, 0x74, 0x79, 0x20, 0x64, 0x6f,
		0x67, 0x00, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06
	};
	const unsigned long contentLen = 10;

	elektraCryptoAddPkcs7Padding(buffer, contentLen, sizeof(buffer));
	succeed_if( cmp_buffers(buffer, sizeof(buffer), expected, sizeof(expected)) == 0, "PKCS#7 padding scheme was not set correctly");
}

void test_gcrypt_padding_function2()
{
	unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] = "This is the end";
	const unsigned char expected[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x54, 0x68, 0x69, 0x73, 0x20, 0x69, 0x73, 0x20,
		0x74, 0x68, 0x65, 0x20, 0x65, 0x6e, 0x64, 0x00
	};
	const unsigned long contentLen = 16;

	elektraCryptoAddPkcs7Padding(buffer, contentLen, sizeof(buffer));
	succeed_if( cmp_buffers(buffer, sizeof(buffer), expected, sizeof(expected)) == 0, "PKCS#7 padding scheme was not set correctly");
}

void test_gcrypt_padding_function3()
{
	unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] = "Fooooooooooooo";
	const unsigned char expected[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x46, 0x6f, 0x6f, 0x6f, 0x6f, 0x6f, 0x6f, 0x6f,
		0x6f, 0x6f, 0x6f, 0x6f, 0x6f, 0x6f, 0x00, 0x01
	};
	const unsigned long contentLen = 15;

	elektraCryptoAddPkcs7Padding(buffer, contentLen, sizeof(buffer));
	succeed_if( cmp_buffers(buffer, sizeof(buffer), expected, sizeof(expected)) == 0, "PKCS#7 padding scheme was not set correctly");
}

void test_gcrypt_padding_length1()
{
	const unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x53, 0x65, 0x6c, 0x66, 0x69, 0x73, 0x68, 0x20,
		0x6d, 0x61, 0x6e, 0x00, 0x04, 0x04, 0x04, 0x04
	};
	const unsigned long expcetedLen = 12;
	succeed_if( elektraCryptoGetPkcs7PaddedContentLen(buffer, sizeof(buffer)) == expcetedLen, "PKCS#7 padded content length calculation failed" );
}

void test_gcrypt_padding_length2()
{
	const unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x53, 0x65, 0x6c, 0x66, 0x69, 0x73, 0x68, 0x20,
		0x6d, 0x61, 0x6e, 0x00, 0xff, 0x04, 0x04, 0x04
	};
	const unsigned long expcetedLen = 16;
	succeed_if( elektraCryptoGetPkcs7PaddedContentLen(buffer, sizeof(buffer)) == expcetedLen, "PKCS#7 padded content length calculation failed" );
}

void test_gcrypt_padding_length3()
{
	const unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x53, 0x65, 0x6c, 0x66, 0x69, 0x73, 0x68, 0x20,
		0x6d, 0x61, 0x6e, 0x00, 0xff, 0x03, 0x04, 0x05
	};
	const unsigned long expcetedLen = 16;
	succeed_if( elektraCryptoGetPkcs7PaddedContentLen(buffer, sizeof(buffer)) == expcetedLen, "PKCS#7 padded content length calculation failed" );
}

void test_gcrypt_padding_length4()
{
	const unsigned char buffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] =
	{
		0x53, 0x65, 0x6c, 0x66, 0x69, 0x73, 0x68, 0x20,
		0x6d, 0x61, 0x6e, 0x00, 0xff, 0x03, 0x04, 0x01
	};
	const unsigned long expcetedLen = 15;
	succeed_if( elektraCryptoGetPkcs7PaddedContentLen(buffer, sizeof(buffer)) == expcetedLen, "PKCS#7 padded content length calculation failed" );
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
	test_gcrypt_padding_function1();
	test_gcrypt_padding_function2();
	test_gcrypt_padding_function3();
	test_gcrypt_padding_length1();
	test_gcrypt_padding_length2();
	test_gcrypt_padding_length3();
	test_gcrypt_padding_length4();

	elektraCryptoGcryClearKeyIv();

	printf("\ntestmod_crypto RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

