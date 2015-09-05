/***************************************************************************
 gcrypt_operations.c  -  gcrypt cryptography interface
 -------------------
 begin                : Mon Aug 31 18:04:14 CEST 2015
 copyright            : (C) 2015 by Peter Nirschl
 email                : peter.nirschl@gmail.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include "gcrypt_operations.h"
#include <gcrypt.h>

static gcry_cipher_hd_t gcry_handle;
static gcry_error_t gcry_err;


void elektraCryptoGcryClearKeyIv()
{
	gcry_cipher_close(gcry_handle);
}

int elektraCryptoGcryInit()
{
	if (!gcry_check_version(GCRYPT_VERSION))
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}
	gcry_control(GCRYCTL_DISABLE_SECMEM, 0);
	gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
	return ELEKTRA_CRYPTO_GCRY_OK;
}

int elektraCryptoGcrySetKeyIv(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen)
{
	elektraCryptoGcryClearKeyIv();
	if (gcry_cipher_open(&gcry_handle, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CBC, 0) != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	if (gcry_cipher_setkey(gcry_handle, key, keyLen) != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	if (gcry_cipher_setiv(gcry_handle, iv, ivLen) != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}
	return ELEKTRA_CRYPTO_GCRY_OK;
}

int elektraCryptoGcryEncrypt(Key *k)
{
	const unsigned char *value = (unsigned char*)keyValue(k);
	const size_t valueLen = keyGetValueSize(k);
	size_t outputLen;

	unsigned char *output;
	unsigned char cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned char contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned long i;

	// prepare buffer for cipher text output
	if(valueLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE == 0)
	{
		outputLen = valueLen / ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	}
	else
	{
		outputLen = (valueLen / ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) + 1;
	}
	outputLen *= ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	output = (unsigned char*)malloc(outputLen);
	if(output == NULL)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	// encrypt content block by block (i = start of the current block)
	for(i = 0; i < valueLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		// load content partition into the content buffer
		long contentLen = ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;

		if((i + 1) * ELEKTRA_CRYPTO_GCRY_BLOCKSIZE > valueLen)
		{
			contentLen = valueLen - (i * ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		}
		memcpy(contentBuffer, (value + i), contentLen);
		// TODO add PKCS#7 padding to buffer

		gcry_err = gcry_cipher_encrypt(gcry_handle, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if(gcry_err != 0)
		{
			// TODO forward detailed error description with gcry_strerror() and gcry_strsource()
			free(output);
			return ELEKTRA_CRYPTO_GCRY_NOK;
		}
		memcpy((output + i), cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	}

	// write back the cipher text to the key
	keySetBinary(k, output, outputLen);
	free(output);

	return ELEKTRA_CRYPTO_GCRY_OK;
}

int elektraCryptoGcryDecrypt(Key *k)
{
	const unsigned char *value = (unsigned char*)keyValue(k);
	const size_t valueLen = keyGetValueSize(k);

	unsigned char *output;
	unsigned char cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned char contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned long i;
	unsigned long written = 0;

	// plausibility check
	if(valueLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	// prepare buffer for plain text output
	output = (unsigned char*)malloc(valueLen);
	if(output == NULL)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	// decrypt content block by block (i = start of the current block)
	for(i = 0; i < valueLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		// load cipher text partition into the cipher buffer
		memcpy(cipherBuffer, (value + i), ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);

		gcry_err = gcry_cipher_decrypt(gcry_handle, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if(gcry_err != 0)
		{
			// TODO forward detailed error description with gcry_strerror() and gcry_strsource()
			free(output);
			return ELEKTRA_CRYPTO_GCRY_NOK;
		}
		// TODO consider PKCS#7 padding
		memcpy((output + i), contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		written += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	}

	// write back the cipher text to the key
	keySetBinary(k, output, written);
	free(output);

	return ELEKTRA_CRYPTO_GCRY_OK;
}
