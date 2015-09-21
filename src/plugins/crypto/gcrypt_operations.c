/**
* @file
*
* @brief cryptographic interface using the gcrypt library
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include "crypto.h"
#include "gcrypt_operations.h"
#include <kdberrors.h>
#include <stdlib.h>
#include <gcrypt.h>


static void addPkcs7Padding(unsigned char *buffer, const unsigned int contentLen, const unsigned int bufferLen);
static unsigned int getPkcs7PaddedContentLen(const unsigned char *buffer, const unsigned int bufferLen);


void elektraCryptoGcryHandleDestroy(elektraCryptoHandle *handle)
{
	if(handle != NULL)
	{
		gcry_cipher_close(*handle);
		elektraFree(handle);
	}
}

int elektraCryptoGcryInit(Key *errorKey)
{
	if (!gcry_check_version(GCRYPT_VERSION))
	{
		ELEKTRA_SET_ERRORF(111, errorKey, "Libgcrypt version check failed, looking for version: %s", GCRYPT_VERSION);
		return (-1);
	}
	gcry_control(GCRYCTL_DISABLE_SECMEM, 0);
	gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
	return 1;
}

int elektraCryptoGcryHandleCreate(elektraCryptoHandle **handle, KeySet *config, Key *errorKey)
{
	gcry_error_t gcry_err;
	unsigned char keyBuffer[64], ivBuffer[64];
	size_t keyLength, ivLength;
	const char *keyPath = "/elektra/modules/crypto/key-derivation/key";
	const char *ivPath = "/elektra/modules/crypto/key-derivation/iv";

	// retrieve keys from configuration
	Key *key = ksLookupByName(config, keyPath, 0);
	if(key == NULL)
	{
		ELEKTRA_SET_ERRORF(116, errorKey, "missing %s in configuration", keyPath);
		return -1;
	}

	Key *iv = ksLookupByName(config, ivPath, 0);
	if(iv == NULL)
	{
		ELEKTRA_SET_ERRORF(116, errorKey, "missing %s in configuration", ivPath);
		return -1;
	}

	keyLength = keyGetBinary(key, keyBuffer, sizeof(keyBuffer));
	ivLength = keyGetBinary(iv, ivBuffer, sizeof(ivBuffer));

	// create the handle
	(*handle) = elektraMalloc(sizeof(elektraCryptoHandle));
	if(*handle == NULL)
	{
		ELEKTRA_SET_ERROR(87, errorKey, "Memory allocation failed");
		return -1;
	}

	if((gcry_err = gcry_cipher_open(*handle, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CBC, 0)) != 0)
	{
		goto error;
	}

	if((gcry_err = gcry_cipher_setkey(**handle, keyBuffer, keyLength)) != 0)
	{
		goto error;
	}

	if((gcry_err = gcry_cipher_setiv(**handle, ivBuffer, ivLength)) != 0)
	{
		goto error;
	}

	return 1;

error:
	ELEKTRA_SET_ERRORF(116, errorKey, "Failed to create handle because: %s", gcry_strerror(gcry_err));
	gcry_cipher_close(**handle);
	elektraFree(*handle);
	(*handle) = NULL;
	return -1;
}

int elektraCryptoGcryEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	const unsigned char *value = (unsigned char*)keyValue(k);
	const size_t valueLen = keyGetValueSize(k);
	size_t outputLen;
	gcry_error_t gcry_err;

	unsigned char *output;
	unsigned char cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned char contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned long i;

	// TODO consider that the original value might be a string value!
	// This should be saved as meta information

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
	output = elektraMalloc(outputLen);
	if(output == NULL)
	{
		ELEKTRA_SET_ERROR(87, errorKey, "Memory allocation failed");
		return (-1);
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
		if(contentLen < ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
		{
			addPkcs7Padding(contentBuffer, contentLen, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		}

		gcry_err = gcry_cipher_encrypt(*handle, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if(gcry_err != 0)
		{
			ELEKTRA_SET_ERRORF(113, errorKey, "Encryption failed because: %s", gcry_strerror(gcry_err));
			elektraFree(output);
			return (-1);
		}
		memcpy((output + i), cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	}

	// write back the cipher text to the key
	keySetBinary(k, output, outputLen);
	elektraFree(output);

	return 1;
}

int elektraCryptoGcryDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	const unsigned char *value = (unsigned char*)keyValue(k);
	const size_t valueLen = keyGetValueSize(k);

	unsigned char *output;
	unsigned char cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned char contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned long i;
	unsigned long written = 0;
	unsigned long lastBlockLen;
	gcry_error_t gcry_err;

	// plausibility check
	if(valueLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE != 0)
	{
		// TODO throw inconsistency error
		return (-1);
	}

	// prepare buffer for plain text output
	output = elektraMalloc(valueLen);
	if(output == NULL)
	{
		ELEKTRA_SET_ERROR(87, errorKey, "Memory allocation failed");
		return (-1);
	}

	// decrypt content block by block (i = start of the current block)
	for(i = 0; i < valueLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		// load cipher text partition into the cipher buffer
		memcpy(cipherBuffer, (value + i), ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);

		gcry_err = gcry_cipher_decrypt(*handle, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if(gcry_err != 0)
		{
			ELEKTRA_SET_ERRORF(114, errorKey, "Decryption failed because: %s", gcry_strerror(gcry_err));
			elektraFree(output);
			return (-1);
		}
		memcpy((output + i), contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		written += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	}

	// consider that the last block may contain a PKCS#7 padding
	lastBlockLen = getPkcs7PaddedContentLen((output + written - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) , ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	if(lastBlockLen < ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		written = written - (ELEKTRA_CRYPTO_GCRY_BLOCKSIZE - lastBlockLen);
	}

	// write back the cipher text to the key
	// TODO consider that the keySetString() function should be applied if the original value was of type string
	keySetBinary(k, output, written);
	elektraFree(output);

	return 1;
}

static void addPkcs7Padding(unsigned char *buffer, const unsigned int contentLen, const unsigned int bufferLen)
{
	/*
	* this function adds a PKCS#7 padding to the buffer.
	* Refer to RFC 5652 for more information:
	* <http://tools.ietf.org/html/rfc5652#section-6.3>
	*/
	const unsigned char n = bufferLen - contentLen;
	unsigned long i;

	if(bufferLen <= contentLen)
	{
		return;
	}

	for(i = bufferLen - n; i < bufferLen; i++)
	{
		buffer[i] = n;
	}
}

static unsigned int getPkcs7PaddedContentLen(const unsigned char *buffer, const unsigned int bufferLen)
{
	const unsigned char n = buffer[bufferLen - 1];
	unsigned int i;

	if(n <= 0 || n >= bufferLen)
	{
		// assume no padding -> full buffer size
		return bufferLen;
	}

	for(i = bufferLen - 2; i >= bufferLen - n; i--)
	{
		if(buffer[i] != n)
		{
			// assume no padding -> full buffer size
			return bufferLen;
		}
	}
	return i + 1;
}

