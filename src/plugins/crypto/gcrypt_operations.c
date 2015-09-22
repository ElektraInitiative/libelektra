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
		ELEKTRA_SET_ERRORF(150, errorKey, "Libgcrypt version check failed, looking for version: %s", GCRYPT_VERSION);
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

	(*handle) = NULL;

	// retrieve keys from configuration
	Key *key = ksLookupByName(config, keyPath, 0);
	if(key == NULL)
	{
		ELEKTRA_SET_ERRORF(155, errorKey, "missing %s in configuration", keyPath);
		return -1;
	}

	Key *iv = ksLookupByName(config, ivPath, 0);
	if(iv == NULL)
	{
		ELEKTRA_SET_ERRORF(155, errorKey, "missing %s in configuration", ivPath);
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
	ELEKTRA_SET_ERRORF(155, errorKey, "Failed to create handle because: %s", gcry_strerror(gcry_err));
	gcry_cipher_close(**handle);
	elektraFree(*handle);
	(*handle) = NULL;
	return -1;
}

int elektraCryptoGcryEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	const unsigned char *value = (unsigned char*)keyValue(k);
	size_t outputLen;
	gcry_error_t gcry_err;

	struct ElektraCryptoHeader header;
	unsigned char *output;
	unsigned char cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned char contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned long i;

	// check if key has been marked for encryption
	const Key *metaEncrypt = keyGetMeta(k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if(metaEncrypt == NULL || strlen(keyValue(metaEncrypt)) == 0)
	{
		// nothing to do
		return 1;
	}

	// prepare the crypto header
	header.contentLen = keyGetValueSize(k);
	header.flags = ELEKTRA_CRYPTO_FLAG_NONE;

	switch(keyIsString(k))
	{
	case 1: // string
		header.flags = ELEKTRA_CRYPTO_FLAG_STRING;
		break;
	case 0: // binary
		break;
	case -1: // NULL pointer
		header.flags = ELEKTRA_CRYPTO_FLAG_NULL;
		break;
	}

	// prepare buffer for cipher text output
	// NOTE the header goes into the first block
	if(header.contentLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE == 0)
	{
		outputLen = (header.contentLen / ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) + 1;
	}
	else
	{
		outputLen = (header.contentLen / ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) + 2;
	}
	outputLen *= ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	output = elektraMalloc(outputLen);
	if(output == NULL)
	{
		ELEKTRA_SET_ERROR(87, errorKey, "Memory allocation failed");
		return (-1);
	}

	// encrypt the header (1st block)
	memcpy(contentBuffer, &header, sizeof(struct ElektraCryptoHeader));
	gcry_err = gcry_cipher_encrypt(*handle, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	if(gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF(152, errorKey, "Encryption failed because: %s", gcry_strerror(gcry_err));
		elektraFree(output);
		return (-1);
	}
	memcpy(output, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);

	// encrypt content block by block (i = start of the current block)
	for(i = 0; i < header.contentLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		// load content partition into the content buffer
		long contentLen = ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
		if((i + 1) * ELEKTRA_CRYPTO_GCRY_BLOCKSIZE > header.contentLen)
		{
			contentLen = header.contentLen - (i * ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		}
		memcpy(contentBuffer, (value + i), contentLen);

		gcry_err = gcry_cipher_encrypt(*handle, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if(gcry_err != 0)
		{
			ELEKTRA_SET_ERRORF(152, errorKey, "Encryption failed because: %s", gcry_strerror(gcry_err));
			elektraFree(output);
			return (-1);
		}
		memcpy((output + i + ELEKTRA_CRYPTO_GCRY_BLOCKSIZE), cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	}

	// write back the cipher text to the key
	keySetBinary(k, output, outputLen);
	keySetMeta(k, ELEKTRA_CRYPTO_META_ENCRYPTED, "X");
	elektraFree(output);

	return 1;
}

int elektraCryptoGcryDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	const unsigned char *value = (unsigned char*)keyValue(k);
	const size_t valueLen = keyGetValueSize(k);

	struct ElektraCryptoHeader header;
	unsigned char *output;
	unsigned char cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned char contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	unsigned long i;
	unsigned long written = 0;
	gcry_error_t gcry_err;

	// check if key has been encrypted in the first place
	const Key *metaEncrypted = keyGetMeta(k, ELEKTRA_CRYPTO_META_ENCRYPTED);
	if(metaEncrypted == NULL || strlen(keyValue(metaEncrypted)) == 0)
	{
		// nothing to do
		return 1;
	}

	// plausibility check
	if(valueLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE != 0)
	{
		ELEKTRA_SET_ERROR(153, errorKey, "value length is not a multiple of the block size");
		return (-1);
	}

	// prepare buffer for plain text output
	output = elektraMalloc(valueLen);
	if(output == NULL)
	{
		ELEKTRA_SET_ERROR(87, errorKey, "Memory allocation failed");
		return (-1);
	}

	// decrypt the header (1st block)
	memcpy(cipherBuffer, value, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	gcry_err = gcry_cipher_decrypt(*handle, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	if(gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF(153, errorKey, "Decryption failed because: %s", gcry_strerror(gcry_err));
		elektraFree(output);
		return (-1);
	}
	memcpy(&header, contentBuffer, sizeof(struct ElektraCryptoHeader));

	// decrypt content block by block
	// (i = start of the current block and the 1st block has already been consumed)
	for(i = ELEKTRA_CRYPTO_GCRY_BLOCKSIZE; i < valueLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		memcpy(cipherBuffer, (value + i), ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		gcry_err = gcry_cipher_decrypt(*handle, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if(gcry_err != 0)
		{
			ELEKTRA_SET_ERRORF(153, errorKey, "Decryption failed because: %s", gcry_strerror(gcry_err));
			elektraFree(output);
			return (-1);
		}
		memcpy((output + i - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE), contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		written += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	}

	if(written < header.contentLen)
	{
		ELEKTRA_SET_ERROR(153, errorKey, "Content was shorter than described in the header");
		elektraFree(output);
		return (-1);
	}

	// write back the cipher text to the key
	if((header.flags & ELEKTRA_CRYPTO_FLAG_STRING) == ELEKTRA_CRYPTO_FLAG_STRING)
	{
		keySetString(k, (const char*)output);
	}
	else if((header.flags & ELEKTRA_CRYPTO_FLAG_NULL) == ELEKTRA_CRYPTO_FLAG_NULL || header.contentLen == 0)
	{
		keySetBinary(k, NULL, 0);
	}
	else
	{
		keySetBinary(k, output, header.contentLen);
	}
	keySetMeta(k, ELEKTRA_CRYPTO_META_ENCRYPTED, "");

	elektraFree(output);
	return 1;
}
