/**
* @file
*
* @brief cryptographic interface using the gcrypt library
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

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
		if(contentLen < ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
		{
			elektraCryptoAddPkcs7Padding(contentBuffer, contentLen, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		}

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
	unsigned long lastBlockLen;

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
		memcpy((output + i), contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		written += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	}

	// consider that the last block may contain a PKCS#7 padding
	lastBlockLen = elektraCryptoGetPkcs7PaddedContentLen((output + written - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) , ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	if(lastBlockLen < ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		written = written - (ELEKTRA_CRYPTO_GCRY_BLOCKSIZE - lastBlockLen);
	}

	// write back the cipher text to the key
	// TODO consider that the keySetString() function should be applied if the original value was of type string
	keySetBinary(k, output, written);
	free(output);

	return ELEKTRA_CRYPTO_GCRY_OK;
}

void elektraCryptoAddPkcs7Padding(unsigned char *buffer, const unsigned int contentLen, const unsigned int bufferLen)
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

unsigned int elektraCryptoGetPkcs7PaddedContentLen(const unsigned char *buffer, const unsigned int bufferLen)
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

