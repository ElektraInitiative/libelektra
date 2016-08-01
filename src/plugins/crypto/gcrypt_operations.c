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
#include "rand_helper.h"

#include <errno.h>
#include <gcrypt.h>
#include <kdberrors.h>
#include <kdbtypes.h>
#include <pthread.h>
#include <stdlib.h>


// initialize the gcrypt threading subsystem
// NOTE: old versions of libgcrypt require the functions defined in this macro!
GCRY_THREAD_OPTION_PTHREAD_IMPL;

/**
 * @brief read the cryptographic key from the given keyset.
 * @retval NULL on error
 * @retval address of the (Elektra) key in the given keyset holding the cryptographic key to be used.
 */
static Key * elektraCryptoReadParamKey (KeySet * config, Key * errorKey)
{
	Key * key = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_KEY_PATH, 0);
	if (key == NULL)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in configuration",
				    ELEKTRA_CRYPTO_PARAM_KEY_PATH);
	}
	return key;
}

/**
 * @brief read the cryptographic initialization vector (IV) from the given keyset.
 * @retval NULL on error
 * @retval address of the (Elektra) key in the given keyset holding the IV to be used.
 */
static Key * elektraCryptoReadParamIv (KeySet * config, Key * errorKey)
{
	Key * iv = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_IV_PATH, 0);
	if (iv == NULL)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in configuration",
				    ELEKTRA_CRYPTO_PARAM_IV_PATH);
	}
	return iv;
}

void elektraCryptoGcryHandleDestroy (elektraCryptoHandle * handle)
{
	if (handle != NULL)
	{
		gcry_cipher_close (*handle);
		elektraFree (handle);
	}
}

int elektraCryptoGcryInit (Key * errorKey)
{
	// check if gcrypt has already been initialized (possibly by the application)
	if (gcry_control (GCRYCTL_INITIALIZATION_FINISHED_P))
	{
		return 1;
	}

	// initialize the gcrypt threading subsystem
	// NOTE: this is a dummy call in newer versions of gcrypt, but old versions require it
	gcry_control (GCRYCTL_SET_THREAD_CBS, &gcry_threads_pthread);

	// initialize the rest of the gcrypt library
	if (!gcry_check_version (GCRYPT_VERSION))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INIT, errorKey, "Libgcrypt version check failed, looking for version: %s",
				    GCRYPT_VERSION);
		return (-1);
	}
	gcry_control (GCRYCTL_DISABLE_SECMEM, 0);
	gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);
	return 1;
}

int elektraCryptoGcryHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey)
{
	gcry_error_t gcry_err;
	unsigned char keyBuffer[64], ivBuffer[64];
	size_t keyLength, ivLength;

	(*handle) = NULL;

	// retrieve keys from configuration
	Key * key = elektraCryptoReadParamKey (config, errorKey);
	Key * iv = elektraCryptoReadParamIv (config, errorKey);
	if (key == NULL || iv == NULL)
	{
		return (-1);
	}

	keyLength = keyGetBinary (key, keyBuffer, sizeof (keyBuffer));
	ivLength = keyGetBinary (iv, ivBuffer, sizeof (ivBuffer));

	// create the handle
	(*handle) = elektraMalloc (sizeof (elektraCryptoHandle));
	if (*handle == NULL)
	{
		memset (keyBuffer, 0, sizeof (keyBuffer));
		memset (ivBuffer, 0, sizeof (ivBuffer));
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return (-1);
	}

	if ((gcry_err = gcry_cipher_open (*handle, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CBC, 0)) != 0)
	{
		goto error;
	}

	if ((gcry_err = gcry_cipher_setkey (**handle, keyBuffer, keyLength)) != 0)
	{
		goto error;
	}

	if ((gcry_err = gcry_cipher_setiv (**handle, ivBuffer, ivLength)) != 0)
	{
		goto error;
	}

	memset (keyBuffer, 0, sizeof (keyBuffer));
	memset (ivBuffer, 0, sizeof (ivBuffer));
	return 1;

error:
	memset (keyBuffer, 0, sizeof (keyBuffer));
	memset (ivBuffer, 0, sizeof (ivBuffer));
	ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "Failed to create handle because: %s", gcry_strerror (gcry_err));
	gcry_cipher_close (**handle);
	elektraFree (*handle);
	(*handle) = NULL;
	return (-1);
}

int elektraCryptoGcryEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	const kdb_octet_t * value = (kdb_octet_t *)keyValue (k);
	size_t outputLen;
	gcry_error_t gcry_err;

	kdb_octet_t * output;
	kdb_octet_t cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	kdb_octet_t contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE] = { 0 };

	// check if key has been marked for encryption
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypt == NULL || strlen (keyValue (metaEncrypt)) == 0)
	{
		// nothing to do
		return 1;
	}

	// prepare the crypto header data
	const kdb_unsigned_long_t contentLen = keyGetValueSize (k);
	kdb_octet_t flags;

	switch (keyIsString (k))
	{
	case 1: // string
		flags = ELEKTRA_CRYPTO_FLAG_STRING;
		break;
	case -1: // NULL pointer
		flags = ELEKTRA_CRYPTO_FLAG_NULL;
		break;
	default: // binary
		flags = ELEKTRA_CRYPTO_FLAG_NONE;
		break;
	}

	// prepare buffer for cipher text output
	// NOTE the header goes into the first block
	if (contentLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE == 0)
	{
		outputLen = (contentLen / ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) + 1;
	}
	else
	{
		outputLen = (contentLen / ELEKTRA_CRYPTO_GCRY_BLOCKSIZE) + 2;
	}
	outputLen *= ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	output = elektraMalloc (outputLen);
	if (output == NULL)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return (-1);
	}

	// encrypt the header (1st block)
	memcpy (contentBuffer, &flags, sizeof (flags));
	memcpy (contentBuffer + sizeof (flags), &contentLen, sizeof (contentLen));
	gcry_err = gcry_cipher_encrypt (*handle, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption failed because: %s", gcry_strerror (gcry_err));
		elektraFree (output);
		return (-1);
	}
	memcpy (output, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);

	// encrypt content block by block (i = start of the current block)
	for (kdb_unsigned_long_t i = 0; i < contentLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		// load content partition into the content buffer
		kdb_unsigned_long_t partitionLen = ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
		if ((i + 1) * ELEKTRA_CRYPTO_GCRY_BLOCKSIZE > contentLen)
		{
			partitionLen = contentLen - (i * ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		}
		memcpy (contentBuffer, (value + i), partitionLen);

		gcry_err = gcry_cipher_encrypt (*handle, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, contentBuffer,
						ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if (gcry_err != 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption failed because: %s",
					    gcry_strerror (gcry_err));
			elektraFree (output);
			return (-1);
		}
		memcpy ((output + i + ELEKTRA_CRYPTO_GCRY_BLOCKSIZE), cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	}

	// write back the cipher text to the key
	keySetBinary (k, output, outputLen);
	elektraFree (output);

	return 1;
}

int elektraCryptoGcryDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	kdb_octet_t * value = (kdb_octet_t *)keyValue (k);
	const size_t valueLen = keyGetValueSize (k);

	kdb_octet_t * output;
	kdb_octet_t cipherBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	kdb_octet_t contentBuffer[ELEKTRA_CRYPTO_GCRY_BLOCKSIZE];
	kdb_unsigned_long_t written = 0;
	gcry_error_t gcry_err;

	// initialize crypto header data
	kdb_unsigned_long_t contentLen = 0;
	kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;

	// check if key has been encrypted in the first place
	const Key * metaEncrypted = keyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypted == NULL || strlen (keyValue (metaEncrypted)) == 0)
	{
		// nothing to do
		return 1;
	}

	// plausibility check
	if (valueLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE != 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "value length is not a multiple of the block size");
		return (-1);
	}

	// prepare buffer for plain text output
	output = elektraMalloc (valueLen);
	if (output == NULL)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return (-1);
	}

	// decrypt the header (1st block)
	memcpy (cipherBuffer, value, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	gcry_err = gcry_cipher_decrypt (*handle, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, cipherBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption failed because: %s", gcry_strerror (gcry_err));
		elektraFree (output);
		return (-1);
	}

	// restore the header data
	memcpy (&flags, contentBuffer, sizeof (flags));
	memcpy (&contentLen, contentBuffer + sizeof (flags), sizeof (contentLen));

	// decrypt content block by block
	// (i = start of the current block and the 1st block has already been consumed)
	for (kdb_unsigned_long_t i = ELEKTRA_CRYPTO_GCRY_BLOCKSIZE; i < valueLen; i += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)
	{
		memcpy (cipherBuffer, (value + i), ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		gcry_err = gcry_cipher_decrypt (*handle, contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, cipherBuffer,
						ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		if (gcry_err != 0)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption failed because: %s",
					    gcry_strerror (gcry_err));
			elektraFree (output);
			return (-1);
		}
		memcpy ((output + i - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE), contentBuffer, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
		written += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	}

	if (written < contentLen)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Content was shorter than described in the header");
		elektraFree (output);
		return (-1);
	}

	// write back the cipher text to the key
	if ((flags & ELEKTRA_CRYPTO_FLAG_STRING) == ELEKTRA_CRYPTO_FLAG_STRING)
	{
		keySetString (k, (const char *)output);
	}
	else if ((flags & ELEKTRA_CRYPTO_FLAG_NULL) == ELEKTRA_CRYPTO_FLAG_NULL || contentLen == 0)
	{
		keySetBinary (k, NULL, 0);
	}
	else
	{
		keySetBinary (k, output, contentLen);
	}

	elektraFree (output);
	return 1;
}

char * elektraCryptoGcryCreateRandomString (const kdb_unsigned_short_t length)
{
	kdb_octet_t * buffer = elektraMalloc (length);
	if (!buffer)
	{
		return 0;
	}

	gcry_create_nonce (buffer, length - 1);
	elektraCryptoNormalizeRandomString (buffer, length);
	return (char *)buffer;
}
