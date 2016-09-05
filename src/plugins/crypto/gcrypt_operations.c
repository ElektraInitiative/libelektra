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
#include "gpg.h"
#include "helper.h"

#include <errno.h>
#include <gcrypt.h>
#include <kdberrors.h>
#include <kdbtypes.h>
#include <pthread.h>
#include <stdlib.h>

#define KEY_BUFFER_SIZE (ELEKTRA_CRYPTO_GCRY_KEYSIZE + ELEKTRA_CRYPTO_GCRY_BLOCKSIZE)

// initialize the gcrypt threading subsystem
// NOTE: old versions of libgcrypt require the functions defined in this macro!
GCRY_THREAD_OPTION_PTHREAD_IMPL;


/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey (Elektra)-Key holding the cryptographic material
 * @param cIv (Elektra)-Key holding the initialization vector
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForEncryption (KeySet * config, Key * errorKey, Key * k, Key * cKey, Key * cIv)
{
	gcry_error_t gcry_err;
	kdb_octet_t salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN];
	kdb_octet_t keyBuffer[KEY_BUFFER_SIZE];

	// generate the salt
	gcry_create_nonce (salt, sizeof (salt));
	elektraCryptoNormalizeRandomString (salt, sizeof (salt));
	keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, (char *)salt);

	// read iteration count
	const kdb_unsigned_long_t iterations = elektraCryptoGetIterationCount (config);

	// receive master password from the configuration
	Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PWD, 0);
	if (!master)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s", ELEKTRA_CRYPTO_PARAM_MASTER_PWD);
		return -1;
	}
	Key * msg = keyDup (master);
	if (elektraCryptoGpgDecryptMasterPassword (config, errorKey, msg) != 1)
	{
		goto error;
	}

	// generate/derive the cryptographic key and the IV
	if ((gcry_err = gcry_kdf_derive (keyValue (msg), keyGetValueSize (msg), GCRY_KDF_PBKDF2, GCRY_MD_SHA512, salt, sizeof (salt),
					 iterations, KEY_BUFFER_SIZE, keyBuffer)))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "PBKDF2 failed because: %s", gcry_strerror (gcry_err));
		goto error;
	}

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_GCRY_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_GCRY_KEYSIZE, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);

	keyDel (msg);
	return 1;

error:
	keyDel (msg);
	return -1;
}

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey (Elektra)-Key holding the cryptographic material
 * @param cIv (Elektra)-Key holding the initialization vector
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForDecryption (KeySet * config, Key * errorKey, Key * k, Key * cKey, Key * cIv)
{
	gcry_error_t gcry_err;
	kdb_octet_t keyBuffer[KEY_BUFFER_SIZE];

	// get the salt
	const Key * salt = keyGetMeta (k, ELEKTRA_CRYPTO_META_SALT);
	if (!salt)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing salt as metakey %s for key %s",
				    ELEKTRA_CRYPTO_META_SALT, keyName (k));
		return -1;
	}

	// get the iteration count
	const kdb_unsigned_long_t iterations = elektraCryptoGetIterationCount (config);

	// receive master password from the configuration
	Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PWD, 0);
	if (!master)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s", ELEKTRA_CRYPTO_PARAM_MASTER_PWD);
		return -1;
	}
	Key * msg = keyDup (master);
	if (elektraCryptoGpgDecryptMasterPassword (config, errorKey, msg) != 1)
	{
		goto error;
	}

	// derive the cryptographic key and the IV
	if ((gcry_err = gcry_kdf_derive (keyValue (msg), keyGetValueSize (msg), GCRY_KDF_PBKDF2, GCRY_MD_SHA512, keyValue (salt),
					 keyGetValueSize (salt), iterations, KEY_BUFFER_SIZE, keyBuffer)))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "PBKDF2 failed because: %s", gcry_strerror (gcry_err));
		goto error;
	}

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_GCRY_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_GCRY_KEYSIZE, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);

	keyDel (msg);
	return 1;

error:
	keyDel (msg);
	return -1;
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
		return -1;
	}
	gcry_control (GCRYCTL_DISABLE_SECMEM, 0);
	gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);
	return 1;
}

int elektraCryptoGcryHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey, Key * k,
				   const enum ElektraCryptoOperation op)
{
	gcry_error_t gcry_err;
	unsigned char keyBuffer[64], ivBuffer[64];
	size_t keyLength, ivLength;

	(*handle) = NULL;

	// retrieve/derive the cryptographic material
	Key * key = keyNew (0);
	Key * iv = keyNew (0);
	switch (op)
	{
	case ELEKTRA_CRYPTO_ENCRYPT:
		if (getKeyIvForEncryption (config, errorKey, k, key, iv) != 1)
		{
			keyDel (key);
			keyDel (iv);
			return -1;
		}
		break;

	case ELEKTRA_CRYPTO_DECRYPT:
		if (getKeyIvForDecryption (config, errorKey, k, key, iv) != 1)
		{
			keyDel (key);
			keyDel (iv);
			return -1;
		}
		break;

	default: // not supported
		keyDel (key);
		keyDel (iv);
		return -1;
	}

	keyLength = keyGetBinary (key, keyBuffer, sizeof (keyBuffer));
	ivLength = keyGetBinary (iv, ivBuffer, sizeof (ivBuffer));

	// create the handle
	(*handle) = elektraMalloc (sizeof (elektraCryptoHandle));
	if (*handle == NULL)
	{
		memset (keyBuffer, 0, sizeof (keyBuffer));
		memset (ivBuffer, 0, sizeof (ivBuffer));
		keyDel (key);
		keyDel (iv);
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
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
	keyDel (key);
	keyDel (iv);
	return 1;

error:
	memset (keyBuffer, 0, sizeof (keyBuffer));
	memset (ivBuffer, 0, sizeof (ivBuffer));
	ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "Failed to create handle because: %s", gcry_strerror (gcry_err));
	gcry_cipher_close (**handle);
	elektraFree (*handle);
	(*handle) = NULL;
	keyDel (key);
	keyDel (iv);
	return -1;
}

int elektraCryptoGcryEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	size_t outputLen;
	gcry_error_t gcry_err;

	// prepare the crypto header data
	const kdb_octet_t * content = keyValue (k);
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
	kdb_octet_t * output = elektraMalloc (outputLen);
	if (!output)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
	}

	// encrypt the header (1st block) using gcrypt's in-place encryption
	memcpy (output, &flags, sizeof (flags));
	memcpy (output + sizeof (flags), &contentLen, sizeof (contentLen));
	gcry_err = gcry_cipher_encrypt (*handle, output, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, NULL, 0);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption failed because: %s", gcry_strerror (gcry_err));
		memset (output, 0, outputLen);
		elektraFree (output);
		return -1;
	}

	// encrypt the value using gcrypt's in-place encryption
	kdb_octet_t * dataOut = output + ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	const size_t dataLen = outputLen - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	memcpy (dataOut, content, contentLen);
	gcry_err = gcry_cipher_encrypt (*handle, dataOut, dataLen, NULL, 0);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption failed because: %s", gcry_strerror (gcry_err));
		memset (output, 0, outputLen);
		elektraFree (output);
		return -1;
	}

	// write back the cipher text to the key
	keySetBinary (k, output, outputLen);
	memset (output, 0, outputLen);
	elektraFree (output);
	return 1;
}

int elektraCryptoGcryDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	const kdb_octet_t * value = (kdb_octet_t *)keyValue (k);
	const size_t valueLen = keyGetValueSize (k);
	gcry_error_t gcry_err;

	// plausibility check
	if (valueLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE != 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "value length is not a multiple of the block size");
		return -1;
	}

	// prepare buffer for plain text output and crypto operations
	kdb_octet_t * output = elektraMalloc (valueLen);
	if (!output)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
	}

	// initialize crypto header data
	kdb_unsigned_long_t contentLen = 0;
	kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;

	// decrypt the header (1st block)
	memcpy (output, value, valueLen);
	gcry_err = gcry_cipher_decrypt (*handle, output, valueLen, NULL, 0);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption failed because: %s", gcry_strerror (gcry_err));
		memset (output, 0, valueLen);
		elektraFree (output);
		return -1;
	}

	// restore the header data
	memcpy (&flags, output, sizeof (flags));
	memcpy (&contentLen, output + sizeof (flags), sizeof (contentLen));

	const kdb_octet_t * data = output + ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	const size_t dataLen = valueLen - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;

	// validate restored content length
	if (contentLen > dataLen)
	{
		ELEKTRA_SET_ERROR (
			ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey,
			"restored content length is bigger than the available amount of decrypted data. The header is possibly corrupted.");
		memset (output, 0, valueLen);
		elektraFree (output);
		return -1;
	}

	// restore the key to its original status
	if ((flags & ELEKTRA_CRYPTO_FLAG_STRING) == ELEKTRA_CRYPTO_FLAG_STRING && contentLen > 0)
	{
		keySetString (k, (const char *)data);
	}
	else if ((flags & ELEKTRA_CRYPTO_FLAG_NULL) == ELEKTRA_CRYPTO_FLAG_NULL || contentLen == 0)
	{
		keySetBinary (k, NULL, 0);
	}
	else
	{
		keySetBinary (k, data, contentLen);
	}

	memset (output, 0, valueLen);
	elektraFree (output);
	return 1;
}

/**
 * @brief create a random sequence of characters with given length.
 * @param length the number of random bytes to be generated.
 * @returns allocated buffer holding length bytes. Must be freed by the caller.
 */
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
