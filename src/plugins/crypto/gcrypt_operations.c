/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./crypto.h"

#include "./gcrypt_operations.h"
#include "./gpg.h"
#include "./helper.h"

#include <elektra/core/errors.h>
#include <elektra/type/types.h>
#include <errno.h>
#include <gcrypt.h>
#include <internal/utility/assert.h>
#include <internal/utility/alloc.h>
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
 * @param masterKey holds the decrypted master password from the plugin configuration
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey (Elektra)-Key holding the cryptographic material
 * @param cIv (Elektra)-Key holding the initialization vector
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForEncryption (KeySet * config, Key * errorKey, Key * masterKey, Key * k, Key * cKey, Key * cIv)
{
	gcry_error_t gcry_err;
	kdb_octet_t salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN];
	kdb_octet_t keyBuffer[KEY_BUFFER_SIZE];
	char * saltHexString = NULL;

	ELEKTRA_ASSERT (masterKey != NULL, "Parameter `masterKey` must not be NULL");

	// generate the salt
	gcry_create_nonce (salt, sizeof (salt));
	const int encodingResult = ELEKTRA_PLUGIN_FUNCTION (base64Encode) (errorKey, salt, sizeof (salt), &saltHexString);
	if (encodingResult < 0)
	{
		// error in libinvoke - errorKey has been set by base64Encode
		return -1;
	}
	if (!saltHexString)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return -1;
	}
	keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, saltHexString);
	elektraFree (saltHexString);

	// read iteration count
	const kdb_unsigned_long_t iterations = ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

	// generate/derive the cryptographic key and the IV
	if ((gcry_err = gcry_kdf_derive (keyValue (masterKey), keyGetValueSize (masterKey), GCRY_KDF_PBKDF2, GCRY_MD_SHA512, salt,
					 sizeof (salt), iterations, KEY_BUFFER_SIZE, keyBuffer)))
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Failed to create a cryptographic key for encryption. Reason: %s",
					     gcry_strerror (gcry_err));
		return -1;
	}

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_GCRY_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_GCRY_KEYSIZE, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	return 1;
}

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param masterKey holds the decrypted master password from the plugin configuration
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey (Elektra)-Key holding the cryptographic material
 * @param cIv (Elektra)-Key holding the initialization vector
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForDecryption (KeySet * config, Key * errorKey, Key * masterKey, Key * k, Key * cKey, Key * cIv)
{
	gcry_error_t gcry_err;
	kdb_octet_t keyBuffer[KEY_BUFFER_SIZE];
	kdb_octet_t * saltBuffer = NULL;
	kdb_unsigned_long_t saltBufferLen = 0;

	ELEKTRA_ASSERT (masterKey != NULL, "Parameter `masterKey` must not be NULL");

	// get the salt
	if (ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, &saltBuffer, &saltBufferLen) != 1)
	{
		return -1; // error set by ELEKTRA_PLUGIN_FUNCTION(getSaltFromPayload)()
	}

	// get the iteration count
	const kdb_unsigned_long_t iterations = ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

	// derive the cryptographic key and the IV
	if ((gcry_err = gcry_kdf_derive (keyValue (masterKey), keyGetValueSize (masterKey), GCRY_KDF_PBKDF2, GCRY_MD_SHA512, saltBuffer,
					 saltBufferLen, iterations, KEY_BUFFER_SIZE, keyBuffer)))
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Failed to restore the cryptographic key for decryption. Reason: %s",
					     gcry_strerror (gcry_err));
		return -1;
	}

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_GCRY_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_GCRY_KEYSIZE, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE);
	return 1;
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
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Libgcrypt version check failed, looking for version: %s", GCRYPT_VERSION);
		return -1;
	}
	gcry_control (GCRYCTL_DISABLE_SECMEM, 0);
	gcry_control (GCRYCTL_INITIALIZATION_FINISHED, 0);
	return 1;
}

int elektraCryptoGcryHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey, Key * masterKey, Key * k,
				   const enum ElektraCryptoOperation op)
{
	gcry_error_t gcry_err;
	unsigned char keyBuffer[64], ivBuffer[64];
	size_t keyLength, ivLength;

	(*handle) = NULL;

	// retrieve/derive the cryptographic material
	Key * key = keyNew ("/", KEY_END);
	Key * iv = keyNew ("/", KEY_END);
	switch (op)
	{
	case ELEKTRA_CRYPTO_ENCRYPT:
		if (getKeyIvForEncryption (config, errorKey, masterKey, k, key, iv) != 1)
		{
			keyDel (key);
			keyDel (iv);
			return -1;
		}
		break;

	case ELEKTRA_CRYPTO_DECRYPT:
		if (getKeyIvForDecryption (config, errorKey, masterKey, k, key, iv) != 1)
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
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
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
	ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Failed to setup libgcrypt. Reason: %s", gcry_strerror (gcry_err));
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

	// prepare the salt for payload output
	kdb_unsigned_long_t saltLen = 0;
	kdb_octet_t * salt = NULL;

	if (ELEKTRA_PLUGIN_FUNCTION (getSaltFromMetakey) (errorKey, k, &salt, &saltLen) != 1)
	{
		return -1; // error set by ELEKTRA_PLUGIN_FUNCTION(getSaltFromMetakey)()
	}

	// remove salt as metakey because it will be encoded into the crypto payload
	keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, NULL);

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
	outputLen += ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;
	outputLen += sizeof (kdb_unsigned_long_t) + saltLen;
	kdb_octet_t * output = elektraCalloc (outputLen);
	if (!output)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		elektraFree (salt);
		return -1;
	}

	kdb_octet_t * current = output;

	// output of the magic number
	memcpy (current, ELEKTRA_CRYPTO_MAGIC_NUMBER, ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN);
	current += ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;

	// encode the salt into the crypto payload
	memcpy (current, &saltLen, sizeof (kdb_unsigned_long_t));
	current += sizeof (kdb_unsigned_long_t);
	memcpy (current, salt, saltLen);
	current += saltLen;

	// encrypt the header (1st block) using gcrypt's in-place encryption
	memcpy (current, &flags, sizeof (flags));
	memcpy (current + sizeof (flags), &contentLen, sizeof (contentLen));
	gcry_err = gcry_cipher_encrypt (*handle, current, ELEKTRA_CRYPTO_GCRY_BLOCKSIZE, NULL, 0);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Encryption failed. Reason: %s", gcry_strerror (gcry_err));
		memset (output, 0, outputLen);
		elektraFree (output);
		elektraFree (salt);
		return -1;
	}
	current += ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;

	// encrypt the value using gcrypt's in-place encryption
	const size_t dataLen =
		outputLen - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE - sizeof (kdb_unsigned_long_t) - saltLen - ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;
	if (contentLen) memcpy (current, content, contentLen);
	gcry_err = gcry_cipher_encrypt (*handle, current, dataLen, NULL, 0);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Encryption failed. Reason: %s", gcry_strerror (gcry_err));
		memset (output, 0, outputLen);
		elektraFree (output);
		elektraFree (salt);
		return -1;
	}

	// write back the cipher text to the key
	keySetBinary (k, output, outputLen);
	memset (output, 0, outputLen);
	elektraFree (output);
	elektraFree (salt);
	return 1;
}

int elektraCryptoGcryDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	gcry_error_t gcry_err;

	// parse salt length from crypto payload
	kdb_unsigned_long_t saltLen = 0;
	if (ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, NULL, &saltLen) != 1)
	{
		return -1; // error set by ELEKTRA_PLUGIN_FUNCTION(getSaltFromPayload)()
	}
	saltLen += sizeof (kdb_unsigned_long_t);

	// set payload pointer
	const kdb_octet_t * payload = ((kdb_octet_t *) keyValue (k)) + saltLen + ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;
	const size_t payloadLen = keyGetValueSize (k) - saltLen - ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;

	// plausibility check
	if (payloadLen % ELEKTRA_CRYPTO_GCRY_BLOCKSIZE != 0)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "Value length is not a multiple of the block size");
		return -1;
	}

	// prepare buffer for plain text output and crypto operations
	kdb_octet_t * output = elektraMalloc (payloadLen);
	if (!output)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return -1;
	}

	// initialize crypto header data
	kdb_unsigned_long_t contentLen = 0;
	kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;

	// in-place decryption
	memcpy (output, payload, payloadLen);
	gcry_err = gcry_cipher_decrypt (*handle, output, payloadLen, NULL, 0);
	if (gcry_err != 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Decryption failed. Reason: %s", gcry_strerror (gcry_err));
		memset (output, 0, payloadLen);
		elektraFree (output);
		return -1;
	}

	// restore the header data
	memcpy (&flags, output, sizeof (flags));
	memcpy (&contentLen, output + sizeof (flags), sizeof (contentLen));

	const kdb_octet_t * data = output + ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;
	const size_t dataLen = payloadLen - ELEKTRA_CRYPTO_GCRY_BLOCKSIZE;

	// validate restored content length
	if (contentLen > dataLen)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (
			errorKey,
			"Restored content length is bigger than the available amount of decrypted data. The header is possibly corrupted");
		memset (output, 0, payloadLen);
		elektraFree (output);
		return -1;
	}

	// restore the key to its original status
	if ((flags & ELEKTRA_CRYPTO_FLAG_STRING) == ELEKTRA_CRYPTO_FLAG_STRING && contentLen > 0)
	{
		keySetString (k, (const char *) data);
	}
	else if ((flags & ELEKTRA_CRYPTO_FLAG_NULL) == ELEKTRA_CRYPTO_FLAG_NULL || contentLen == 0)
	{
		keySetBinary (k, NULL, 0);
	}
	else
	{
		keySetBinary (k, data, contentLen);
	}

	memset (output, 0, payloadLen);
	elektraFree (output);
	return 1;
}

/**
 * @brief create a random sequence of characters with given length.
 * @param errorKey holds an error description in case of failure.
 * @param length the number of random bytes to be generated.
 * @returns allocated buffer holding a hex-encoded random string or NULL in case of error. Must be freed by the caller.
 */
char * elektraCryptoGcryCreateRandomString (Key * errorKey, const kdb_unsigned_short_t length)
{
	char * encoded = NULL;
	kdb_octet_t buffer[length];
	gcry_create_nonce (buffer, length);
	if (ELEKTRA_PLUGIN_FUNCTION (base64Encode) (errorKey, buffer, length, &encoded) < 0)
	{
		return NULL;
	}
	if (!encoded)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
	}
	return encoded;
}
