/**
 * @file
 *
 * @brief cryptographic interface using the libcrypto library (part of the OpenSSL project)
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "crypto.h"

#include "gpg.h"
#include "helper.h"
#include "openssl_operations.h"

#include <base64_functions.h>
#include <kdberrors.h>
#include <kdbtypes.h>
#include <openssl/buffer.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/rand.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

#define KEY_BUFFER_SIZE (ELEKTRA_CRYPTO_SSL_KEYSIZE + ELEKTRA_CRYPTO_SSL_BLOCKSIZE)

/*
 * Protects all calls to OpenSSL (libcrypto.so).
 *
 * This is required because we can not setup the multi-threading capabilities of OpenSSL directly from within Elektra.
 */
static pthread_mutex_t mutex_ssl = PTHREAD_MUTEX_INITIALIZER;

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
	kdb_octet_t salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN] = { 0 };
	kdb_octet_t keyBuffer[KEY_BUFFER_SIZE] = { 0 };
	char * saltHexString = NULL;

	// generate the salt
	pthread_mutex_lock (&mutex_ssl);
	if (!RAND_bytes (salt, ELEKTRA_CRYPTO_DEFAULT_SALT_LEN - 1))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "failed to generate random salt with error code %lu",
				    ERR_get_error ());
		pthread_mutex_unlock (&mutex_ssl);
		return -1;
	}
	pthread_mutex_unlock (&mutex_ssl);
	saltHexString = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (salt, sizeof (salt));
	if (!saltHexString)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
	}
	keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, saltHexString);
	elektraFree (saltHexString);

	// read iteration count
	const kdb_unsigned_long_t iterations = CRYPTO_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

	// receive master password from the configuration
	Key * msg = CRYPTO_PLUGIN_FUNCTION (getMasterPassword) (errorKey, config);
	if (!msg) goto error; // error set by CRYPTO_PLUGIN_FUNCTION(getMasterPassword)()

	// generate/derive the cryptographic key and the IV
	pthread_mutex_lock (&mutex_ssl);
	if (!PKCS5_PBKDF2_HMAC_SHA1 (keyValue (msg), keyGetValueSize (msg), salt, sizeof (salt), iterations, KEY_BUFFER_SIZE, keyBuffer))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "Failed to create a cryptographic key for encryption. Libcrypto returned error code: %lu",
				    ERR_get_error ());
		pthread_mutex_unlock (&mutex_ssl);
		goto error;
	}
	pthread_mutex_unlock (&mutex_ssl);

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_SSL_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_SSL_KEYSIZE, ELEKTRA_CRYPTO_SSL_BLOCKSIZE);

	keyDel (msg);
	return 1;

error:
	if (msg) keyDel (msg);
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
	kdb_octet_t keyBuffer[KEY_BUFFER_SIZE];
	kdb_octet_t * saltBuffer = NULL;
	kdb_unsigned_long_t saltBufferLen = 0;

	// get the salt
	if (CRYPTO_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, &saltBuffer, &saltBufferLen) != 1)
	{
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getSaltFromPayload)()
	}

	// get the iteration count
	const kdb_unsigned_long_t iterations = CRYPTO_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

	// receive master password from the configuration
	Key * msg = CRYPTO_PLUGIN_FUNCTION (getMasterPassword) (errorKey, config);
	if (!msg) goto error; // error set by CRYPTO_PLUGIN_FUNCTION(getMasterPassword)()

	// derive the cryptographic key and the IV
	pthread_mutex_lock (&mutex_ssl);
	if (!PKCS5_PBKDF2_HMAC_SHA1 (keyValue (msg), keyGetValueSize (msg), saltBuffer, saltBufferLen, iterations, KEY_BUFFER_SIZE,
				     keyBuffer))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "Failed to restore the cryptographic key for decryption. Libcrypto returned the error code: %lu",
				    ERR_get_error ());
		pthread_mutex_unlock (&mutex_ssl);
		goto error;
	}
	pthread_mutex_unlock (&mutex_ssl);

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_SSL_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_SSL_KEYSIZE, ELEKTRA_CRYPTO_SSL_BLOCKSIZE);

	keyDel (msg);
	return 1;

error:
	if (msg) keyDel (msg);
	return -1;
}

int elektraCryptoOpenSSLInit (Key * errorKey ELEKTRA_UNUSED)
{
	// initialize OpenSSL according to
	// https://wiki.openssl.org/index.php/Library_Initialization
	pthread_mutex_lock (&mutex_ssl);
	OpenSSL_add_all_algorithms ();
	ERR_load_crypto_strings ();
	pthread_mutex_unlock (&mutex_ssl);
	return 1;
}

int elektraCryptoOpenSSLHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey, Key * k,
				      const enum ElektraCryptoOperation op)
{
	unsigned char keyBuffer[64], ivBuffer[64];

	*handle = NULL;

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

	if (keyGetValueSize (key) != ELEKTRA_CRYPTO_SSL_KEYSIZE)
	{
		keyDel (key);
		keyDel (iv);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "Failed to create handle! Invalid key length.");
		return -1;
	}

	if (keyGetValueSize (iv) != ELEKTRA_CRYPTO_SSL_BLOCKSIZE)
	{
		keyDel (key);
		keyDel (iv);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "Failed to create handle! Invalid IV length.");
		return -1;
	}

	keyGetBinary (key, keyBuffer, sizeof (keyBuffer));
	keyGetBinary (iv, ivBuffer, sizeof (ivBuffer));

	keyDel (key);
	keyDel (iv);

	*handle = elektraMalloc (sizeof (elektraCryptoHandle));
	if (!(*handle))
	{
		memset (keyBuffer, 0, sizeof (keyBuffer));
		memset (ivBuffer, 0, sizeof (ivBuffer));
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
	}

	pthread_mutex_lock (&mutex_ssl);

	EVP_EncryptInit (&((*handle)->encrypt), EVP_aes_256_cbc (), keyBuffer, ivBuffer);
	EVP_DecryptInit (&((*handle)->decrypt), EVP_aes_256_cbc (), keyBuffer, ivBuffer);

	memset (keyBuffer, 0, sizeof (keyBuffer));
	memset (ivBuffer, 0, sizeof (ivBuffer));

	if (ERR_peek_error ())
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "Failed to create handle! libcrypto error code was: %lu",
				    ERR_get_error ());
		elektraFree (*handle);
		*handle = NULL;
		pthread_mutex_unlock (&mutex_ssl);
		return -1;
	}
	pthread_mutex_unlock (&mutex_ssl);
	return 1;
}

void elektraCryptoOpenSSLHandleDestroy (elektraCryptoHandle * handle)
{
	if (handle)
	{
		pthread_mutex_lock (&mutex_ssl);
		EVP_CIPHER_CTX_cleanup (&(handle->encrypt));
		EVP_CIPHER_CTX_cleanup (&(handle->decrypt));
		pthread_mutex_unlock (&mutex_ssl);
		elektraFree (handle);
	}
}

int elektraCryptoOpenSSLEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	// NOTE to prevent memory overflows in libcrypto the buffer holding the encrypted content (cipherBuffer) is 2 cipher blocks long.
	kdb_octet_t cipherBuffer[2 * ELEKTRA_CRYPTO_SSL_BLOCKSIZE];
	kdb_octet_t headerBuffer[ELEKTRA_CRYPTO_SSL_BLOCKSIZE];
	int written = 0;
	BIO * encrypted;

	// prepare the salt for payload output
	kdb_unsigned_long_t saltLen = 0;
	kdb_octet_t * salt = NULL;

	if (CRYPTO_PLUGIN_FUNCTION (getSaltFromMetakey) (errorKey, k, &salt, &saltLen) != 1)
	{
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getSaltFromMetakey)()
	}

	// remove salt as metakey because it will be encoded into the crypto payload
	keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, NULL);

	// prepare the crypto header data
	kdb_octet_t flags;
	const size_t contentLen = keyGetValueSize (k);
	const size_t headerLen = sizeof (flags) + sizeof (contentLen);

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

	pthread_mutex_lock (&mutex_ssl);

	encrypted = BIO_new (BIO_s_mem ());
	if (!encrypted)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		pthread_mutex_unlock (&mutex_ssl);
		elektraFree (salt);
		return -1;
	}

	// encode the salt into the payload
	BIO_write (encrypted, &saltLen, sizeof (saltLen));
	BIO_write (encrypted, salt, saltLen);

	// encrypt the header data
	memcpy (headerBuffer, &flags, sizeof (flags));
	memcpy (headerBuffer + sizeof (flags), &contentLen, sizeof (contentLen));
	EVP_EncryptUpdate (&(handle->encrypt), cipherBuffer, &written, headerBuffer, headerLen);
	if (written > 0)
	{
		BIO_write (encrypted, cipherBuffer, written);
	}

	if (ERR_peek_error ())
	{
		goto error;
	}

	// encrypt content block by block
	kdb_octet_t * content = (kdb_octet_t *)keyValue (k);
	size_t processed = 0;

	while (processed < contentLen)
	{
		kdb_unsigned_long_t partitionLen = ELEKTRA_CRYPTO_SSL_BLOCKSIZE;
		// the last partition may not fill an entire cipher block
		if (processed + ELEKTRA_CRYPTO_SSL_BLOCKSIZE > contentLen)
		{
			// length of last partition = total content length - number of bytes already processed
			partitionLen = contentLen - processed;
		}

		EVP_EncryptUpdate (&(handle->encrypt), cipherBuffer, &written, content, partitionLen);
		if (written > 0)
		{
			BIO_write (encrypted, cipherBuffer, written);
		}

		if (ERR_peek_error ())
		{
			goto error;
		}

		// move content pointer to the next partition
		processed += partitionLen;
		content += partitionLen;
	}

	EVP_EncryptFinal (&(handle->encrypt), cipherBuffer, &written);
	if (written > 0)
	{
		BIO_write (encrypted, cipherBuffer, written);
	}

	if (ERR_peek_error ())
	{
		goto error;
	}

	// write back the cipher text to the key
	kdb_octet_t * output;
	size_t outputLen = BIO_get_mem_data (encrypted, &output);
	if (outputLen > 0)
	{
		keySetBinary (k, output, outputLen);
	}
	BIO_free_all (encrypted);
	pthread_mutex_unlock (&mutex_ssl);
	elektraFree (salt);
	return 1;

error:
	ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption error! libcrypto error code was: %lu",
			    ERR_get_error ());
	BIO_free_all (encrypted);
	pthread_mutex_unlock (&mutex_ssl);
	elektraFree (salt);
	return -1;
}

int elektraCryptoOpenSSLDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	// NOTE to prevent memory overflows in libcrypto the buffer holding the decrypted content (contentBuffer) is two cipher blocks long
	kdb_octet_t contentBuffer[2 * ELEKTRA_CRYPTO_SSL_BLOCKSIZE];
	int written = 0;
	kdb_octet_t * plaintext;
	size_t plaintextLen;

	// parse salt length from crypto payload
	kdb_unsigned_long_t saltLen = 0;
	if (CRYPTO_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, NULL, &saltLen) != 1)
	{
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getSaltFromPayload)()
	}
	saltLen += sizeof (kdb_unsigned_long_t);

	// set payload pointer
	const kdb_octet_t * payload = ((kdb_octet_t *)keyValue (k)) + saltLen;
	const size_t payloadLen = keyGetValueSize (k) - saltLen;

	// initialize crypto header data
	kdb_unsigned_long_t contentLen = 0;
	kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;
	const size_t headerLen = sizeof (flags) + sizeof (contentLen);

	// plausibility check
	if (payloadLen % ELEKTRA_CRYPTO_SSL_BLOCKSIZE != 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "value length is not a multiple of the block size");
		return -1;
	}

	pthread_mutex_lock (&mutex_ssl);

	// prepare sink for plain text output
	BIO * decrypted = BIO_new (BIO_s_mem ());
	if (!decrypted)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		pthread_mutex_unlock (&mutex_ssl);
		return -1;
	}

	// decrypt the whole BLOB and store the plain text into the memory sink
	for (kdb_unsigned_long_t i = 0; i < payloadLen; i += ELEKTRA_CRYPTO_SSL_BLOCKSIZE)
	{
		EVP_DecryptUpdate (&(handle->decrypt), contentBuffer, &written, (payload + i), ELEKTRA_CRYPTO_SSL_BLOCKSIZE);
		if (written > 0)
		{
			BIO_write (decrypted, contentBuffer, written);
		}

		if (ERR_peek_error ())
		{
			goto error;
		}
	}

	EVP_DecryptFinal (&(handle->decrypt), contentBuffer, &written);
	if (written > 0)
	{
		BIO_write (decrypted, contentBuffer, written);
	}

	if (ERR_peek_error ())
	{
		goto error;
	}

	plaintextLen = BIO_get_mem_data (decrypted, &plaintext);
	if (plaintextLen < headerLen)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption error! header data is incomplete.");
		goto error;
	}

	// restore the header
	memcpy (&flags, plaintext, sizeof (flags));
	plaintext += sizeof (flags);
	memcpy (&contentLen, plaintext, sizeof (contentLen));
	plaintext += sizeof (contentLen);

	// validate restored header
	if (contentLen > (plaintextLen - headerLen))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey,
				   "Content length is bigger than amount of decrypted data. Data is possibly corrupted.");
		goto error;
	}

	// write back the cipher text to the key
	if ((flags & ELEKTRA_CRYPTO_FLAG_STRING) == ELEKTRA_CRYPTO_FLAG_STRING)
	{
		keySetString (k, (const char *)plaintext);
	}
	else if ((flags & ELEKTRA_CRYPTO_FLAG_NULL) == ELEKTRA_CRYPTO_FLAG_NULL || contentLen == 0)
	{
		keySetBinary (k, NULL, 0);
	}
	else
	{
		keySetBinary (k, plaintext, contentLen);
	}

	BIO_free_all (decrypted);
	pthread_mutex_unlock (&mutex_ssl);
	return 1;

error:
	ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption error! libcrypto error code was: %lu",
			    ERR_get_error ());
	BIO_free_all (decrypted);
	pthread_mutex_unlock (&mutex_ssl);
	return -1;
}

/**
 * @brief create a random sequence of characters with given length.
 * @param errorKey holds an error description in case of failure.
 * @param length the number of random bytes to be generated.
 * @returns allocated buffer holding a hex-encoded random string or NULL in case of error. Must be freed by the caller.
  */
char * elektraCryptoOpenSSLCreateRandomString (Key * errorKey, const kdb_unsigned_short_t length)
{
	kdb_octet_t buffer[length];
	pthread_mutex_lock (&mutex_ssl);
	if (!RAND_bytes (buffer, length))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "Failed to generate random string. libcrypto error code was: %lu", ERR_get_error ());
		pthread_mutex_unlock (&mutex_ssl);
		return NULL;
	}
	pthread_mutex_unlock (&mutex_ssl);
	char * encoded = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (buffer, length);
	if (!encoded)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
	}
	return encoded;
}
