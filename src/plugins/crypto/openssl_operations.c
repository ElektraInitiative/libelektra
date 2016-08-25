/**
 * @file
 *
 * @brief cryptographic interface using the libcrypto library (part of the OpenSSL project)
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "crypto.h"

#include "gpg.h"
#include "helper.h"
#include "openssl_operations.h"

#include <kdberrors.h>
#include <kdbtypes.h>
#include <openssl/buffer.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/rand.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

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
	kdb_octet_t salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN];
	const size_t keyBufferSize = ELEKTRA_CRYPTO_SSL_KEYSIZE + ELEKTRA_CRYPTO_SSL_BLOCKSIZE;
	kdb_octet_t keyBuffer[keyBufferSize];

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
	pthread_mutex_lock (&mutex_ssl);
	if (!PKCS5_PBKDF2_HMAC_SHA1 (keyValue (msg), keyGetValueSize (msg), salt, sizeof (salt), iterations, keyBufferSize, keyBuffer))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "PBKDF2 failed with error code %lu", ERR_get_error ());
		pthread_mutex_unlock (&mutex_ssl);
		goto error;
	}
	pthread_mutex_unlock (&mutex_ssl);

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_SSL_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_SSL_KEYSIZE, ELEKTRA_CRYPTO_SSL_BLOCKSIZE);

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
	const size_t keyBufferSize = ELEKTRA_CRYPTO_SSL_KEYSIZE + ELEKTRA_CRYPTO_SSL_BLOCKSIZE;
	kdb_octet_t keyBuffer[keyBufferSize];

	// get the salt
	const Key * salt = keyGetMeta (k, ELEKTRA_CRYPTO_META_SALT);
	if (!salt)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing salt as meta-key %s for key %s",
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
	pthread_mutex_lock (&mutex_ssl);
	if (!PKCS5_PBKDF2_HMAC_SHA1 (keyValue (msg), keyGetValueSize (msg), keyValue (salt), keyGetValueSize (salt), iterations,
				     keyBufferSize, keyBuffer))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "PBKDF2 failed with error code %lu", ERR_get_error ());
		pthread_mutex_unlock (&mutex_ssl);
		goto error;
	}
	pthread_mutex_unlock (&mutex_ssl);

	keySetBinary (cKey, keyBuffer, ELEKTRA_CRYPTO_SSL_KEYSIZE);
	keySetBinary (cIv, keyBuffer + ELEKTRA_CRYPTO_SSL_KEYSIZE, ELEKTRA_CRYPTO_SSL_BLOCKSIZE);

	keyDel (msg);
	return 1;

error:
	keyDel (msg);
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
		return (-1);
	}

	if (keyGetValueSize (iv) != ELEKTRA_CRYPTO_SSL_BLOCKSIZE)
	{
		keyDel (key);
		keyDel (iv);
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "Failed to create handle! Invalid IV length.");
		return (-1);
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
		return (-1);
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
		return (-1);
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
	// NOTE to prevent memory overflows in libcrypto the buffer holding the encrypted content
	//      is one block bigger than the inupt buffer.
	kdb_octet_t cipherBuffer[2 * ELEKTRA_CRYPTO_SSL_BLOCKSIZE];
	kdb_octet_t contentBuffer[ELEKTRA_CRYPTO_SSL_BLOCKSIZE] = { 0 };
	kdb_octet_t * output;
	int written = 0;
	size_t outputLen;
	BIO * encrypted;
	const kdb_octet_t * value = (kdb_octet_t *)keyValue (k);

	// prepare the crypto header data
	const kdb_unsigned_long_t contentLen = keyGetValueSize (k);
	kdb_octet_t flags;
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
		return (-1);
	}

	// encrypt the header data
	memcpy (contentBuffer, &flags, sizeof (flags));
	memcpy (contentBuffer + sizeof (flags), &contentLen, sizeof (contentLen));
	EVP_EncryptUpdate (&(handle->encrypt), cipherBuffer, &written, contentBuffer, headerLen);
	if (written > 0)
	{
		BIO_write (encrypted, cipherBuffer, written);
	}

	if (ERR_peek_error ())
	{
		goto error;
	}

	// encrypt content block by block (i = start of the current block)
	for (kdb_unsigned_long_t i = 0; i < contentLen; i += ELEKTRA_CRYPTO_SSL_BLOCKSIZE)
	{
		// load content partition into the content buffer
		kdb_unsigned_long_t partitionLen = ELEKTRA_CRYPTO_SSL_BLOCKSIZE;
		if ((i + 1) * ELEKTRA_CRYPTO_SSL_BLOCKSIZE > contentLen)
		{
			partitionLen = contentLen - (i * ELEKTRA_CRYPTO_SSL_BLOCKSIZE);
		}
		memcpy (contentBuffer, (value + i), partitionLen);

		EVP_EncryptUpdate (&(handle->encrypt), cipherBuffer, &written, contentBuffer, partitionLen);
		if (written > 0)
		{
			BIO_write (encrypted, cipherBuffer, written);
		}

		if (ERR_peek_error ())
		{
			goto error;
		}
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
	outputLen = BIO_get_mem_data (encrypted, &output);
	if (outputLen > 0)
	{
		keySetBinary (k, output, outputLen);
	}
	BIO_free_all (encrypted);
	pthread_mutex_unlock (&mutex_ssl);
	return 1;

error:
	ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption error! libcrypto error code was: %lu",
			    ERR_get_error ());
	BIO_free_all (encrypted);
	pthread_mutex_unlock (&mutex_ssl);
	return (-1);
}

int elektraCryptoOpenSSLDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey)
{
	const kdb_octet_t * value = (kdb_octet_t *)keyValue (k);
	const size_t valueLen = keyGetValueSize (k);

	kdb_octet_t cipherBuffer[ELEKTRA_CRYPTO_SSL_BLOCKSIZE];
	// NOTE to prevent memory overflows in libcrypto the buffer holding the decrypted content
	//      is one block bigger than the inupt buffer.
	kdb_octet_t contentBuffer[2 * ELEKTRA_CRYPTO_SSL_BLOCKSIZE];
	int written = 0;
	kdb_octet_t * plaintext;
	size_t plaintextLen;

	// initialize crypto header data
	kdb_unsigned_long_t contentLen = 0;
	kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;
	const size_t headerLen = sizeof (flags) + sizeof (contentLen);

	// plausibility check
	if (valueLen % ELEKTRA_CRYPTO_SSL_BLOCKSIZE != 0)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "value length is not a multiple of the block size");
		return (-1);
	}

	pthread_mutex_lock (&mutex_ssl);

	// prepare sink for plain text output
	BIO * decrypted = BIO_new (BIO_s_mem ());
	if (!decrypted)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		pthread_mutex_unlock (&mutex_ssl);
		return (-1);
	}

	// decrypt the whole BLOB and store the plain text into the memory sink
	for (kdb_unsigned_long_t i = 0; i < valueLen; i += ELEKTRA_CRYPTO_SSL_BLOCKSIZE)
	{
		memcpy (cipherBuffer, (value + i), ELEKTRA_CRYPTO_SSL_BLOCKSIZE);
		EVP_DecryptUpdate (&(handle->decrypt), contentBuffer, &written, cipherBuffer, ELEKTRA_CRYPTO_SSL_BLOCKSIZE);
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

	if ((plaintextLen - headerLen) != contentLen)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption error! corrupted data.");
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
	return (-1);
}

char * elektraCryptoOpenSSLCreateRandomString (const kdb_unsigned_short_t length)
{
	kdb_octet_t * buffer = elektraMalloc (length);
	if (!buffer)
	{
		return 0;
	}
	pthread_mutex_lock (&mutex_ssl);
	if (!RAND_bytes (buffer, length - 1))
	{
		pthread_mutex_unlock (&mutex_ssl);
		elektraFree (buffer);
		return 0;
	}
	pthread_mutex_unlock (&mutex_ssl);
	elektraCryptoNormalizeRandomString (buffer, length);
	return (char *)buffer;
}
