/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif
#include "crypto.h"
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
#include "gcrypt_operations.h"
#endif
#ifdef ELEKTRA_CRYPTO_API_OPENSSL
#include "openssl_operations.h"
#endif
#include <pthread.h>

static pthread_mutex_t mutex_ref_cnt = PTHREAD_MUTEX_INITIALIZER;
static unsigned int ref_cnt = 0;

/**
 * @brief initialize the crypto provider for the first instance of the plugin.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey)
{
	pthread_mutex_lock(&mutex_ref_cnt);
	if (ref_cnt == 0)
	{
		if (elektraCryptoInit(errorKey) != 1)
		{
			pthread_mutex_unlock(&mutex_ref_cnt);
			return (-1);
		}
	}
	ref_cnt++;
	pthread_mutex_unlock(&mutex_ref_cnt);
	return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;
}

/**
 * @brief finalizes the crypto provider for the last instance of the plugin.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	pthread_mutex_lock(&mutex_ref_cnt);
	if (--ref_cnt == 0)
	{
		elektraCryptoTeardown();
	}
	pthread_mutex_unlock(&mutex_ref_cnt);
	return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;
}

/**
 * @brief establish the Elektra plugin contract and decrypt values, if possible.
 *
 * The crypto configuration is expected to be contained within the KeySet ks.
 * All keys having a metakey "crypto/encrypted" with a strlen() > 0 are being decrypted.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoGet(Plugin *handle ELEKTRA_UNUSED, KeySet *ks, Key *parentKey)
{
	Key *k;
	elektraCryptoHandle *cryptoHandle;

	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName(parentKey), "system/elektra/modules/crypto"))
	{
		KeySet *moduleConfig = ksNew (30,
#include "contract.h"
			KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;
	}

	// the actual decryption

	// for now we expect the crypto configuration to be stored in the KeySet ks
	// we may add more options in the future

	if (elektraCryptoHandleCreate(&cryptoHandle, ks, parentKey) != ELEKTRA_CRYPTO_FUNCTION_SUCCESS)
	{
		goto error;
	}

	ksRewind (ks);
	while ((k = ksNext (ks)) != 0)
	{
		if (elektraCryptoDecrypt(cryptoHandle, k, parentKey) != ELEKTRA_CRYPTO_FUNCTION_SUCCESS)
		{
			goto error;
		}
	}
	elektraCryptoHandleDestroy(cryptoHandle);
	return 1;

error:
	elektraCryptoHandleDestroy(cryptoHandle);
	return -1;
}

/**
 * @brief Encrypt values marked for encryption.
 *
 * If a key has the metakey "crypto/encrypt" with a strlen() > 0, then the value
 * will be encrypted using the configuration stored in the KeySet ks.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoSet(Plugin *handle ELEKTRA_UNUSED, KeySet *ks, Key *parentKey)
{
	Key *k;
	elektraCryptoHandle *cryptoHandle;

	// for now we expect the crypto configuration to be stored in the KeySet ks
	// we may add more options in the future

	if (elektraCryptoHandleCreate(&cryptoHandle, ks, parentKey) != ELEKTRA_CRYPTO_FUNCTION_SUCCESS)
	{
		goto error;
	}

	ksRewind (ks);
	while ((k = ksNext (ks)) != 0)
	{
		if (elektraCryptoEncrypt(cryptoHandle, k, parentKey) != ELEKTRA_CRYPTO_FUNCTION_SUCCESS)
		{
			goto error;
		}
	}
	elektraCryptoHandleDestroy(cryptoHandle);
	return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;

error:
	elektraCryptoHandleDestroy(cryptoHandle);
	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoError(Plugin *handle ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_CRYPTO_FUNCTION_SUCCESS;
}

/**
 * @brief initialize the crypto backend
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoInit(Key *errorKey)
{
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
	return elektraCryptoGcryInit(errorKey);
#endif

#ifdef ELEKTRA_CRYPTO_API_OPENSSL
	return elektraCryptoOpenSSLInit(errorKey);
#endif

	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

/**
 * @brief clean up the crypto backend
 *
 * Some libraries may need extra code for cleaning up the environment.
 */
void elektraCryptoTeardown()
{
	// nothing to do for libgcrypt, but maybe other libraries need clean-up
}

/**
 * @brief allocate a new crypto handle
 *
 * A pointer to a new crypto handle will be stored at handle. In order to obtain
 * the key and the initialization vector (IV) for the handle, the KeySet config
 * is being used.
 *
 * The function may look for the following keys in config:
 *
 * - /elektra/modules/crypto/key-derivation/key
 * - /elektra/modules/crypto/key-derivation/iv
 *
 * The caller of this function must provide this keys in config!
 *
 * The caller of this function is also responsible for calling elektraCryptoHandleDestroy()
 * on the handle to avoid memory leaks.
 *
 * @param handle the memory location where the address of the new crypto handle will be stored
 * @param config the KeySet holding the key and IV parameters for key derivation
 * @param errorKey the key holding error messages that might occur during the creation
 * @returns 1 on success
 * @returns -1 on failure
 */
int elektraCryptoHandleCreate(elektraCryptoHandle **handle, KeySet *config, Key *errorKey)
{
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
	return elektraCryptoGcryHandleCreate(handle, config, errorKey);
#endif

#ifdef ELEKTRA_CRYPTO_API_OPENSSL
	return elektraCryptoOpenSSLHandleCreate(handle, config, errorKey);
#endif

	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

/**
 * @brief clean up and destroy a crypto handle
 *
 * This function overwrites confidential data (e.g. keys) and releases the handle.
 */
void elektraCryptoHandleDestroy(elektraCryptoHandle *handle)
{
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
	elektraCryptoGcryHandleDestroy(handle);
#endif

#ifdef ELEKTRA_CRYPTO_API_OPENSSL
	elektraCryptoOpenSSLHandleDestroy(handle);
#endif
}

/**
 * @brief encrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
	return elektraCryptoGcryEncrypt(handle, k, errorKey);
#endif

#ifdef ELEKTRA_CRYPTO_API_OPENSSL
	return elektraCryptoOpenSSLEncrypt(handle, k, errorKey);
#endif

	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

/**
 * @brief decrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
#ifdef ELEKTRA_CRYPTO_API_GCRYPT
	return elektraCryptoGcryDecrypt(handle, k, errorKey);
#endif

#ifdef ELEKTRA_CRYPTO_API_OPENSSL
	return elektraCryptoOpenSSLDecrypt(handle, k, errorKey);
#endif

	return ELEKTRA_CRYPTO_FUNCTION_ERROR;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto)
{
	return elektraPluginExport("crypto",
			ELEKTRA_PLUGIN_OPEN,  &elektraCryptoOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraCryptoClose,
			ELEKTRA_PLUGIN_GET,   &elektraCryptoGet,
			ELEKTRA_PLUGIN_SET,   &elektraCryptoSet,
			ELEKTRA_PLUGIN_ERROR, &elektraCryptoError,
			ELEKTRA_PLUGIN_END);
}
