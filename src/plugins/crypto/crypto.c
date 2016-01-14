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
#include <kdberrors.h>
#include <pthread.h>
#include <string.h>

static pthread_mutex_t mutex_ref_cnt = PTHREAD_MUTEX_INITIALIZER;
static unsigned int ref_cnt = 0;


/**
 * @brief initialize the crypto backend
 * @retval 1 on success
 * @retval -1 on failure
 */
static int elektraCryptoInit(Key *errorKey)
{
#if defined(ELEKTRA_CRYPTO_API_GCRYPT)
	return elektraCryptoGcryInit(errorKey);
#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)
	return elektraCryptoOpenSSLInit(errorKey);
#else
	return 1;
#endif
}

/**
 * @brief clean up the crypto backend
 *
 * Some libraries may need extra code for cleaning up the environment.
 */
static void elektraCryptoTeardown()
{
#ifdef ELEKTRA_CRYPTO_API_OPENSSL
	elektraCryptoOpenSSLTeardown();
#endif
}

/**
 * @brief encrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
static int elektraCryptoEncrypt(Plugin *handle, KeySet *data, Key *errorKey)
{
#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL)
	Key *k;
	elektraCryptoHandle *cryptoHandle = NULL;
	KeySet *pluginConfig = elektraPluginGetConfig(handle);
#endif

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)

	if (elektraCryptoGcryHandleCreate (&cryptoHandle, pluginConfig, errorKey) != 1)
	{
		return -1;
	}

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (elektraCryptoGcryEncrypt (cryptoHandle, k, errorKey)  != 1)
		{
			elektraCryptoGcryHandleDestroy (cryptoHandle);
			return -1;
		}
	}

	elektraCryptoGcryHandleDestroy (cryptoHandle);
	return 1;

#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (elektraCryptoOpenSSLHandleCreate (&cryptoHandle, pluginConfig, errorKey) != 1)
		{
			goto openssl_error;
		}

		if (elektraCryptoOpenSSLEncrypt (cryptoHandle, k, errorKey) != 1)
		{
			goto openssl_error;
		}

		elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
		cryptoHandle = NULL;
	}
	return 1;

openssl_error:
	elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
	return -1;

#else
	return 1;
#endif
}

/**
 * @brief decrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
static int elektraCryptoDecrypt(Plugin *handle, KeySet *data, Key *errorKey)
{
#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL)
	Key *k;
	elektraCryptoHandle *cryptoHandle = NULL;
	KeySet *pluginConfig = elektraPluginGetConfig(handle);
#endif

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)

	if (elektraCryptoGcryHandleCreate (&cryptoHandle, pluginConfig, errorKey) != 1)
	{
		return -1;
	}

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (elektraCryptoGcryDecrypt (cryptoHandle, k, errorKey)  != 1)
		{
			elektraCryptoGcryHandleDestroy (cryptoHandle);
			return -1;
		}
	}

	elektraCryptoGcryHandleDestroy (cryptoHandle);
	return 1;

#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (elektraCryptoOpenSSLHandleCreate (&cryptoHandle, pluginConfig, errorKey) != 1)
		{
			goto openssl_error;
		}

		if (elektraCryptoOpenSSLDecrypt (cryptoHandle, k, errorKey) != 1)
		{
			goto openssl_error;
		}

		elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
		cryptoHandle = NULL;
	}
	return 1;

openssl_error:
	elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
	return -1;

#else
	return 1;
#endif
}

/**
 * @brief initialize the crypto provider for the first instance of the plugin.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int CRYPTO_PLUGIN_FUNCTION(open)(Plugin *handle ELEKTRA_UNUSED, Key *errorKey)
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
	return 1;
}

/**
 * @brief finalizes the crypto provider for the last instance of the plugin.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int CRYPTO_PLUGIN_FUNCTION(close)(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	pthread_mutex_lock(&mutex_ref_cnt);
	if (--ref_cnt == 0)
	{
		elektraCryptoTeardown();
	}
	pthread_mutex_unlock(&mutex_ref_cnt);
	return 1;
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
int CRYPTO_PLUGIN_FUNCTION(get)(Plugin *handle, KeySet *ks, Key *parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName(parentKey), "system/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet *moduleConfig = ksNew (30,
#include "contract.h"
			KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	// the actual decryption

	// for now we expect the crypto configuration to be stored in the KeySet ks
	// we may add more options in the future

	return elektraCryptoDecrypt(handle, ks, parentKey);
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
int CRYPTO_PLUGIN_FUNCTION(set)(Plugin *handle, KeySet *ks, Key *parentKey)
{
	// for now we expect the crypto configuration to be stored in the KeySet ks
	// we may add more options in the future

	return elektraCryptoEncrypt(handle, ks, parentKey);
}

/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int CRYPTO_PLUGIN_FUNCTION(error)(Plugin *handle ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto)
{
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_OPEN,  &CRYPTO_PLUGIN_FUNCTION(open),
			ELEKTRA_PLUGIN_CLOSE, &CRYPTO_PLUGIN_FUNCTION(close),
			ELEKTRA_PLUGIN_GET,   &CRYPTO_PLUGIN_FUNCTION(get),
			ELEKTRA_PLUGIN_SET,   &CRYPTO_PLUGIN_FUNCTION(set),
			ELEKTRA_PLUGIN_ERROR, &CRYPTO_PLUGIN_FUNCTION(error),
			ELEKTRA_PLUGIN_END);
}
