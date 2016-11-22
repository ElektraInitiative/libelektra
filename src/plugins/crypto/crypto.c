/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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
#ifdef ELEKTRA_CRYPTO_API_BOTAN
#include "botan_operations.h"
#endif
#include "gpg.h"
#include <kdb.h>
#include <kdberrors.h>
#include <kdbtypes.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

static pthread_mutex_t mutex_ref_cnt = PTHREAD_MUTEX_INITIALIZER;
static unsigned int ref_cnt = 0;

// gurads against compiler warnings because the functions are only used within the specified compile variants
#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL) || defined(ELEKTRA_CRYPTO_API_BOTAN)

/**
 * @brief checks if a Key has been marked for encryption by checking the Key's metadata.
 *
 * If the metakey ELEKTRA_CRYPTO_META_ENCRYPT has the value "1" it is considered to be true.
 * Every other value or the non-existence of the metakey is considered to be false.
 *
 * @param k the Key to be checked
 * @retval 0 if the Key has not been marked for encryption
 * @retval 1 if the Key has been marked for encryption
 */
static int isMarkedForEncryption (const Key * k)
{
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypt && strcmp (keyString (metaEncrypt), "1") == 0)
	{
		return 1;
	}
	return 0;
}

/**
 * @brief checks if a given Key k is in the spec namespace.
 * @retval 0 if the Key k is in the spec namespace.
 * @retval 1 if the Key k is NOT in the spec namespace.
 */
static inline int isSpecNamespace (const Key * k)
{
	return (keyGetNamespace (k) == KEY_NS_SPEC);
}

#endif

/**
 * @brief initialize the crypto backend
 * @retval 1 on success
 * @retval -1 on failure
 */
static int elektraCryptoInit (Key * errorKey ELEKTRA_UNUSED)
{
#if defined(ELEKTRA_CRYPTO_API_GCRYPT)
	return elektraCryptoGcryInit (errorKey);
#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)
	return elektraCryptoOpenSSLInit (errorKey);
#elif defined(ELEKTRA_CRYPTO_API_BOTAN)
	return elektraCryptoBotanInit (errorKey);
#else
	return 1;
#endif
}

/**
 * @brief clean up the crypto backend
 *
 * Some libraries may need extra code for cleaning up the environment.
 */
static void elektraCryptoTeardown ()
{
}

/**
 * @brief read the plugin configuration for the supposed length of the master password.
 * @param errorKey may hold a warning if the provided configuration is invalid
 * @param conf the plugin configuration
 * @return the expected length of the master password
 */
static kdb_unsigned_short_t elektraCryptoGetRandomPasswordLength (Key * errorKey, KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD_LEN, 0);
	if (k && keyIsString (k) > 0)
	{
		kdb_unsigned_short_t passwordLen = (kdb_unsigned_short_t)strtoul (keyString (k), NULL, 10);
		if (passwordLen > 0)
		{
			return passwordLen;
		}
		else
		{
			ELEKTRA_ADD_WARNING (ELEKTRA_WARNING_CRYPTO_CONFIG, errorKey,
					     "Master password length provided at " ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD_LEN
					     " is invalid. Using default value instead.");
		}
	}
	return ELEKTRA_CRYPTO_DEFAULT_MASTER_PWD_LENGTH;
}

/**
 * @brief create a random master password using the crypto backend's SRNG.
 * @param errorKey holds an error description in case of failure.
 * @param buffer is used to store the allocated hex-encoded random string. Must be freed by the caller.
 * @param length limit the length of the generated string to length characters (including the 0x00 terminator)
 * @retval 1 on success
 * @retval -1 on error. errorKey holds a description.
 */
static int elektraCryptoCreateRandomString (Key * errorKey ELEKTRA_UNUSED, char ** buffer ELEKTRA_UNUSED,
					    const kdb_unsigned_short_t length ELEKTRA_UNUSED)
{
	*buffer = NULL;

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)
	*buffer = elektraCryptoGcryCreateRandomString (errorKey, length);
#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)
	*buffer = elektraCryptoOpenSSLCreateRandomString (errorKey, length);
#elif defined(ELEKTRA_CRYPTO_API_BOTAN)
	*buffer = elektraCryptoBotanCreateRandomString (errorKey, length);
#endif

	if (*buffer) return 1;
	return -1;
}

/**
 * @brief encrypt the (Elektra) Keys contained in data.
 * @param handle for the current plugin instance
 * @param data the KeySet holding the data
 * @param errorKey holds an error description in case of failure
 * @retval 1 on success
 * @retval -1 on failure. errorKey holds an error description.
 */
static int elektraCryptoEncrypt (Plugin * handle ELEKTRA_UNUSED, KeySet * data ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	Key * k;

#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL) || defined(ELEKTRA_CRYPTO_API_BOTAN)
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
#endif

#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL)
	elektraCryptoHandle * cryptoHandle = NULL;
#endif

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (!isMarkedForEncryption (k) || isSpecNamespace (k))
		{
			continue;
		}

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)

		if (elektraCryptoGcryHandleCreate (&cryptoHandle, pluginConfig, errorKey, k, ELEKTRA_CRYPTO_ENCRYPT) != 1)
		{
			return -1;
		}

		if (elektraCryptoGcryEncrypt (cryptoHandle, k, errorKey) != 1)
		{
			elektraCryptoGcryHandleDestroy (cryptoHandle);
			return -1;
		}

		elektraCryptoGcryHandleDestroy (cryptoHandle);
		cryptoHandle = NULL;

#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)

		if (elektraCryptoOpenSSLHandleCreate (&cryptoHandle, pluginConfig, errorKey, k, ELEKTRA_CRYPTO_ENCRYPT) != 1)
		{
			elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
			return -1;
		}

		if (elektraCryptoOpenSSLEncrypt (cryptoHandle, k, errorKey) != 1)
		{
			elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
			return -1;
		}

		elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
		cryptoHandle = NULL;

#elif defined(ELEKTRA_CRYPTO_API_BOTAN)

		if (elektraCryptoBotanEncrypt (pluginConfig, k, errorKey) != 1)
		{
			return -1; // failure, error has been set by elektraCryptoBotanEncrypt
		}

#endif
	}
	return 1;
}

/**
 * @brief decrypt the (Elektra) Keys contained in data.
 * @param handle for the current plugin instance
 * @param data the KeySet holding the data
 * @param errorKey holds an error description in case of failure
 * @retval 1 on success
 * @retval -1 on failure. errorKey holds an error description.
 */
static int elektraCryptoDecrypt (Plugin * handle ELEKTRA_UNUSED, KeySet * data ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	Key * k;

#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL) || defined(ELEKTRA_CRYPTO_API_BOTAN)
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
#endif

#if defined(ELEKTRA_CRYPTO_API_GCRYPT) || defined(ELEKTRA_CRYPTO_API_OPENSSL)
	elektraCryptoHandle * cryptoHandle = NULL;
#endif

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (!isMarkedForEncryption (k) || isSpecNamespace (k))
		{
			continue;
		}

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)

		if (elektraCryptoGcryHandleCreate (&cryptoHandle, pluginConfig, errorKey, k, ELEKTRA_CRYPTO_DECRYPT) != 1)
		{
			return -1;
		}

		if (elektraCryptoGcryDecrypt (cryptoHandle, k, errorKey) != 1)
		{
			elektraCryptoGcryHandleDestroy (cryptoHandle);
			return -1;
		}

		elektraCryptoGcryHandleDestroy (cryptoHandle);
		cryptoHandle = NULL;

#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)

		if (elektraCryptoOpenSSLHandleCreate (&cryptoHandle, pluginConfig, errorKey, k, ELEKTRA_CRYPTO_DECRYPT) != 1)
		{
			elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
			return -1;
		}

		if (elektraCryptoOpenSSLDecrypt (cryptoHandle, k, errorKey) != 1)
		{
			elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
			return -1;
		}

		elektraCryptoOpenSSLHandleDestroy (cryptoHandle);
		cryptoHandle = NULL;

#elif defined(ELEKTRA_CRYPTO_API_BOTAN)

		if (elektraCryptoBotanDecrypt (pluginConfig, k, errorKey) != 1)
		{
			return -1; // failure, error has been set by elektraCryptoBotanDecrypt
		}

#endif
	}
	return 1;
}

/**
 * @brief initialize the crypto provider for the first instance of the plugin.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int CRYPTO_PLUGIN_FUNCTION (open) (Plugin * handle ELEKTRA_UNUSED, Key * errorKey)
{
	pthread_mutex_lock (&mutex_ref_cnt);
	if (ref_cnt == 0)
	{
		if (elektraCryptoInit (errorKey) != 1)
		{
			pthread_mutex_unlock (&mutex_ref_cnt);
			return -1;
		}
	}
	ref_cnt++;
	pthread_mutex_unlock (&mutex_ref_cnt);
	return 1;
}

/**
 * @brief finalizes the crypto provider for the last instance of the plugin.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int CRYPTO_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	/* default behaviour: no teardown except the user/system requests it */
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	if (!pluginConfig)
	{
		return -1; // failure because of missing plugin config
	}

	Key * shutdown = ksLookupByName (pluginConfig, ELEKTRA_CRYPTO_PARAM_SHUTDOWN, 0);
	if (!shutdown)
	{
		return 1; // applying default behaviour -> success
	}
	else
	{
		if (strcmp (keyString (shutdown), "1") != 0)
		{
			return 1; // applying default behaviour -> success
		}
	}

	pthread_mutex_lock (&mutex_ref_cnt);
	if (--ref_cnt == 0)
	{
		elektraCryptoTeardown ();
	}
	pthread_mutex_unlock (&mutex_ref_cnt);
	return 1; // success
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
int CRYPTO_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey)
{
	// Publish module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	// the actual decryption

	// for now we expect the crypto configuration to be stored in the KeySet ks
	// we may add more options in the future

	return elektraCryptoDecrypt (handle, ks, parentKey);
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
int CRYPTO_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey)
{
	// for now we expect the crypto configuration to be stored in the KeySet ks
	// we may add more options in the future

	return elektraCryptoEncrypt (handle, ks, parentKey);
}

/**
 * @brief Checks for the existense of the master password, that is used for encryption and decryption.
 *
 * If the master password can not be found it will be generated randomly.
 * Then it will be encrypted and stored in conf.
 *
 * If the master password can be found, it will be decrypted temporarily in order to verify its correctness.
 * conf will not be modified in this case.
 *
 * An error might occur during the password generation, encryption and decryption.
 * The error will be appended to errorKey.
 *
 * @retval 0 no changes were made to the configuration
 * @retval 1 the master password has been appended to the configuration
 * @retval -1 an error occured. Check errorKey
 */
int CRYPTO_PLUGIN_FUNCTION (checkconf) (Key * errorKey, KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD, 0);
	if (k)
	{
		// call gpg module to verify that we own the required key
		Key * msg = keyDup (k);
		if (CRYPTO_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (conf, errorKey, msg) != 1)
		{
			keyDel (msg);
			return -1; // error set by CRYPTO_PLUGIN_FUNCTION(gpgDecryptMasterPassword)()
		}
		keyDel (msg);
		return 0;
	}
	else
	{
		// generate random master password
		const kdb_unsigned_short_t passwordLen = elektraCryptoGetRandomPasswordLength (errorKey, conf);
		char * r = NULL;
		if (elektraCryptoCreateRandomString (errorKey, &r, passwordLen) != 1)
		{
			return -1; // error set by elektraCryptoCreateRandomString()
		}

		// store password in configuration
		k = keyNew ("user/" ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD, KEY_END);
		keySetString (k, r);
		elektraFree (r);
		if (CRYPTO_PLUGIN_FUNCTION (gpgEncryptMasterPassword) (conf, errorKey, k) != 1)
		{
			keyDel (k);
			return -1; // error set by CRYPTO_PLUGIN_FUNCTION(gpgEncryptMasterPassword)()
		}
		ksAppendKey (conf, k);
		return 1;
	}
}

/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int CRYPTO_PLUGIN_FUNCTION (error) (Plugin * handle ELEKTRA_UNUSED, KeySet * ks ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return 1;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (crypto)
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_OPEN,  &CRYPTO_PLUGIN_FUNCTION(open),
			ELEKTRA_PLUGIN_CLOSE, &CRYPTO_PLUGIN_FUNCTION(close),
			ELEKTRA_PLUGIN_GET,   &CRYPTO_PLUGIN_FUNCTION(get),
			ELEKTRA_PLUGIN_SET,   &CRYPTO_PLUGIN_FUNCTION(set),
			ELEKTRA_PLUGIN_ERROR, &CRYPTO_PLUGIN_FUNCTION(error),
			ELEKTRA_PLUGIN_END);
}
