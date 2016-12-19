/**
 * @file
 *
 * @brief helper functions for the crypto plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "helper.h"
#include "crypto.h"
#include "gpg.h"
#include <base64_functions.h>
#include <kdberrors.h>
#include <stdlib.h>

/**
 * @brief parse the hex-encoded salt from the metakey.
 * @param errorKey holds an error description in case of failure.
 * @param k holds the salt as metakey
 * @param salt is set to an allocated buffer containing the salt. Must be freed by the caller.
 * @param saltLen is set to the length of the salt. Ignored if NULL is provided.
 * @retval 1 on success
 * @retval -1 on error. errorKey holds a description.
*/
int CRYPTO_PLUGIN_FUNCTION (getSaltFromMetakey) (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen)
{
	size_t saltLenInternal = 0;
	const Key * meta = keyGetMeta (k, ELEKTRA_CRYPTO_META_SALT);
	if (!meta)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "missing salt as metakey %s in key %s",
				    ELEKTRA_CRYPTO_META_SALT, keyName (k));
		return -1;
	}

	int result = ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (keyString (meta), salt, &saltLenInternal);
	if (result == -1)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "Salt was not stored Base64 encoded.");
		return -1;
	}
	else if (result == -2)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		return -1;
	}
	*saltLen = saltLenInternal;
	return 1;
}

/**
 * @brief parse the salt from the crypto payload in the given (Elektra) Key.
 * @param errorKey holds an error description in case of failure.
 * @param k holds the crypto paylaod.
 * @param salt is set to the location of the salt within the crypto payload. Ignored if NULL is provided.
 * @param saltLen is set to the length of the salt. Ignored if NULL is provided.
 * @retval 1 on success
 * @retval -1 on error. errorKey holds a description.
*/
int CRYPTO_PLUGIN_FUNCTION (getSaltFromPayload) (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen)
{
	static const size_t headerLen = sizeof (kdb_unsigned_long_t);
	const ssize_t payloadLen = keyGetValueSize (k);
	const kdb_octet_t * payload = (kdb_octet_t *)keyValue (k);
	kdb_unsigned_long_t restoredSaltLen = 0;

	// validate payload length
	if ((size_t)payloadLen < sizeof (size_t) || payloadLen < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "payload is too small to contain a salt (payload length is: %lu)", payloadLen);
		if (salt) *salt = NULL;
		return -1;
	}

	// restore salt length
	memcpy (&restoredSaltLen, payload, headerLen);
	if (saltLen) *saltLen = restoredSaltLen;

	// validate restored salt length
	if (restoredSaltLen < 1 || restoredSaltLen > (payloadLen - headerLen))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "restored salt has invalid length of %u (payload length is: %lu)", restoredSaltLen, payloadLen);
		if (salt) *salt = NULL;
		return -1;
	}

	// restore salt
	if (salt) *salt = ((kdb_octet_t *)(payload)) + headerLen;

	return 1;
}

/**
* @brief read the encrypted password form the configuration and decrypt it.
* @param errorKey holds an error description in case of failure.
* @param config holds the plugin configuration.
* @returns the decrypted master password as (Elektra) Key or NULL in case of error. Must be freed by the caller.
*/
Key * CRYPTO_PLUGIN_FUNCTION (getMasterPassword) (Key * errorKey, KeySet * config)
{
	Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD, 0);
	if (!master)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in plugin configuration",
				    ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD);
		return NULL;
	}
	Key * msg = keyDup (master);
	if (CRYPTO_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (config, errorKey, msg) != 1)
	{
		keyDel (msg);
		return NULL; // error set by CRYPTO_PLUGIN_FUNCTION(gpgDecryptMasterPassword)()
	}
	return msg;
}

/**
 * @brief read the desired iteration count from config
 * @param errorKey may hold a warning if an invalid configuration is provided
 * @param config KeySet holding the plugin configuration
 * @returns the number of iterations for the key derivation function
 */
kdb_unsigned_long_t CRYPTO_PLUGIN_FUNCTION (getIterationCount) (Key * errorKey, KeySet * config)
{
	Key * k = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_ITERATION_COUNT, 0);
	if (k)
	{
		const kdb_unsigned_long_t iterations = strtoul (keyString (k), NULL, 10);
		if (iterations > 0)
		{
			return iterations;
		}
		else
		{
			ELEKTRA_ADD_WARNING (ELEKTRA_WARNING_CRYPTO_CONFIG, errorKey,
					     "iteration count provided at " ELEKTRA_CRYPTO_PARAM_ITERATION_COUNT
					     " is invalid. Using default value instead.");
		}
	}
	return ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT;
}
