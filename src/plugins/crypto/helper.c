/**
 * @file
 *
 * @brief helper functions for the crypto plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "helper.h"
#include "crypto.h"
#include "gpg.h"
#include <kdberrors.h>
#include <kdbinvoke.h>
#include <stdlib.h>


/**
 * @brief Encodes arbitrary data using the Base64 schema by utilizing libinvoke.
 * @param errorKey will hold an error description if libinvoke fails.
 * @param input holds the data to be encoded
 * @param inputLength tells how many bytes the input buffer is holding.
 * @param output points to an allocated string holding the Base64 encoded input data or NULL if the string can not be allocated. Must be
 * freed by the caller.
 * @retval 1 on success
 * @retval -1 if libinvoke reported an error (errorKey is being set).
 */
int ELEKTRA_PLUGIN_FUNCTION (base64Encode) (Key * errorKey, const kdb_octet_t * input, const size_t inputLength, char ** output)
{
	ElektraInvokeHandle * handle = elektraInvokeOpen ("base64", 0, errorKey);
	if (!handle)
	{
		return -1;
	}

	typedef char * (*base64EncodeFunction) (const kdb_octet_t * input, const size_t inputLength);
	base64EncodeFunction encodingFunction = *(base64EncodeFunction *) elektraInvokeGetFunction (handle, "base64Encode");

	if (!encodingFunction)
	{
		elektraInvokeClose (handle, 0);
		return -1;
	}

	*output = encodingFunction (input, inputLength);
	elektraInvokeClose (handle, 0);
	return 1;
}

/**
 * @brief decodes Base64 encoded data by utilizing libinvoke.
 * @param input holds the Base64 encoded data string
 * @param output will be set to an allocated buffer holding the decoded data or NULL if the allocation failed. Must be freed by the caller
	  on success.
 * @param outputLength will be set to the amount of decoded bytes.
 * @param errorKey will hold an error description if libinvoke fails.
 * @retval 1 on success
 * @retval -1 if the provided string has not been encoded with Base64
 * @retval -2 if the output buffer allocation failed
 * @retval -3 if libinvoke reported an error (errorKey is being set).
 */
int ELEKTRA_PLUGIN_FUNCTION (base64Decode) (Key * errorKey, const char * input, kdb_octet_t ** output, size_t * outputLength)
{
	ElektraInvokeHandle * handle = elektraInvokeOpen ("base64", 0, errorKey);
	if (!handle)
	{
		return -3;
	}

	typedef int (*base64DecodeFunction) (const char * input, kdb_octet_t ** output, size_t * outputLength);
	base64DecodeFunction decodingFunction = *(base64DecodeFunction *) elektraInvokeGetFunction (handle, "base64Decode");

	if (!decodingFunction)
	{
		elektraInvokeClose (handle, 0);
		return -3;
	}

	int result = decodingFunction (input, output, outputLength);
	elektraInvokeClose (handle, 0);
	return result;
}

/**
 * @brief parse the hex-encoded salt from the metakey.
 * @param errorKey holds an error description in case of failure.
 * @param k holds the salt as metakey
 * @param salt is set to an allocated buffer containing the salt. Must be freed by the caller.
 * @param saltLen is set to the length of the salt. Ignored if NULL is provided.
 * @retval 1 on success
 * @retval -1 on error. errorKey holds a description.
 */
int ELEKTRA_PLUGIN_FUNCTION (getSaltFromMetakey) (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen)
{
	size_t saltLenInternal = 0;
	const Key * meta = keyGetMeta (k, ELEKTRA_CRYPTO_META_SALT);
	if (!meta)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "missing salt as metakey %s in key %s",
				    ELEKTRA_CRYPTO_META_SALT, keyName (k));
		return -1;
	}

	int result = ELEKTRA_PLUGIN_FUNCTION (base64Decode) (errorKey, keyString (meta), salt, &saltLenInternal);
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
	else if (result < -2)
	{
		// errorKey has been set by base64Decode (...)
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
int ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen)
{
	static const size_t headerLen = sizeof (kdb_unsigned_long_t);
	const ssize_t payloadLen = keyGetValueSize (k) - ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;

	// validate payload length
	if ((size_t) payloadLen < sizeof (size_t) || payloadLen < 0)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "payload is too small to contain a salt (payload length is: %zu)", payloadLen);
		if (salt) *salt = NULL;
		return -1;
	}

	const kdb_octet_t * value = (kdb_octet_t *) keyValue (k);
	const kdb_octet_t * payload = &value[ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN];
	kdb_unsigned_long_t restoredSaltLen = 0;

	// restore salt length
	memcpy (&restoredSaltLen, payload, headerLen);
	if (saltLen) *saltLen = restoredSaltLen;

	// validate restored salt length
	if (restoredSaltLen < 1 || restoredSaltLen > (payloadLen - headerLen))
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey,
				    "restored salt has invalid length of %u (payload length is: %zu)", restoredSaltLen, payloadLen);
		if (salt) *salt = NULL;
		return -1;
	}

	// restore salt
	if (salt) *salt = ((kdb_octet_t *) (payload)) + headerLen;

	return 1;
}

/**
 * @brief read the encrypted password form the configuration and decrypt it.
 * @param errorKey holds an error description in case of failure.
 * @param config holds the plugin configuration.
 * @returns the decrypted master password as (Elektra) Key or NULL in case of error. Must be freed by the caller.
 */
Key * ELEKTRA_PLUGIN_FUNCTION (getMasterPassword) (Key * errorKey, KeySet * config)
{
	Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD, 0);
	if (!master)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in plugin configuration",
				    ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD);
		return NULL;
	}
	Key * msg = keyDup (master);
	if (ELEKTRA_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (config, errorKey, msg) != 1)
	{
		keyDel (msg);
		return NULL; // error set by ELEKTRA_PLUGIN_FUNCTION(gpgDecryptMasterPassword)()
	}
	return msg;
}

/**
 * @brief read the desired iteration count from config
 * @param errorKey may hold a warning if an invalid configuration is provided
 * @param config KeySet holding the plugin configuration
 * @returns the number of iterations for the key derivation function
 */
kdb_unsigned_long_t ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (Key * errorKey, KeySet * config)
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
