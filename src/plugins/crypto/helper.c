/**
 * @file
 *
 * @brief helper functions for the crypto plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./helper.h"
#include "./crypto.h"
#include "./gpg.h"
#include <elektra/kdb/errors.h>
#include <elektra/plugin/invoke.h>
#include <internal/utility/assert.h>
#include <stdlib.h>

/**
 * @brief lookup if the test mode for unit testing is enabled.
 * @param conf KeySet holding the plugin configuration.
 * @retval 0 test mode is not enabled
 * @retval 1 test mode is enabled
 */
static int inTestMode (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST, 0);
	if (k && !strcmp (keyString (k), "1"))
	{
		return 1;
	}
	return 0;
}

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
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Missing salt as metakey %s in key %s", ELEKTRA_CRYPTO_META_SALT,
							keyName (k));
		return -1;
	}

	int result = ELEKTRA_PLUGIN_FUNCTION (base64Decode) (errorKey, keyString (meta), salt, &saltLenInternal);
	if (result == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Salt was not stored Base64 encoded in key %s", keyName (k));
		return -1;
	}
	else if (result == -2)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
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
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Payload is too small to contain a salt (payload length is: %zu)",
							payloadLen);
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
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Restored salt has invalid length of %u (payload length is: %zu)",
							restoredSaltLen, payloadLen);
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
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Missing %s in plugin configuration", ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD);
		return NULL;
	}
	Key * msg = keyDup (master, KEY_CP_ALL);
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
			ELEKTRA_ADD_INSTALLATION_WARNING (errorKey, "Iteration count provided at " ELEKTRA_CRYPTO_PARAM_ITERATION_COUNT
								    " is invalid. Using default value instead.");
		}
	}
	return ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT;
}

/**
 * @brief call the gpg binary to encrypt the random master password.
 *
 * @param conf holds the backend/plugin configuration
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the master password to be encrypted
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (gpgEncryptMasterPassword) (KeySet * conf, Key * errorKey, Key * msgKey)
{
	// [0]: <path to binary>, [argc-3]: --batch, [argc-2]: -e, [argc-1]: NULL-terminator
	static const kdb_unsigned_short_t staticArgumentsCount = 4;
	Key * k;

	// determine the number of total GPG keys to be used
	kdb_unsigned_short_t recipientCount = 0;
	kdb_unsigned_short_t testMode = 0;
	Key * root = ksLookupByName (conf, ELEKTRA_RECIPIENT_KEY, 0);

	// check root key crypto/key
	if (root && strlen (keyString (root)) > 0)
	{
		recipientCount++;
	}

	// check for key beneath crypto/key (like crypto/key/#0 etc)
	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		k = ksAtCursor (conf, it);
		if (keyIsBelow (k, root) && strlen (keyString (k)) > 0)
		{
			recipientCount++;
		}
	}

	if (recipientCount == 0)
	{
		char * errorDescription = ELEKTRA_PLUGIN_FUNCTION (getMissingGpgKeyErrorText) (conf);
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, errorDescription);
		elektraFree (errorDescription);
		return -1;
	}

	if (inTestMode (conf))
	{
		// add two parameters for unit testing
		testMode = 2;
	}

	// initialize argument vector for gpg call
	const kdb_unsigned_short_t argc = (2 * recipientCount) + staticArgumentsCount + testMode;
	kdb_unsigned_short_t i = 1;
	char * argv[argc];

	// append root (crypto/key) as gpg recipient
	if (root && strlen (keyString (root)) > 0)
	{
		argv[i++] = "-r";
		// NOTE argv[] values will not be modified, so const can be discarded safely
		argv[i++] = (char *) keyString (root);
	}

	// append keys beneath root (crypto/key/#_) as gpg recipients
	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		k = ksAtCursor (conf, it);
		const char * kStringVal = keyString (k);
		if (keyIsBelow (k, root) && strlen (kStringVal) > 0)
		{
			argv[i++] = "-r";
			// NOTE argv[] values will not be modified, so const can be discarded safely
			argv[i++] = (char *) kStringVal;
		}
	}

	// append option for unit tests
	if (testMode)
	{
		argv[i++] = "--trust-model";
		argv[i++] = "always";
	}

	argv[i++] = "--batch";
	argv[i++] = "-e";
	argv[i++] = NULL;

	ELEKTRA_ASSERT (i == argc, "invalid number of arguments generated in method gpgEncryptMasterPassword()");

	// call gpg
	int gpgResult = ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msgKey, argv, argc);
	if (gpgResult != 1) // no success
	{
		return gpgResult; // error set by ELEKTRA_PLUGIN_FUNCTION (gpgCall)
	}

	// encode result as Base64 string
	char * base64Encoded = NULL;
	int base64Result = ELEKTRA_PLUGIN_FUNCTION (base64Encode) (errorKey, keyValue (msgKey), keyGetValueSize (msgKey), &base64Encoded);
	if (base64Encoded)
	{
		keySetString (msgKey, base64Encoded);
		elektraFree (base64Encoded);
	}

	return base64Result;
}

/**
 * @brief call the gpg binary to decrypt the random master password.
 *
 * @param conf holds the backend/plugin configuration
 * @param errorKey holds the error description in case of failure
 * @param msgKey holds the master password to be decrypted. Note that the content of this key will be modified.
 *
 * @retval 1 on success
 * @retval -1 on failure
 */
int ELEKTRA_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (KeySet * conf, Key * errorKey, Key * msgKey)
{
	kdb_octet_t * binaryData = NULL;
	size_t binaryDataLength;

	// decode the master password string
	ELEKTRA_PLUGIN_FUNCTION (base64Decode) (errorKey, keyString (msgKey), &binaryData, &binaryDataLength);
	if (binaryData)
	{
		keySetBinary (msgKey, binaryData, binaryDataLength);
		elektraFree (binaryData);
	}

	// password decryption
	if (inTestMode (conf))
	{
		char * argv[] = { "", "--batch", "--trust-model", "always", "-d", NULL };
		return ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msgKey, argv, 6);
	}
	else
	{
		char * argv[] = { "", "--batch", "-d", NULL };
		return ELEKTRA_PLUGIN_FUNCTION (gpgCall) (conf, errorKey, msgKey, argv, 4);
	}
}
