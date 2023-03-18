/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations using GPGME
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include <internal/kdb/config.h>
#endif
#include "gpgme.h"
#include "keylist.h"
#include <elektra/kdb.h>
#include <elektra/kdb/errors.h>
#include <elektra/type/types.h>
#include <gpgme.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>

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
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_GPGME_META_ENCRYPT);
	if (metaEncrypt && strcmp (keyString (metaEncrypt), "1") == 0)
	{
		return 1;
	}
	return 0;
}

/**
 * @brief checks if a Key contained a binary value before its value has been encrypted by the gpgme plugin.
 *
 * If the metakey ELEKTRA_GPGME_META_BINARY has the value "1" it is considered to be true.
 * Every other value or the non-existence of the metakey is considered to be false.
 *
 * @param k the Key to be checked
 * @retval 0 if the Key contained a string value before encryption
 * @retval 1 if the Key contained a binary value before encryption
 */
static int isOriginallyBinary (const Key * k)
{
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_GPGME_META_BINARY);
	if (metaEncrypt && strcmp (keyString (metaEncrypt), "1") == 0)
	{
		return 1;
	}
	return 0;
}

/**
 * @brief lookup if the test mode for unit testing is enabled.
 * @param conf KeySet holding the plugin configuration.
 * @retval 0 test mode is not enabled
 * @retval 1 test mode is enabled
 */
static int inTestMode (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_GPGME_UNIT_TEST, 0);
	if (k && !strcmp (keyString (k), "1"))
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

/**
 * @brief checks if a given key holds a null value.
 * @retval 0 if the Key k does not hold a null value.
 * @retval 1 if the Key k holds a null value.
 */
static inline int isNullValue (const Key * k)
{
	return keyGetValueSize (k) == 0;
}

/**
 * @brief lookup if the text mode is disabled in the plugin config.
 * Text mode is enabled per default.
 * @param conf KeySet holding the plugin configuration.
 * @retval 0 text mode is not enabled
 * @retval 1 text mode is enabled
 */
static int isTextMode (KeySet * conf)
{
	Key * k = ksLookupByName (conf, ELEKTRA_GPGME_CONFIG_TEXTMODE, 0);
	if (k && !strcmp (keyString (k), "0"))
	{
		return 0;
	}
	return 1;
}

/*
 * @brief invoke gpgme_key_unref on all keys and free the array.
 * @param recipients the array to be released.
 */
static void freeRecipientArray (gpgme_key_t * recipients)
{
	unsigned long index = 0;
	if (recipients)
	{
		while (recipients[index])
		{
			gpgme_key_unref (recipients[index++]);
		}
		elektraFree (recipients);
	}
}

/**
 * @brief extract all GPG recipients that shall be used for encryption.
 * @param config holds the plugin configuration
 * @param ctx holds the gpgme context to be used
 * @return the recipients as NULL-terminated array of gpgme_key_t, or just NULL if no recipients is specified in config. Must be freed by
 * the caller.
 */
static gpgme_key_t * extractRecipientFromPluginConfig (KeySet * config, Key * errorKey, gpgme_ctx_t ctx)
{
	gpgme_error_t err;
	gpgme_key_t key;

	keylist_t list;
	Key * gpgRecipientRoot = ksLookupByName (config, ELEKTRA_RECIPIENT_KEY, 0);

	elektraGpgmeKeylistInit (&list);

	// append root (gpg/key) as recipient
	if (gpgRecipientRoot && strlen (keyString (gpgRecipientRoot)) > 0)
	{
		err = gpgme_get_key (ctx, keyString (gpgRecipientRoot), &key, 0);
		if (err)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Failed to read the specified GPG key. Reason: %s",
								gpgme_strerror (err));
			elektraGpgmeKeylistFree (&list);
			return NULL;
		}

		if (key)
		{
			if (!elektraGpgmeKeylistAdd (&list, key))
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
				elektraGpgmeKeylistFree (&list);
				return NULL;
			}
		}
	}

	// append keys beneath root (crypto/key/#_) as recipients
	if (gpgRecipientRoot)
	{
		Key * k;

		for (elektraCursor it = 0; it < ksGetSize (config); ++it)
		{
			k = ksAtCursor (config, it);
			if (keyIsBelow (k, gpgRecipientRoot))
			{
				err = gpgme_get_key (ctx, keyString (k), &key, 0);
				if (err)
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
						errorKey, "Failed to read the specified GPG key. Reason: %s", gpgme_strerror (err));
					elektraGpgmeKeylistFree (&list);
					return NULL;
				}

				if (key)
				{
					if (!elektraGpgmeKeylistAdd (&list, key))
					{
						ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
						elektraGpgmeKeylistFree (&list);
						return NULL;
					}
				}
			}
		}
	}

	if (list.size > 0)
	{
		unsigned long index = 0;
		gpgme_key_t tempKey;

		// allocate one extra slot for the NULL-terminator
		gpgme_key_t * keyArray = elektraMalloc ((list.size + 1) * sizeof (gpgme_key_t));
		if (!keyArray)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
			elektraGpgmeKeylistFree (&list);
			return NULL;
		}

		elektraGpgmeKeylistRewind (&list);
		while ((tempKey = elektraGpgmeKeylistNext (&list)))
		{
			keyArray[index++] = tempKey;
		}
		keyArray[index] = NULL;

		elektraGpgmeKeylistFree (&list);
		return keyArray;
	}
	elektraGpgmeKeylistFree (&list);
	return NULL;
}

/**
 * @brief read out the contents of src and place it to dst.
 * @param src the source of type gpgme_data_t
 * @param dst the Elektra key of type Key
 * @param errorKey will hold an error description in case of failure
 * @param textMode set to 1 if the text mode is enabled.
 * @retval 1 on success
 * @retval -1 on failure
 */
static int transferGpgmeDataToElektraKey (gpgme_data_t src, Key * dst, Key * errorKey, int textMode)
{
	int returnValue = 1; // success
	off_t ciphertextLen;
	ssize_t readCount;
	char * buffer = NULL;

	ciphertextLen = gpgme_data_seek (src, 0, SEEK_END);
	buffer = (char *) elektraMalloc (ciphertextLen);
	if (!buffer)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		returnValue = -1; // failure
		goto cleanup;
	}

	gpgme_data_seek (src, 0, SEEK_SET);
	readCount = gpgme_data_read (src, buffer, ciphertextLen);
	if (readCount != ciphertextLen)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "An error occurred during the en/decryption process. Reason: %s", strerror (errno));
		returnValue = -1; // failure
		goto cleanup;
	}

	if (textMode)
	{
		keySetString (dst, buffer);
	}
	else
	{
		keySetBinary (dst, buffer, ciphertextLen);
	}

cleanup:
	if (buffer)
	{
		elektraFree (buffer);
	}
	return returnValue;
}

/**
 * @brief Concatenate the key IDs held by the argument invalidKeys.
 * @param result will hold a pointer to the allocated list of key IDs. Must be freed by the caller. Will be set to NULL if the memory
 * allocation fails.
 * @param invalidKeys the list of gpgme_invalid_key_t elements to concatenate.
 */
static void generateInvalidKeyErrorMsg (char ** result, gpgme_invalid_key_t invalidKeys)
{
	size_t length = 0;
	gpgme_invalid_key_t iterator = invalidKeys;

	while (iterator)
	{
		// + 2 characters: coma and blank -> <keyID>, <keyID>, ...
		length += strlen (iterator->fpr) + 2;
		iterator = iterator->next;
	}

	if (length == 0)
	{
		*result = NULL;
		return;
	}

	// +1 for the NULL terminator
	*result = elektraMalloc (length + 1);
	if (!result)
	{
		// memory allocation error must be handled by the caller
		return;
	}

	iterator = invalidKeys;
	while (iterator)
	{
		strncat (*result, iterator->fpr, length);
		length -= strlen (iterator->fpr);
		if (iterator->next)
		{
			strncat (*result, ", ", length);
			length -= 2;
		}
		iterator = iterator->next;
	}
}

/**
 * @brief Encrypt all Keys in the KeySet "data" that have a meta-key "gpg/encrpyt" set.
 * @see isMarkedForEncryption (const Key * k)
 * @param handle holds the plugin configuration.
 * @param data the KeySet to be encrypted.
 * @param errorKey will hold and error description if an operation fails.
 * @retval 1 on success.
 * @retval -1 on failure. In this case errorKey will provide a description.
 */
static int gpgEncrypt (Plugin * handle, KeySet * data, Key * errorKey)
{
	int returnValue = 1; // success per default
	int textMode;
	Key * k;

	gpgme_key_t * recipients;
	gpgme_ctx_t ctx;
	gpgme_error_t err;
	gpgme_encrypt_flags_t encryptFlags = GPGME_ENCRYPT_NO_ENCRYPT_TO;

	err = gpgme_new (&ctx);
	if (err)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Failed to initialize gpgme. Reason: %s", gpgme_strerror (err));
		return -1; // at this point nothing has been initialized
	}

	KeySet * pluginConfig = elektraPluginGetConfig (handle);

	textMode = isTextMode (pluginConfig);
	if (textMode)
	{
		gpgme_set_armor (ctx, 1);
	}

	recipients = extractRecipientFromPluginConfig (pluginConfig, errorKey, ctx);
	if (!recipients)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "No valid recipients were specified");
		returnValue = -1;
		goto cleanup;
	}

	if (inTestMode (pluginConfig))
	{
		encryptFlags |= GPGME_ENCRYPT_ALWAYS_TRUST;
	}

	for (elektraCursor it = 0; it < ksGetSize (data); ++it)
	{
		k = ksAtCursor (data, it);
		gpgme_data_t input;
		gpgme_data_t ciphertext;
		gpgme_encrypt_result_t result;
		gpgme_invalid_key_t invalidKey;

		if (!isMarkedForEncryption (k) || isSpecNamespace (k) || isNullValue (k))
		{
			continue;
		}

		// preserve the data type of k (string, binary)
		if (!keyIsBinary (k))
		{
			err = gpgme_data_new_from_mem (&input, keyString (k), strlen (keyString (k)) + 1, 0);
		}
		else
		{
			keySetMeta (k, ELEKTRA_GPGME_META_BINARY, "1");
			err = gpgme_data_new_from_mem (&input, keyValue (k), keyGetValueSize (k), 0);
		}
		if (err)
		{
			returnValue = -1;
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
			goto cleanup;
		}

		err = gpgme_data_new (&ciphertext);
		if (err)
		{
			returnValue = -1;
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
			gpgme_data_release (input);
			goto cleanup;
		}

		err = gpgme_op_encrypt (ctx, recipients, encryptFlags, input, ciphertext);
		if (err)
		{
			returnValue = -1;
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
			gpgme_data_release (ciphertext);
			gpgme_data_release (input);
			goto cleanup;
		}

		result = gpgme_op_encrypt_result (ctx);
		invalidKey = result->invalid_recipients;
		if (invalidKey)
		{
			char * errorMsg = NULL;
			returnValue = -1;
			generateInvalidKeyErrorMsg (&errorMsg, invalidKey);
			if (errorMsg)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Invalid key ID(s): %s", errorMsg);
				elektraFree (errorMsg);
			}
			else
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
			}
			gpgme_data_release (ciphertext);
			gpgme_data_release (input);
			goto cleanup;
		}

		// update Elektra key to encrypted value
		if (transferGpgmeDataToElektraKey (ciphertext, k, errorKey, textMode) != 1)
		{
			// error description has been set by transferGpgmeDataToElektraKey()
			returnValue = -1;
			gpgme_data_release (ciphertext);
			gpgme_data_release (input);
			goto cleanup;
		}

		gpgme_data_release (ciphertext);
		gpgme_data_release (input);
	}

cleanup:
	freeRecipientArray (recipients);
	gpgme_release (ctx);
	return returnValue;
}

/**
 * @brief Decrypt all Keys in the KeySet "data" that have a meta-key "gpg/encrpyt" set.
 * @see isMarkedForEncryption (const Key * k)
 * @param handle holds the plugin configuration.
 * @param data the KeySet to be decrypted.
 * @param errorKey will hold and error description if an operation fails.
 * @retval 1 on success.
 * @retval -1 on failure. In this case errorKey will provide a description.
 */
static int gpgDecrypt (ELEKTRA_UNUSED Plugin * handle, KeySet * data, Key * errorKey)
{
	int returnValue = 1; // success
	Key * k;

	gpgme_ctx_t ctx;
	gpgme_error_t err;

	err = gpgme_new (&ctx);
	if (err)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Failed to initialize gpgme. Reason: %s", gpgme_strerror (err));
		return -1; // at this point nothing has been initialized
	}

	for (elektraCursor it = 0; it < ksGetSize (data); ++it)
	{
		k = ksAtCursor (data, it);
		if (!isMarkedForEncryption (k) || isSpecNamespace (k) || isNullValue (k))
		{
			continue;
		}

		gpgme_data_t ciphertext;
		gpgme_data_t plaintext;
		int originallyBinary = isOriginallyBinary (k);

		err = gpgme_data_new_from_mem (&ciphertext, keyValue (k), keyGetValueSize (k), 0);
		if (err)
		{
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
			returnValue = -1;
			goto cleanup;
		}

		err = gpgme_data_new (&plaintext);
		if (err)
		{
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
			returnValue = -1;
			gpgme_data_release (ciphertext);
			goto cleanup;
		}

		err = gpgme_op_decrypt (ctx, ciphertext, plaintext);
		if (err)
		{
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
			returnValue = -1;
			gpgme_data_release (plaintext);
			gpgme_data_release (ciphertext);
			goto cleanup;
		}

		if (transferGpgmeDataToElektraKey (plaintext, k, errorKey, !originallyBinary) != 1)
		{
			// error description has been set by transferGpgmeDataToElektraKey()
			returnValue = -1;
			gpgme_data_release (plaintext);
			gpgme_data_release (ciphertext);
			goto cleanup;
		}

		gpgme_data_release (plaintext);
		gpgme_data_release (ciphertext);
	}

cleanup:
	gpgme_release (ctx);
	return returnValue;
}

int elektraGpgmeOpen (Plugin * handle, Key * errorKey)
{
	gpgme_error_t err;

	// if the plugin is used by the unit test, initialization of libgpgme is done by the test code
	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	if (inTestMode (pluginConfig))
	{
		return 1; // success
	}

	// initialize libgpgme
	gpgme_check_version (NULL);
	// NOTE the code below is recommended by the gpgme manual
	//	gpgme_set_locale (NULL, LC_CTYPE, setlocale (LC_CTYPE, NULL));
	// #ifndef HAVE_W32_SYSTEM
	//	gpgme_set_locale (NULL, LC_MESSAGES, setlocale (LC_MESSAGES, NULL));
	// #endif

	err = gpgme_engine_check_version (GPGME_PROTOCOL_OpenPGP);
	if (err)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Internal error: %s", gpgme_strerror (err));
		return -1; // failure
	}
	return 1; // success
}

int elektraGpgmeClose (ELEKTRA_UNUSED Plugin * handle, ELEKTRA_UNUSED Key * errorKey)
{
	// at the moment there is nothing to do
	return 1; // success
}

int elektraGpgmeGet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	// publish the module configuration to Elektra (establish the contract)
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/" ELEKTRA_PLUGIN_NAME))
	{
		KeySet * moduleConfig = ksNew (30,
#include "contract.h"
					       KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1; // success
	}

	return gpgDecrypt (handle, ks, parentKey);
}

int elektraGpgmeSet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	return gpgEncrypt (handle, ks, parentKey);
}

int elektraGpgmeCheckconf (Key * errorKey, KeySet * conf)
{
	gpgme_ctx_t ctx;
	gpgme_error_t err;

	err = gpgme_new (&ctx);
	if (err)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Failed to initialize gpgme. Reason: %s", gpgme_strerror (err));
		return -1; // at this point nothing has been initialized
	}

	gpgme_key_t * recipients = extractRecipientFromPluginConfig (conf, errorKey, ctx);
	gpgme_release (ctx);

	if (recipients)
	{
		freeRecipientArray (recipients);
	}
	else
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "No valid recipients were specified");
		return -1; // failure
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_OPEN,  &elektraGpgmeOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraGpgmeClose,
			ELEKTRA_PLUGIN_GET,   &elektraGpgmeGet,
			ELEKTRA_PLUGIN_SET,   &elektraGpgmeSet,
			ELEKTRA_PLUGIN_END);
}
