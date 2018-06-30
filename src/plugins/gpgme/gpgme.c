/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations using GPGME
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif
#include "gpgme.h"
#include <gpgme.h>
#include <kdb.h>
#include <kdberrors.h>
#include <kdbtypes.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>

// define linked list of gpgme_key_t elements
struct internal_keylist
{
	gpgme_key_t key;
	struct internal_keylist * next;
};

struct keylist
{
	struct internal_keylist * head;
	struct internal_keylist * iterator;
	unsigned long size;
};

typedef struct keylist keylist_t;

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
 * @brief checks if a given Key k is in the spec namespace.
 * @retval 0 if the Key k is in the spec namespace.
 * @retval 1 if the Key k is NOT in the spec namespace.
 */
static inline int isSpecNamespace (const Key * k)
{
	return (keyGetNamespace (k) == KEY_NS_SPEC);
}

/**
 * @brief initializes the internal key list
 * @param list the list
 */
static void keylistInit (keylist_t * list)
{
	list->head = NULL;
	list->iterator = NULL;
	list->size = 0;
}

/**
 * @brief add key to the list
 * @param list the key list
 * @param key the element to be added to the list
 * @retval 0 in case of insufficient memory (malloc failed)
 * @retval 1 in case of success
 */
static int keylistAdd (keylist_t * list, gpgme_key_t key)
{
	if (list->iterator)
	{
		// append key to existing list
		list->iterator->next = elektraMalloc (sizeof (struct internal_keylist));
		if (!list->iterator->next)
		{
			return 0; // not enough memory
		}
		list->iterator->next->key = key;
		list->iterator->next->next = NULL;
		list->iterator = list->iterator->next;
	}
	else
	{
		// first element in empty list
		list->head = elektraMalloc (sizeof (struct internal_keylist));
		if (!list->head)
		{
			return 0; // not enough memory
		}
		list->head->key = key;
		list->head->next = NULL;
		list->iterator = list->head;
	}
	list->size++;
	return 1; // success
}

/**
 * @brief reset the iterator of the list to the head of the list.
 */
static void keylistRewind (keylist_t * list)
{
	list->iterator = list->head;
}

/**
 * @brief get the next gpgme_key_t of the list.
 * @return the next element or NULL if no such element exists.
 */
static gpgme_key_t keylistNext (keylist_t * list)
{
	if (!list->iterator)
	{
		return NULL;
	}
	gpgme_key_t key = list->iterator->key;
	list->iterator = list->iterator->next;
	return key;
}

/**
 * @brief release the key list
 * @param list the list to be released
 */
static void keylistRelease (keylist_t * list)
{
	struct internal_keylist * iterator = list->head;
	struct internal_keylist * next;

	while (iterator)
	{
		next = iterator->next;
		gpgme_key_unref (iterator->key);
		elektraFree (iterator);
		iterator = next;
	}
	list->head = NULL;
	list->iterator = NULL;
	list->size = 0;
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

	keylistInit (&list);

	// append root (gpg/key) as recipient
	if (gpgRecipientRoot && strlen (keyString (gpgRecipientRoot)) > 0)
	{
		err = gpgme_get_key (ctx, (char *) keyString (gpgRecipientRoot), &key, 0);
		if (err)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Failed to receive the GPG key because: %s",
					    gpgme_strerror (err));
			keylistRelease (&list);
			return NULL;
		}

		if (key)
		{
			// TODO remove debug fuckup
			fprintf (stdout, "extracted key %p sizeof %lu\n", (void *) key, sizeof (gpgme_key_t));

			if (!keylistAdd (&list, key))
			{
				ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
				keylistRelease (&list);
				return NULL;
			}
		}
	}

	// append keys beneath root (crypto/key/#_) as recipients
	if (gpgRecipientRoot)
	{
		Key * k;

		ksRewind (config);
		while ((k = ksNext (config)) != 0)
		{
			if (keyIsBelow (k, gpgRecipientRoot))
			{
				err = gpgme_get_key (ctx, (char *) keyString (k), &key, 0);
				if (err)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Failed to receive the GPG key because: %s",
							    gpgme_strerror (err));
					keylistRelease (&list);
					return NULL;
				}

				if (key)
				{
					// TODO remove debug fuckup
					fprintf (stdout, "extracted key %p sizeof %lu\n", (void *) key, sizeof (gpgme_key_t));

					if (!keylistAdd (&list, key))
					{
						ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
						keylistRelease (&list);
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
			ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
			keylistRelease (&list);
			return NULL;
		}

		keylistRewind (&list);
		while ((tempKey = keylistNext (&list)))
		{
			keyArray[index++] = tempKey;
		}
		keyArray[index] = NULL;

		keylistRelease (&list);
		return keyArray;
	}
	keylistRelease (&list);
	return NULL;
}

/**
 * @brief read out the contents of src and place it to dst.
 * @param src the source of type gpgme_data_t
 * @param dst the Elektra key of type Key
 * @param errorKey will hold an error description in case of failure
 * @retval 1 on success
 * @retval -1 on failure
 */
static int transferGpgmeDataToElektraKey (gpgme_data_t src, Key * dst, Key * errorKey)
{
	int returnValue = 1; // success
	off_t ciphertextLen;
	ssize_t readCount;
	char * buffer = NULL;

	ciphertextLen = gpgme_data_seek (src, 0, SEEK_END);
	buffer = (char *) elektraMalloc (ciphertextLen);
	if (!buffer)
	{
		ELEKTRA_SET_ERROR (87, errorKey, "Memory allocation failed");
		returnValue = -1; // failure
		goto cleanup;
	}

	gpgme_data_seek (src, 0, SEEK_SET);
	readCount = gpgme_data_read (src, buffer, ciphertextLen);
	if (readCount != ciphertextLen)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_GPGME, errorKey, "An error during occured during the data transfer.");
		returnValue = -1; // failure
		goto cleanup;
	}

	keySetString (dst, buffer);

cleanup:
	if (buffer)
	{
		elektraFree (buffer);
	}
	return returnValue;
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
	Key * k;

	gpgme_key_t * recipients;
	gpgme_ctx_t ctx;
	gpgme_error_t err;

	fprintf (stdout, "encrypt\n"); // TODO remove debug stuff

	err = gpgme_new (&ctx);
	if (err)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Failed to create the gpgme context because: %s", gpgme_strerror (err));
		return -1; // at this point nothing has been initialized
	}

	// TODO make it possible to enable/disable text mode
	gpgme_set_armor (ctx, 1);

	KeySet * pluginConfig = elektraPluginGetConfig (handle);
	recipients = extractRecipientFromPluginConfig (pluginConfig, errorKey, ctx);
	if (!recipients)
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_GPGME_CONFIG, errorKey,
				   "No valid recipients were specified. Please provide at least one valid GPG key.");
		returnValue = -1;
		goto cleanup;
	}

	ksRewind (data);
	while ((k = ksNext (data)))
	{
		gpgme_data_t input;
		gpgme_data_t ciphertext;
		gpgme_encrypt_result_t result;

		fprintf (stdout, "checking %s\n", keyName (k)); // TODO remove debug stuff

		if (!isMarkedForEncryption (k) || isSpecNamespace (k))
		{
			// TODO remove debugging stuff
			fprintf (stdout, "%s spec namespace? %d\n", keyName (k), isSpecNamespace (k));
			fprintf (stdout, "%s marked for enc? %d\n", keyName (k), isMarkedForEncryption (k));
			continue;
		}

		// TODO preserve the data type of k (string, binary, null)
		err = gpgme_data_new_from_mem (&input, keyString (k), keyGetValueSize (k), 0);
		if (err)
		{
			returnValue = -1;
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
			goto cleanup;
		}

		err = gpgme_data_new (&ciphertext);
		if (err)
		{
			returnValue = -1;
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
			gpgme_data_release (input);
			goto cleanup;
		}

		//////////////////////////////////////////////////////////////////////////////////////
		fprintf (stdout, "gonna encrypt %s\n", keyName (k)); // TODO remove debug stuff
		int i = 0;
		gpgme_key_t x = recipients[i++];
		while (x)
		{
			fprintf (stdout, "will use address %p\n", (void *) x);
			fprintf (stdout, "using key: %s\n", x->fpr);
			x = recipients[i++];
		}
		//////////////////////////////////////////////////////////////////////////////////////

		err = gpgme_op_encrypt (ctx, recipients, GPGME_ENCRYPT_NO_ENCRYPT_TO, input, ciphertext);
		if (err)
		{
			returnValue = -1;
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
			gpgme_data_release (ciphertext);
			gpgme_data_release (input);
			goto cleanup;
		}

		result = gpgme_op_encrypt_result (ctx);
		if (result->invalid_recipients)
		{
			// TODO add warning to error key
		}

		fprintf (stdout, "gonna transfer the results to %s\n", keyName (k)); // TODO remove debug stuff

		// update Elektra key to encrypted value
		if (transferGpgmeDataToElektraKey (ciphertext, k, errorKey) != 1)
		{
			// error description has been set by transferGpgmeDataToElektraKey()
			returnValue = -1;
			gpgme_data_release (ciphertext);
			gpgme_data_release (input);
			goto cleanup;
		}

		fprintf (stdout, "key value = %s\n", keyString (k)); // TODO remove debugging stuff

		gpgme_data_release (ciphertext);
		gpgme_data_release (input);
	}

cleanup:
	if (recipients)
	{
		elektraFree (recipients);
	}
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
static int gpgDecrypt (ELEKTRA_UNUSED Plugin * handle, KeySet * data, ELEKTRA_UNUSED Key * errorKey)
{
	int returnValue = 1; // success
	Key * k;
	KeySet * pluginConfig = elektraPluginGetConfig (handle);

	gpgme_ctx_t ctx;
	gpgme_error_t err;

	fprintf (stdout, "decrypt\n"); // TODO remove debug stuff

	err = gpgme_new (&ctx);
	if (err)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Failed to the gpgme context because: %s", gpgme_strerror (err));
		return -1; // at this point nothing has been initialized
	}

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (!isMarkedForEncryption (k) || isSpecNamespace (k))
		{
			continue;
		}

		gpgme_data_t ciphertext;
		gpgme_data_t plaintext;
		gpgme_decrypt_result_t dec_result;

		err = gpgme_data_new_from_mem (&ciphertext, keyValue (k), keyGetValueSize (k), 0);
		if (err)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
			returnValue = -1;
			goto cleanup;
		}

		err = gpgme_data_new (&plaintext);
		if (err)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
			returnValue = -1;
			gpgme_data_release (ciphertext);
			goto cleanup;
		}

		err = gpgme_op_decrypt (ctx, ciphertext, plaintext);
		if (err)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
			returnValue = -1;
			gpgme_data_release (plaintext);
			gpgme_data_release (ciphertext);
			goto cleanup;
		}

		dec_result = gpgme_op_decrypt_result (ctx);

		// TODO consider original data type (string, binary, null)
		if (transferGpgmeDataToElektraKey (plaintext, k, errorKey) != 1)
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

int elektraGpgmeOpen (ELEKTRA_UNUSED Plugin * handle, ELEKTRA_UNUSED Key * errorKey)
{
	gpgme_error_t err;

	gpgme_check_version (NULL);
	gpgme_set_locale (NULL, LC_CTYPE, setlocale (LC_CTYPE, NULL));
#ifndef HAVE_W32_SYSTEM
	gpgme_set_locale (NULL, LC_MESSAGES, setlocale (LC_MESSAGES, NULL));
#endif

	err = gpgme_engine_check_version (GPGME_PROTOCOL_OpenPGP);
	if (err)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Internal error: %s", gpgme_strerror (err));
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
	if (!strcmp (keyName (parentKey), "system/elektra/modules/" ELEKTRA_PLUGIN_NAME))
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
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GPGME, errorKey, "Failed to create the gpgme context because: %s", gpgme_strerror (err));
		return -1; // at this point nothing has been initialized
	}

	gpgme_key_t * recipients = extractRecipientFromPluginConfig (conf, errorKey, ctx);
	gpgme_release (ctx);

	if (recipients)
	{
		elektraFree (recipients);
	}
	else
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_GPGME_CONFIG, errorKey,
				   "No valid recipients were specified. Please provide at least one valid GPG key.");
		return -1; // failure
	}
	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (gpgme)
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
			ELEKTRA_PLUGIN_OPEN,  &elektraGpgmeOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraGpgmeClose,
			ELEKTRA_PLUGIN_GET,   &elektraGpgmeGet,
			ELEKTRA_PLUGIN_SET,   &elektraGpgmeSet,
			ELEKTRA_PLUGIN_END);
}
