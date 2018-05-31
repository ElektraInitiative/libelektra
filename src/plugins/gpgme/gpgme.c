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
 * @brief Encrypt all Keys in the KeySet "data" that have a meta-key "gpg/encrpyt" set.
 * @see isMarkedForEncryption (const Key * k)
 * @param handle holds the plugin configuration.
 * @param data the KeySet to be encrypted.
 * @param errorKey will hold and error description if an operation fails.
 * @retval 1 on success.
 * @retval -1 on failure. In this case errorKey will provide a description.
 */
static int gpgEncrypt (ELEKTRA_UNUSED Plugin * handle, KeySet * data, ELEKTRA_UNUSED Key * errorKey)
{
	Key * k;
	// TODO KeySet * pluginConfig = elektraPluginGetConfig (handle);

	// TODO convert pluginConfig to an array of recipient keys

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (!isMarkedForEncryption (k) || isSpecNamespace (k))
		{
			continue;
		}

		// TODO use gpgme to encrypt the value of k
	}
	return 1;
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
	Key * k;
	// TODO KeySet * pluginConfig = elektraPluginGetConfig (handle);

	gpgme_ctx_t ctx;
	gpgme_error_t err;

	err = gpgme_new (&ctx);

	ksRewind (data);
	while ((k = ksNext (data)) != 0)
	{
		if (!isMarkedForEncryption (k) || isSpecNamespace (k))
		{
			continue;
		}

		// TODO use gpgme to decrypt the value of k
		gpgme_data_t in;
		gpgme_data_t out;
		gpgme_decrypt_result_t dec_result;

		// TODO error handling
		// TODO consider original data type (string, binary, null)

		err = gpgme_data_new_from_mem (&in, keyValue (k), keyGetValueSize (k), 0);
		err = gpgme_data_new (&out);

		err = gpgme_op_decrypt (ctx, out, in);
		dec_result = gpgme_op_decrypt_result (ctx);
		// TODO handle gppme errors

		// TODO finish here

		gpgme_data_release (out);
		gpgme_data_release (in);
	}
	gpgme_release (ctx);
	return 1;
}

int elektraGpgmeOpen (ELEKTRA_UNUSED Plugin * handle, ELEKTRA_UNUSED Key * errorKey)
{
	gpgme_error_t err;

	gpgme_check_version (NULL);
	setlocale (LC_ALL, "");
	gpgme_set_locale (NULL, LC_CTYPE, setlocale (LC_CTYPE, NULL));
#ifndef HAVE_W32_SYSTEM
	gpgme_set_locale (NULL, LC_MESSAGES, setlocale (LC_MESSAGES, NULL));
#endif

	err = gpgme_engine_check_version (GPGME_PROTOCOL_OpenPGP);
	if (err)
	{
		// TODO set errorKey
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

	return gpgEncrypt (handle, ks, parentKey);
}

int elektraGpgmeSet (Plugin * handle, KeySet * ks, Key * parentKey)
{
	return gpgDecrypt (handle, ks, parentKey);
}

int elektraGpgmeCheckconf (ELEKTRA_UNUSED Key * errorKey, ELEKTRA_UNUSED KeySet * conf)
{
	// TODO verify that at least one gpg recipient is available in conf
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
