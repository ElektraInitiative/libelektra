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
#include "gcrypt_operations.h"


/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 1;
}

/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 1;
}

/**
 * @brief establish the Elektra plugin contract and decrypt values, if possible
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoGet(Plugin *handle ELEKTRA_UNUSED, KeySet *ks, Key *parentKey)
{
	// Publish module configuration to Elektra
	if (!strcmp (keyName(parentKey), "system/elektra/modules/crypto"))
	{
		KeySet *moduleConfig = ksNew (30,
#include "contract.h"
			KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	return 1;
}

/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoSet(Plugin *handle ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1;
}

/**
 * @brief TBD
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoError(Plugin *handle ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1;
}

/**
 * @brief initialize the crypto backend
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoInit(Key *errorKey)
{
	return elektraCryptoGcryInit(errorKey);
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
	return elektraCryptoGcryHandleCreate(handle, config, errorKey);
}

/**
 * @brief clean up and destroy a crypto handle
 *
 * This function overwrites confidential data (e.g. keys) and releases the handle.
 */
void elektraCryptoHandleDestroy(elektraCryptoHandle *handle)
{
	elektraCryptoGcryHandleDestroy(handle);
}

/**
 * @brief encrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	return elektraCryptoGcryEncrypt(handle, k, errorKey);
}

/**
 * @brief decrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{
	return elektraCryptoGcryDecrypt(handle, k, errorKey);
}

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto){
return elektraPluginExport("crypto",
		ELEKTRA_PLUGIN_GET, &elektraCryptoGet,
		ELEKTRA_PLUGIN_SET, &elektraCryptoSet,
		ELEKTRA_PLUGIN_END);
}

