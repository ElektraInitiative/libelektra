/**
* @file
*
* @brief filter plugin providing cryptographic operations
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif
#include "crypto.h"
#include "gcrypt_operations.h"


int elektraCryptoOpen(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 1;
}

int elektraCryptoClose(Plugin *handle ELEKTRA_UNUSED, Key *errorKey ELEKTRA_UNUSED)
{
	return 1;
}

int elektraCryptoGet(Plugin *handle ELEKTRA_UNUSED, KeySet *ks, Key *parentKey)
{
	// Publish module configuration to Elektra
	if (!strcmp (keyName(parentKey), "system/elektra/modules/crypto"))
	{
		KeySet *moduleConfig = ksNew (30,
			keyNew ("system/elektra/modules/crypto", KEY_VALUE, "crypto plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/crypto/exports", KEY_END),
			keyNew ("system/elektra/modules/crypto/exports/get", KEY_FUNC, elektraCryptoGet, KEY_END),
			keyNew ("system/elektra/modules/crypto/exports/set", KEY_FUNC, elektraCryptoSet, KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			keyNew ("system/elektra/modules/crypto/infos", KEY_VALUE, "Information about crypto plugin is paced in the keys below", KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/author", KEY_VALUE, "Peter Nirschl <peter.nirschl@gmail.com>", KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/licence", KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/needs", KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/provides", KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/placements", KEY_VALUE, "postgetstorage presetstorage", KEY_END),
			keyNew ("system/elektra/modules/crypto/infos/description", KEY_VALUE, "filter plugin for cryptographic operations", KEY_END),
			KS_END);
		ksAppend (ks, moduleConfig);
		ksDel (moduleConfig);
		return 1;
	}

	return 1;
}

int elektraCryptoSet(Plugin *handle ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1;
}

int elektraCryptoError(Plugin *handle ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *parentKey ELEKTRA_UNUSED)
{
	return 1;
}

/**
 * @brief initialize the crypto backend
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoInit()
{
	return elektraCryptoGcryInit();
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
 * @returns a new crypto handle, or NULL if an error occurs
 */
elektraCryptoHandle *elektraCryptoHandleCreate(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen)
{
	return elektraCryptoGcryHandleCreate(key, keyLen, iv, ivLen);
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
int elektraCryptoEncrypt(elektraCryptoHandle *handle, Key *k)
{
	return elektraCryptoGcryEncrypt(handle, k);
}

/**
 * @brief decrypt the content of the Key k
 * @retval 1 on success
 * @retval -1 on failure
 */
int elektraCryptoDecrypt(elektraCryptoHandle *handle, Key *k)
{
	return elektraCryptoGcryDecrypt(handle, k);
}

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto){
return elektraPluginExport("crypto",
		ELEKTRA_PLUGIN_GET, &elektraCryptoGet,
		ELEKTRA_PLUGIN_SET, &elektraCryptoSet,
		ELEKTRA_PLUGIN_END);
}

