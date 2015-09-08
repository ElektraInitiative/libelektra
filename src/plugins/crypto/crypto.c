/**
* @file
*
* @brief filter plugin providing cryptographic operations
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include "crypto.h"
#include "gcrypt_operations.h"


int elektraCryptoOpen(Plugin *handle, Key *errorKey)
{
	return 1;
}

int elektraCryptoClose(Plugin *handle, Key *errorKey)
{
	return 1;
}

int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return 1;
}

int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return 1;
}

int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey)
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

