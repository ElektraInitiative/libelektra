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

int elektraCryptoInit()
{
	return elektraCryptoGcryInit();
}

void elektraCryptoTeardown()
{
	// nothing to do for libgcrypt, but maybe other libraries need clean-up
}

elektraCryptoHandle *elektraCryptoHandleCreate(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen)
{
	return elektraCryptoGcryHandleCreate(key, keyLen, iv, ivLen);
}

void elektraCryptoHandleDestroy(elektraCryptoHandle *handle)
{
	elektraCryptoGcryHandleDestroy(handle);
}

int elektraCryptoEncrypt(elektraCryptoHandle *handle, Key *k)
{
	return elektraCryptoGcryEncrypt(handle, k);
}

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

