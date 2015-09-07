/**
* @file
*
* @brief filter plugin providing cryptographic operations
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#include "crypto.h"

#define ELEKTRA_PLUGIN_CRYPTO_SUCCESS (1)
#define ELEKTRA_PLUGIN_CRYPTO_ERROR (-1)

int elektraCryptoOpen(Plugin *handle, Key *errorKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoClose(Plugin *handle, Key *errorKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto){
return elektraPluginExport("crypto",
		ELEKTRA_PLUGIN_GET, &elektraCryptoGet,
		ELEKTRA_PLUGIN_SET, &elektraCryptoSet,
		ELEKTRA_PLUGIN_END);
}

