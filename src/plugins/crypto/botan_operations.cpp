/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <botan/filters.h>
#include <botan/init.h>
#include <botan/lookup.h>
#include <botan/pipe.h>
#include <botan/symkey.h>
#include <kdbplugin.h>

using namespace ckdb;
using namespace Botan;

extern "C" {

#include "botan_operations.h"
#include "crypto.h"
#include <kdberrors.h>

/**
 * @brief read the cryptographic key from the given keyset.
 * @retval NULL on error
 * @retval a SymmetricKey to be used for the cryptographic operation
 */
static SymmetricKey * getSymmKey (KeySet * config, Key * errorKey)
{
	Key * key = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_KEY_PATH, 0);
	if (key == NULL)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in configuration",
				    ELEKTRA_CRYPTO_PARAM_KEY_PATH);
		return NULL;
	}
	return new SymmetricKey (static_cast<const byte *> (keyValue (key)), keyGetValueSize (key));
}

/**
 * @brief read the cryptographic initialization vector (IV) from the given keyset.
 * @retval NULL on error
 * @retval an InitializationVector to be used for the cryptographic operation
 */
static InitializationVector * getIv (KeySet * config, Key * errorKey)
{
	Key * iv = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_IV_PATH, 0);
	if (iv == NULL)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s in configuration",
				    ELEKTRA_CRYPTO_PARAM_IV_PATH);
		return NULL;
	}
	return new InitializationVector (static_cast<const byte *> (keyValue (iv)), keyGetValueSize (iv));
}


int elektraCryptoBotanInit (Key * errorKey)
{
	try
	{
		LibraryInitializer::initialize ();
	}
	catch (std::exception & e)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INIT, errorKey, "Botan initialization failed: %s", e.what ());
		return -1; // failure
	}
	return 1; // success
}

int elektraCryptoBotanEncrypt (KeySet * pluginConfig, Key * k, Key * errorKey)
{
	// get cryptographic material
	SymmetricKey * cryptoKey = getSymmKey (pluginConfig, errorKey);
	if (!cryptoKey)
	{
		return -1; // failure, error has been set by getSymmKey(...)
	}

	InitializationVector * cryptoIv = getIv (pluginConfig, errorKey);
	if (!cryptoIv)
	{
		delete cryptoKey;
		return -1; // failure, error has been set by getIv(...)
	}

	// setup pipe and crypto filters
	try
	{
		Pipe encryptor (get_cipher ("AES-256-CBC", *cryptoKey, *cryptoIv, ENCRYPTION));
	}
	catch (std::exception & e)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption failed because: %s", e.what ());
		delete cryptoIv;
		delete cryptoKey;
		return -1; // failure
	}

	delete cryptoIv;
	delete cryptoKey;
	return 1; // success
}

int elektraCryptoBotanDecrypt (KeySet * pluginConfig, Key * k, Key * errorKey)
{
	return -1;
}

} // extern "C"
