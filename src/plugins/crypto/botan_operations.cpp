/**
 * @file
 *
 * @brief cryptographic interface using the Botan library
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
#include <string.h>

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
	// check if key has been marked for encryption
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypt == NULL || strlen (keyString (metaEncrypt)) == 0)
	{
		// nothing to do
		return 1;
	}

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

	try
	{
		// setup pipe and crypto filter
		Pipe encryptor (get_cipher (ELEKTRA_CRYPTO_BOTAN_ALGORITHM, *cryptoKey, *cryptoIv, ENCRYPTION));
		kdb_octet_t flags;

		switch (keyIsString (k))
		{
		case 1: // string
			flags = ELEKTRA_CRYPTO_FLAG_STRING;
			break;
		case -1: // NULL pointer
			flags = ELEKTRA_CRYPTO_FLAG_NULL;
			break;
		default: // binary
			flags = ELEKTRA_CRYPTO_FLAG_NONE;
			break;
		}

		// encryption process
		encryptor.start_msg ();
		encryptor.write (static_cast<const byte *> (&flags), sizeof (kdb_octet_t));

		if (flags == ELEKTRA_CRYPTO_FLAG_STRING)
		{
			const std::string stringVal (keyString (k));
			encryptor.write (stringVal);
		}
		else if (flags == ELEKTRA_CRYPTO_FLAG_NONE && keyGetValueSize (k) > 0)
		{
			encryptor.write (static_cast<const byte *> (keyValue (k)), keyGetValueSize (k));
		}
		encryptor.end_msg ();

		// write the encrypted data back to the Key
		const size_t msgLength = encryptor.remaining ();
		if (msgLength > 0)
		{
			byte * buffer = new byte[msgLength];
			if (!buffer)
			{
				throw std::bad_alloc ();
			}
			const size_t buffered = encryptor.read (buffer, msgLength);
			keySetBinary (k, buffer, buffered);
			delete[] buffer;
		}
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
	// check if key has been marked for encryption
	const Key * metaEncrypt = keyGetMeta (k, ELEKTRA_CRYPTO_META_ENCRYPT);
	if (metaEncrypt == NULL || strlen (keyString (metaEncrypt)) == 0)
	{
		// nothing to do
		return 1;
	}

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

	try
	{
		// setup pipe and crypto filter
		Pipe decryptor (get_cipher (ELEKTRA_CRYPTO_BOTAN_ALGORITHM, *cryptoKey, *cryptoIv, DECRYPTION));
		kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;

		// decrypt the conent
		decryptor.process_msg (static_cast<const byte *> (keyValue (k)), keyGetValueSize (k));

		if (decryptor.remaining () > 0)
		{
			// decode the "header" flags
			decryptor.read (static_cast<byte *> (&flags), sizeof (kdb_octet_t));
		}

		const size_t msgLength = decryptor.remaining ();
		if (msgLength > 0)
		{
			if (flags == ELEKTRA_CRYPTO_FLAG_STRING)
			{
				const std::string stringVal = decryptor.read_all_as_string ();
				keySetString (k, stringVal.c_str ());
			}
			else
			{
				byte * buffer = new byte[msgLength];
				if (!buffer)
				{
					throw std::bad_alloc ();
				}
				const size_t buffered = decryptor.read (buffer, msgLength);
				keySetBinary (k, buffer, buffered);
				delete[] buffer;
			}
		}
		else
		{
			keySetBinary (k, NULL, 0);
		}
	}
	catch (std::exception & e)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_DECRYPT_FAIL, errorKey, "Decryption failed because: %s", e.what ());
		delete cryptoIv;
		delete cryptoKey;
		return -1; // failure
	}

	delete cryptoIv;
	delete cryptoKey;
	return 1; // success
}

} // extern "C"
