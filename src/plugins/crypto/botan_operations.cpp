/**
 * @file
 *
 * @brief cryptographic interface using the Botan library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <botan/auto_rng.h>
#include <botan/filters.h>
#include <botan/init.h>
#include <botan/lookup.h>
#include <botan/pbkdf2.h>
#include <botan/pipe.h>
#include <botan/symkey.h>
#include <kdbplugin.h>

using namespace ckdb;
using namespace Botan;

extern "C" {

#include "botan_operations.h"
#include "crypto.h"
#include "gpg.h"
#include "rand_helper.h"
#include <kdberrors.h>
#include <string.h>

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey pointer to the SymmetricKey
 * @param cIv pointer to the InitializationVector
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForEncryption (KeySet * config, Key * errorKey, Key * k, SymmetricKey ** cKey, InitializationVector ** cIv)
{
	byte salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN];
	const size_t requiredKeyBytes = ELEKTRA_CRYPTO_BOTAN_KEYSIZE + ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE;
	Key * msg = NULL;

	try
	{
		// generate the salt
		AutoSeeded_RNG rng;
		rng.randomize (salt, ELEKTRA_CRYPTO_DEFAULT_SALT_LEN - 1);
		elektraCryptoNormalizeRandomString (salt, sizeof (salt));
		keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, (char *)salt);

		// read iteration count
		const kdb_unsigned_long_t iterations = elektraCryptoGetIterationCount (config);

		// receive master password from the configuration
		Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PWD, 0);
		if (!master)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s", ELEKTRA_CRYPTO_PARAM_MASTER_PWD);
			return -1;
		}
		msg = keyDup (master);
		if (elektraCryptoGpgDecryptMasterPassword (config, errorKey, msg) != 1)
		{
			keyDel (msg);
			return -1;
		}

		// generate/derive the cryptographic key and the IV
		PBKDF * pbkdf = get_pbkdf ("PBKDF2(SHA-256)");
		OctetString derived = pbkdf->derive_key (requiredKeyBytes, std::string (keyString (msg)), salt, sizeof (salt), iterations);

		*cKey = new SymmetricKey (derived.begin (), ELEKTRA_CRYPTO_BOTAN_KEYSIZE);
		*cIv = new InitializationVector (derived.begin () + ELEKTRA_CRYPTO_BOTAN_KEYSIZE, ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE);

		delete pbkdf;
		keyDel (msg);
		return 1;
	}
	catch (std::exception & e)
	{
		if (msg) keyDel (msg);
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INIT, errorKey, "Botan PBKDF2 key derivation failed: %s", e.what ());
		return -1;
	}
}

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey pointer to the SymmetricKey
 * @param cIv pointer to the InitializationVector
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForDecryption (KeySet * config, Key * errorKey, Key * k, SymmetricKey ** cKey, InitializationVector ** cIv)
{
	const size_t requiredKeyBytes = ELEKTRA_CRYPTO_BOTAN_KEYSIZE + ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE;

	// get the salt
	const Key * salt = keyGetMeta (k, ELEKTRA_CRYPTO_META_SALT);
	if (!salt)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing salt as meta-key %s for key %s",
				    ELEKTRA_CRYPTO_META_SALT, keyName (k));
		return -1;
	}

	// get the iteration count
	const kdb_unsigned_long_t iterations = elektraCryptoGetIterationCount (config);

	// receive master password from the configuration
	Key * master = ksLookupByName (config, ELEKTRA_CRYPTO_PARAM_MASTER_PWD, 0);
	if (!master)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_CONFIG_FAULT, errorKey, "missing %s", ELEKTRA_CRYPTO_PARAM_MASTER_PWD);
		return -1;
	}
	Key * msg = keyDup (master);
	if (elektraCryptoGpgDecryptMasterPassword (config, errorKey, msg) != 1)
	{
		keyDel (msg);
		return -1;
	}

	try
	{
		// derive the cryptographic key and the IV
		PBKDF * pbkdf = get_pbkdf ("PBKDF2(SHA-256)");
		OctetString derived = pbkdf->derive_key (requiredKeyBytes, std::string (keyString (msg)),
							 static_cast<const byte *> (keyValue (salt)), keyGetValueSize (salt), iterations);

		*cKey = new SymmetricKey (derived.begin (), ELEKTRA_CRYPTO_BOTAN_KEYSIZE);
		*cIv = new InitializationVector (derived.begin () + ELEKTRA_CRYPTO_BOTAN_KEYSIZE, ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE);

		delete pbkdf;
		keyDel (msg);
		return 1;
	}
	catch (std::exception & e)
	{
		if (msg) keyDel (msg);
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INIT, errorKey, "Botan PBKDF2 key derivation failed: %s", e.what ());
		return -1;
	}
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
	SymmetricKey * cryptoKey = NULL;
	InitializationVector * cryptoIv = NULL;
	if (getKeyIvForEncryption (pluginConfig, errorKey, k, &cryptoKey, &cryptoIv) != 1)
	{
		if (cryptoKey) delete cryptoKey;
		if (cryptoIv) delete cryptoIv;
		return -1;
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
	// get cryptographic material
	SymmetricKey * cryptoKey = NULL;
	InitializationVector * cryptoIv = NULL;
	if (getKeyIvForDecryption (pluginConfig, errorKey, k, &cryptoKey, &cryptoIv) != 1)
	{
		if (cryptoKey) delete cryptoKey;
		if (cryptoIv) delete cryptoIv;
		return -1;
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

char * elektraCryptoBotanCreateRandomString (const kdb_unsigned_short_t length)
{
	try
	{
		kdb_octet_t * buffer = new kdb_octet_t[length];
		if (!buffer)
		{
			return 0;
		}

		AutoSeeded_RNG rng;
		rng.randomize (buffer, length - 1);
		elektraCryptoNormalizeRandomString (buffer, length);
		return reinterpret_cast<char *> (buffer);
	}
	catch (std::exception & e)
	{
		return 0;
	}
}

} // extern "C"
