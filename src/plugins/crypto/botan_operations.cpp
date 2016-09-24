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
#include <memory>

using namespace ckdb;
using namespace Botan;
using std::unique_ptr;

extern "C" {

#include "botan_operations.h"
#include "crypto.h"
#include "gpg.h"
#include "helper.h"
#include <kdberrors.h>
#include <string.h>

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey holds a unique pointer to an allocated SymmetricKey.
 * @param cIv holds a unique pointer to an allocated InitializationVector.
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForEncryption (KeySet * config, Key * errorKey, Key * k, unique_ptr<SymmetricKey> & cKey,
				  unique_ptr<InitializationVector> & cIv)
{
	byte salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN];
	char * saltHexString = NULL;
	const size_t requiredKeyBytes = ELEKTRA_CRYPTO_BOTAN_KEYSIZE + ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE;
	Key * msg = NULL;

	try
	{
		// generate the salt
		AutoSeeded_RNG rng;
		rng.randomize (salt, sizeof (salt));
		saltHexString = CRYPTO_PLUGIN_FUNCTION (bin2hex) (errorKey, salt, sizeof (salt));
		if (!saltHexString) return -1; // error set by CRYPTO_PLUGIN_FUNCTION(bin2hex)()
		keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, saltHexString);
		elektraFree (saltHexString);

		// read iteration count
		const kdb_unsigned_long_t iterations = CRYPTO_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

		// receive master password from the configuration
		msg = CRYPTO_PLUGIN_FUNCTION (getMasterPassword) (errorKey, config);
		if (!msg) return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getMasterPassword)()

		// generate/derive the cryptographic key and the IV
		PBKDF * pbkdf = get_pbkdf ("PBKDF2(SHA-256)");
		OctetString derived = pbkdf->derive_key (
			requiredKeyBytes, std::string (reinterpret_cast<const char *> (keyValue (msg)), keyGetValueSize (msg)), salt,
			sizeof (salt), iterations);

		cKey = unique_ptr<SymmetricKey> (new SymmetricKey (derived.begin (), ELEKTRA_CRYPTO_BOTAN_KEYSIZE));
		cIv = unique_ptr<InitializationVector> (
			new InitializationVector (derived.begin () + ELEKTRA_CRYPTO_BOTAN_KEYSIZE, ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE));

		delete pbkdf;
		keyDel (msg);
		return 1;
	}
	catch (std::exception & e)
	{
		if (msg) keyDel (msg);
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INIT, errorKey, "Failed to create a cryptographic key for encryption because: %s",
				    e.what ());
		return -1;
	}
}

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey holds a unique pointer to an allocated SymmetricKey.
 * @param cIv holds a unique pointer to an allocated InitializationVector.
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForDecryption (KeySet * config, Key * errorKey, Key * k, unique_ptr<SymmetricKey> & cKey,
				  unique_ptr<InitializationVector> & cIv)
{
	const size_t requiredKeyBytes = ELEKTRA_CRYPTO_BOTAN_KEYSIZE + ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE;
	kdb_octet_t * saltBuffer;
	kdb_unsigned_long_t saltBufferLen = 0;

	// get the salt
	if (CRYPTO_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, &saltBuffer, &saltBufferLen) != 1)
	{
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getSaltFromPayload)()
	}

	// get the iteration count
	const kdb_unsigned_long_t iterations = CRYPTO_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

	// receive master password from the configuration
	Key * msg = CRYPTO_PLUGIN_FUNCTION (getMasterPassword) (errorKey, config);
	if (!msg)
	{
		elektraFree (saltBuffer);
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getMasterPassword)()
	}

	try
	{
		// derive the cryptographic key and the IV
		PBKDF * pbkdf = get_pbkdf ("PBKDF2(SHA-256)");
		OctetString derived = pbkdf->derive_key (
			requiredKeyBytes, std::string (reinterpret_cast<const char *> (keyValue (msg)), keyGetValueSize (msg)), saltBuffer,
			saltBufferLen, iterations);

		cKey = unique_ptr<SymmetricKey> (new SymmetricKey (derived.begin (), ELEKTRA_CRYPTO_BOTAN_KEYSIZE));
		cIv = unique_ptr<InitializationVector> (
			new InitializationVector (derived.begin () + ELEKTRA_CRYPTO_BOTAN_KEYSIZE, ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE));

		delete pbkdf;
		keyDel (msg);
		return 1;
	}
	catch (std::exception & e)
	{
		if (msg) keyDel (msg);
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INIT, errorKey,
				    "Failed to restore the cryptographic key for decryption because: %s", e.what ());
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
	unique_ptr<SymmetricKey> cryptoKey;
	unique_ptr<InitializationVector> cryptoIv;
	if (getKeyIvForEncryption (pluginConfig, errorKey, k, cryptoKey, cryptoIv) != 1)
	{
		return -1;
	}

	// prepare the salt for payload output
	kdb_unsigned_long_t saltLen = 0;
	kdb_octet_t * salt = NULL;

	if (CRYPTO_PLUGIN_FUNCTION (getSaltFromMetakey) (errorKey, k, &salt, &saltLen) != 1)
	{
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getSaltFromMetakey)()
	}

	// remove salt as metakey because it will be encoded into the crypto payload
	keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, NULL);

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

		// write the salt and the encrypted data back to the Key
		const size_t msgLength = encryptor.remaining ();
		if (msgLength > 0)
		{
			byte * buffer = new byte[msgLength + sizeof (kdb_unsigned_long_t) + saltLen];
			if (!buffer)
			{
				throw std::bad_alloc ();
			}

			memcpy (buffer, &saltLen, sizeof (kdb_unsigned_long_t));
			memcpy (buffer + sizeof (kdb_unsigned_long_t), salt, saltLen);

			const size_t buffered = encryptor.read (buffer + sizeof (kdb_unsigned_long_t) + saltLen, msgLength);
			keySetBinary (k, buffer, buffered + sizeof (kdb_unsigned_long_t) + saltLen);
			delete[] buffer;
		}
	}
	catch (std::exception & e)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_ENCRYPT_FAIL, errorKey, "Encryption failed because: %s", e.what ());
		elektraFree (salt);
		return -1; // failure
	}

	elektraFree (salt);
	return 1; // success
}

int elektraCryptoBotanDecrypt (KeySet * pluginConfig, Key * k, Key * errorKey)
{
	// get cryptographic material
	unique_ptr<SymmetricKey> cryptoKey;
	unique_ptr<InitializationVector> cryptoIv;
	if (getKeyIvForDecryption (pluginConfig, errorKey, k, cryptoKey, cryptoIv) != 1)
	{
		return -1;
	}

	// parse salt length from crypto payload
	kdb_unsigned_long_t saltLen = 0;
	if (CRYPTO_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, NULL, &saltLen) != 1)
	{
		return -1; // error set by CRYPTO_PLUGIN_FUNCTION(getSaltFromPayload)()
	}
	saltLen += sizeof (kdb_unsigned_long_t);

	// set payload pointer
	const byte * payload = reinterpret_cast<const byte *> (keyValue (k)) + saltLen;
	const size_t payloadLen = keyGetValueSize (k) - saltLen;

	try
	{
		// setup pipe and crypto filter
		Pipe decryptor (get_cipher (ELEKTRA_CRYPTO_BOTAN_ALGORITHM, *cryptoKey, *cryptoIv, DECRYPTION));
		kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;

		// decrypt the conent
		decryptor.process_msg (payload, payloadLen);

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
		return -1; // failure
	}

	return 1; // success
}

/**
 * @brief create a random sequence of characters with given length.
 * @param errorKey holds an error description in case of failure.
 * @param length the number of random bytes to be generated.
 * @returns allocated buffer holding a hex-encoded random string or NULL in case of error. Must be freed by the caller.
 */
char * elektraCryptoBotanCreateRandomString (Key * errorKey, const kdb_unsigned_short_t length)
{
	try
	{
		kdb_octet_t * buffer = new kdb_octet_t[length];
		AutoSeeded_RNG rng;
		rng.randomize (buffer, length);
		char * hexString = CRYPTO_PLUGIN_FUNCTION (bin2hex) (errorKey, buffer, length);
		delete[] buffer;
		return hexString;
	}
	catch (std::exception & e)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CRYPTO_INTERNAL_ERROR, errorKey, "Failed to generate random string because: %s",
				    e.what ());
		return 0;
	}
}

} // extern "C"
