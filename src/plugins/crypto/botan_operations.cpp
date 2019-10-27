/**
 * @file
 *
 * @brief cryptographic interface using the Botan library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <botan/auto_rng.h>
#include <botan/filters.h>
#include <botan/hmac.h>
#include <botan/init.h>
#include <botan/lookup.h>
#include <botan/pbkdf2.h>
#include <botan/pipe.h>
#include <botan/sha2_32.h>
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
#include <kdbassert.h>
#include <kdberrors.h>
#include <string.h>

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param masterKey holds the decrypted master password from the plugin configuration
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey holds a unique pointer to an allocated SymmetricKey.
 * @param cIv holds a unique pointer to an allocated InitializationVector.
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForEncryption (KeySet * config, Key * errorKey, Key * masterKey, Key * k, unique_ptr<SymmetricKey> & cKey,
				  unique_ptr<InitializationVector> & cIv)
{
	byte salt[ELEKTRA_CRYPTO_DEFAULT_SALT_LEN];
	char * saltHexString = NULL;
	const size_t requiredKeyBytes = ELEKTRA_CRYPTO_BOTAN_KEYSIZE + ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE;

	ELEKTRA_ASSERT (masterKey != NULL, "Parameter `masterKey` must not be NULL");

	try
	{
		// generate the salt
		AutoSeeded_RNG rng;
		rng.randomize (salt, sizeof (salt));
		const int encodingResult = ELEKTRA_PLUGIN_FUNCTION (base64Encode) (errorKey, salt, sizeof (salt), &saltHexString);
		if (encodingResult < 0)
		{
			// error in libinvoke - errorKey has been set by base64Encode
			return -1;
		}
		if (!saltHexString)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey, "Memory allocation failed");
			return -1;
		}
		keySetMeta (k, ELEKTRA_CRYPTO_META_SALT, saltHexString);
		elektraFree (saltHexString);

		// read iteration count
		const kdb_unsigned_long_t iterations = ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

		// generate/derive the cryptographic key and the IV
		PKCS5_PBKDF2 pbkdf (new HMAC (new SHA_256));
		OctetString derived = pbkdf.derive_key (
			requiredKeyBytes, std::string (reinterpret_cast<const char *> (keyValue (masterKey)), keyGetValueSize (masterKey)),
			salt, sizeof (salt), iterations);

		cKey = unique_ptr<SymmetricKey> (new SymmetricKey (derived.begin (), ELEKTRA_CRYPTO_BOTAN_KEYSIZE));
		cIv = unique_ptr<InitializationVector> (
			new InitializationVector (derived.begin () + ELEKTRA_CRYPTO_BOTAN_KEYSIZE, ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE));

		return 1;
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Failed to create a cryptographic key for encryption. Reason: %s", e.what ());
		return -1;
	}
}

/**
 * @brief derive the cryptographic key and IV for a given (Elektra) Key k
 * @param config KeySet holding the plugin/backend configuration
 * @param errorKey holds an error description in case of failure
 * @param masterKey holds the decrypted master password from the plugin configuration
 * @param k the (Elektra)-Key to be encrypted
 * @param cKey holds a unique pointer to an allocated SymmetricKey.
 * @param cIv holds a unique pointer to an allocated InitializationVector.
 * @retval -1 on failure. errorKey holds the error description.
 * @retval 1 on success
 */
static int getKeyIvForDecryption (KeySet * config, Key * errorKey, Key * masterKey, Key * k, unique_ptr<SymmetricKey> & cKey,
				  unique_ptr<InitializationVector> & cIv)
{
	const size_t requiredKeyBytes = ELEKTRA_CRYPTO_BOTAN_KEYSIZE + ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE;
	kdb_octet_t * saltBuffer;
	kdb_unsigned_long_t saltBufferLen = 0;

	ELEKTRA_ASSERT (masterKey != NULL, "Parameter `masterKey` must not be NULL");

	// get the salt
	if (ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, &saltBuffer, &saltBufferLen) != 1)
	{
		return -1; // error set by ELEKTRA_PLUGIN_FUNCTION(getSaltFromPayload)()
	}

	// get the iteration count
	const kdb_unsigned_long_t iterations = ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (errorKey, config);

	try
	{
		// derive the cryptographic key and the IV
		PKCS5_PBKDF2 pbkdf (new HMAC (new SHA_256));
		OctetString derived = pbkdf.derive_key (
			requiredKeyBytes, std::string (reinterpret_cast<const char *> (keyValue (masterKey)), keyGetValueSize (masterKey)),
			saltBuffer, saltBufferLen, iterations);

		cKey = unique_ptr<SymmetricKey> (new SymmetricKey (derived.begin (), ELEKTRA_CRYPTO_BOTAN_KEYSIZE));
		cIv = unique_ptr<InitializationVector> (
			new InitializationVector (derived.begin () + ELEKTRA_CRYPTO_BOTAN_KEYSIZE, ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE));

		return 1;
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Failed to restore the cryptographic key for decryption. Reason: %s", e.what ());
		return -1;
	}
}

int elektraCryptoBotanInit (Key * errorKey)
{
	try
	{
		LibraryInitializer::initialize ();
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_INSTALLATION_ERRORF (errorKey, "Botan initialization failed. Reason: %s", e.what ());
		return -1; // failure
	}
	return 1; // success
}

int elektraCryptoBotanEncrypt (KeySet * pluginConfig, Key * k, Key * errorKey, Key * masterKey)
{
	// get cryptographic material
	unique_ptr<SymmetricKey> cryptoKey;
	unique_ptr<InitializationVector> cryptoIv;
	if (getKeyIvForEncryption (pluginConfig, errorKey, masterKey, k, cryptoKey, cryptoIv) != 1)
	{
		return -1;
	}

	// prepare the salt for payload output
	kdb_unsigned_long_t saltLen = 0;
	kdb_octet_t * salt = NULL;

	if (ELEKTRA_PLUGIN_FUNCTION (getSaltFromMetakey) (errorKey, k, &salt, &saltLen) != 1)
	{
		return -1; // error set by ELEKTRA_PLUGIN_FUNCTION(getSaltFromMetakey)()
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
			auto buffer = unique_ptr<byte[]>{
				new byte[msgLength + ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN + sizeof (kdb_unsigned_long_t) + saltLen]
			};
			size_t bufferIndex = 0;

			memcpy (&buffer[bufferIndex], ELEKTRA_CRYPTO_MAGIC_NUMBER, ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN);
			bufferIndex += ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;
			memcpy (&buffer[bufferIndex], &saltLen, sizeof (kdb_unsigned_long_t));
			bufferIndex += sizeof (kdb_unsigned_long_t);
			memcpy (&buffer[bufferIndex], salt, saltLen);
			bufferIndex += saltLen;

			const size_t buffered = encryptor.read (&buffer[bufferIndex], msgLength);
			keySetBinary (k, &buffer[0], buffered + bufferIndex);
		}
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Encryption failed. Reason: %s", e.what ());
		elektraFree (salt);
		return -1; // failure
	}

	elektraFree (salt);
	return 1; // success
}

int elektraCryptoBotanDecrypt (KeySet * pluginConfig, Key * k, Key * errorKey, Key * masterKey)
{
	// get cryptographic material
	unique_ptr<SymmetricKey> cryptoKey;
	unique_ptr<InitializationVector> cryptoIv;
	if (getKeyIvForDecryption (pluginConfig, errorKey, masterKey, k, cryptoKey, cryptoIv) != 1)
	{
		return -1;
	}

	// parse salt length from crypto payload
	kdb_unsigned_long_t saltLen = 0;
	if (ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (errorKey, k, NULL, &saltLen) != 1)
	{
		return -1; // error set by ELEKTRA_PLUGIN_FUNCTION(getSaltFromPayload)()
	}
	saltLen += sizeof (kdb_unsigned_long_t);

	// set payload pointer
	const byte * payload = reinterpret_cast<const byte *> (keyValue (k)) + saltLen + ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;
	const size_t payloadLen = keyGetValueSize (k) - saltLen - ELEKTRA_CRYPTO_MAGIC_NUMBER_LEN;

	try
	{
		// setup pipe and crypto filter
		Pipe decryptor (get_cipher (ELEKTRA_CRYPTO_BOTAN_ALGORITHM, *cryptoKey, *cryptoIv, DECRYPTION));
		kdb_octet_t flags = ELEKTRA_CRYPTO_FLAG_NONE;

		// decrypt the content
		decryptor.process_msg (payload, payloadLen);

		if (decryptor.remaining () > 0)
		{
			// decode the "header" flags
			const size_t flagLength = decryptor.read (static_cast<byte *> (&flags), sizeof (kdb_octet_t));
			if (flagLength != sizeof (kdb_octet_t))
			{
				throw std::length_error ("Failed to restore the original data type of the value.");
			}
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
				auto buffer = unique_ptr<byte[]>{ new byte[msgLength] };
				const size_t buffered = decryptor.read (&buffer[0], msgLength);
				keySetBinary (k, &buffer[0], buffered);
			}
		}
		else
		{
			keySetBinary (k, NULL, 0);
		}
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Decryption failed. Reason: %s", e.what ());
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
		char * hexString = NULL;
		auto buffer = unique_ptr<kdb_octet_t[]>{ new kdb_octet_t[length] };
		AutoSeeded_RNG rng;
		rng.randomize (&buffer[0], length);
		if (ELEKTRA_PLUGIN_FUNCTION (base64Encode) (errorKey, &buffer[0], length, &hexString) < 0)
		{
			// error in libinvoke - errorKey has been set by base64Encode
			return 0;
		}
		return hexString;
	}
	catch (std::exception const & e)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "Failed to generate random string. Reason: %s", e.what ());
		return 0;
	}
}

} // extern "C"
