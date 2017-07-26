/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_H
#define ELEKTRA_PLUGIN_CRYPTO_H

#include <kdbplugin.h>
#include <stdio.h>

enum ElektraCryptoHeaderFlags
{
	ELEKTRA_CRYPTO_FLAG_NONE = 0,
	ELEKTRA_CRYPTO_FLAG_STRING = 1,
	ELEKTRA_CRYPTO_FLAG_NULL = 2
};

enum ElektraCryptoOperation
{
	ELEKTRA_CRYPTO_ENCRYPT = 0,
	ELEKTRA_CRYPTO_DECRYPT = 1
};

// plugin defaults
#define ELEKTRA_CRYPTO_DEFAULT_MASTER_PWD_LENGTH (30)
#define ELEKTRA_CRYPTO_DEFAULT_ITERATION_COUNT (15000)
#define ELEKTRA_CRYPTO_DEFAULT_SALT_LEN (17)

// plugin configuration parameters
#define ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD_LEN "/crypto/masterpasswordlength"
#define ELEKTRA_CRYPTO_PARAM_MASTER_PASSWORD "/crypto/masterpassword"
#define ELEKTRA_CRYPTO_PARAM_SHUTDOWN "/shutdown"
#define ELEKTRA_CRYPTO_PARAM_ITERATION_COUNT "/crypto/iterations"

// metakeys
#define ELEKTRA_CRYPTO_META_ENCRYPT "crypto/encrypt"
#define ELEKTRA_CRYPTO_META_SALT "crypto/salt"

#define CRYPTO_PLUGIN_FUNCTION(name) ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, name)

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)

// gcrypt specific declarations
#include <gcrypt.h>
typedef gcry_cipher_hd_t elektraCryptoHandle;

#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)

// libcrypto (OpenSSL) specific declarations
#include <openssl/evp.h>
typedef struct
{
	EVP_CIPHER_CTX * encrypt;
	EVP_CIPHER_CTX * decrypt;
} elektraCryptoHandle;

#elif defined(ELEKTRA_CRYPTO_API_BOTAN)

// Botan specific declarations
typedef void elektraCryptoHandle;

#else

typedef void elektraCryptoHandle;

#endif

// kdb functions
int CRYPTO_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey);
int CRYPTO_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey);
int CRYPTO_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int CRYPTO_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);
int CRYPTO_PLUGIN_FUNCTION (checkconf) (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (crypto);

#endif
