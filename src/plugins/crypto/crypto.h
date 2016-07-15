/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

#define ELEKTRA_CRYPTO_PARAM_KEY_PATH ("/crypto/key")
#define ELEKTRA_CRYPTO_PARAM_IV_PATH ("/crypto/iv")
#define ELEKTRA_CRYPTO_PARAM_SHUTDOWN ("/shutdown")
#define ELEKTRA_CRYPTO_META_ENCRYPT ("crypto/encrypt")

#if defined(ELEKTRA_CRYPTO_API_GCRYPT)

// gcrypt specific declarations
#include <gcrypt.h>
typedef gcry_cipher_hd_t elektraCryptoHandle;

#define CRYPTO_PLUGIN_FUNCTION(name) ELEKTRA_PLUGIN_FUNCTION (cryptogcrypt, name)

#elif defined(ELEKTRA_CRYPTO_API_OPENSSL)

// libcrypto (OpenSSL) specific declarations
#include <openssl/evp.h>
typedef struct
{
	EVP_CIPHER_CTX encrypt;
	EVP_CIPHER_CTX decrypt;
} elektraCryptoHandle;

#define CRYPTO_PLUGIN_FUNCTION(name) ELEKTRA_PLUGIN_FUNCTION (cryptoopenssl, name)

#elif defined(ELEKTRA_CRYPTO_API_BOTAN)

typedef void elektraCryptoHandle;
#define CRYPTO_PLUGIN_FUNCTION(name) ELEKTRA_PLUGIN_FUNCTION (cryptobotan, name)

#else

typedef void elektraCryptoHandle;
#define CRYPTO_PLUGIN_FUNCTION(name) ELEKTRA_PLUGIN_FUNCTION (crypto, name)

#endif

// kdb functions
int CRYPTO_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey);
int CRYPTO_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey);
int CRYPTO_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * ks, Key * parentKey);
int CRYPTO_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey);
int CRYPTO_PLUGIN_FUNCTION (error) (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (crypto);

#endif
