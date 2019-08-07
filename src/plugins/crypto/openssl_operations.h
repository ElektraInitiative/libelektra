/**
 * @file
 *
 * @brief cryptographic interface using the libcrypto library (part of the OpenSSL project)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LIBCRYPTO_OPERATIONS_H
#define ELEKTRA_PLUGIN_LIBCRYPTO_OPERATIONS_H

#include <elektra/kdb.h>
#include <kdbtypes.h>

#define ELEKTRA_CRYPTO_SSL_KEYSIZE (32)
#define ELEKTRA_CRYPTO_SSL_BLOCKSIZE (16)

char * elektraCryptoOpenSSLCreateRandomString (Key * errorKey, const kdb_unsigned_short_t length);
int elektraCryptoOpenSSLInit (Key * errorKey);
int elektraCryptoOpenSSLHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey, Key * masterKey, Key * k,
				      const enum ElektraCryptoOperation op);
void elektraCryptoOpenSSLHandleDestroy (elektraCryptoHandle * handle);
int elektraCryptoOpenSSLEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);
int elektraCryptoOpenSSLDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);

#endif
