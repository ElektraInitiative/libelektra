/**
 * @file
 *
 * @brief cryptographic interface using the libcrypto library (part of the OpenSSL project)
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LIBCRYPTO_OPERATIONS_H
#define ELEKTRA_PLUGIN_LIBCRYPTO_OPERATIONS_H

#include <kdb.h>

#define ELEKTRA_CRYPTO_SSL_KEYSIZE (32)
#define ELEKTRA_CRYPTO_SSL_BLOCKSIZE (16)

int elektraCryptoOpenSSLInit (Key * errorKey);
int elektraCryptoOpenSSLHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey);
void elektraCryptoOpenSSLHandleDestroy (elektraCryptoHandle * handle);
int elektraCryptoOpenSSLEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);
int elektraCryptoOpenSSLDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);

#endif
