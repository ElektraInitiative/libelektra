/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H
#define ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H

#include <elektra/kdb.h>
#include <kdbtypes.h>

#define ELEKTRA_CRYPTO_GCRY_KEYSIZE (32)
#define ELEKTRA_CRYPTO_GCRY_BLOCKSIZE (16)

char * elektraCryptoGcryCreateRandomString (Key * errorKey, const kdb_unsigned_short_t length);
int elektraCryptoGcryInit (Key * errorKey);
int elektraCryptoGcryHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey, Key * masterKey, Key * k,
				   const enum ElektraCryptoOperation op);
void elektraCryptoGcryHandleDestroy (elektraCryptoHandle * handle);
int elektraCryptoGcryEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);
int elektraCryptoGcryDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);

#endif
