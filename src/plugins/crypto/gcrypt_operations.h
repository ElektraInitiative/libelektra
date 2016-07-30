/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H
#define ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H

#include <kdb.h>
#include <kdbtypes.h>

#define ELEKTRA_CRYPTO_GCRY_KEYSIZE (32)
#define ELEKTRA_CRYPTO_GCRY_BLOCKSIZE (16)

char * elektraCryptoGcryCreateRandomString (const kdb_unsigned_short_t length);
int elektraCryptoGcryInit (Key * errorKey);
int elektraCryptoGcryHandleCreate (elektraCryptoHandle ** handle, KeySet * config, Key * errorKey);
void elektraCryptoGcryHandleDestroy (elektraCryptoHandle * handle);
int elektraCryptoGcryEncrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);
int elektraCryptoGcryDecrypt (elektraCryptoHandle * handle, Key * k, Key * errorKey);

#endif
