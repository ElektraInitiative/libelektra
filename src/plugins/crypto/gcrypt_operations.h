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

#include <kdb.h>
#include <kdbtypes.h>

// gcrypt specific declarations
#include <gcrypt.h>
typedef gcry_cipher_hd_t elektraCryptoHandle;

#define ELEKTRA_CRYPTO_GCRY_KEYSIZE (32)
#define ELEKTRA_CRYPTO_GCRY_BLOCKSIZE (16)

char * elektraCryptoGcryCreateRandomString (ElektraKey * errorKey, const kdb_unsigned_short_t length);
int elektraCryptoGcryInit (ElektraKey * errorKey);
int elektraCryptoGcryHandleCreate (elektraCryptoHandle ** handle, ElektraKeyset * config, ElektraKey * errorKey, ElektraKey * masterKey, ElektraKey * k,
				   const enum ElektraCryptoOperation op);
void elektraCryptoGcryHandleDestroy (elektraCryptoHandle * handle);
int elektraCryptoGcryEncrypt (elektraCryptoHandle * handle, ElektraKey * k, ElektraKey * errorKey);
int elektraCryptoGcryDecrypt (elektraCryptoHandle * handle, ElektraKey * k, ElektraKey * errorKey);

#endif
