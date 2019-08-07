/**
 * @file
 *
 * @brief cryptographic interface using the Botan library
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BOTAN_OPERATIONS_H
#define ELEKTRA_PLUGIN_BOTAN_OPERATIONS_H

#include <elektra/kdb.h>
#include <kdbtypes.h>

#define ELEKTRA_CRYPTO_BOTAN_KEYSIZE (32)
#define ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE (16)
#define ELEKTRA_CRYPTO_BOTAN_ALGORITHM "AES-256/CBC"

char * elektraCryptoBotanCreateRandomString (Key * errorKey, const kdb_unsigned_short_t length);
int elektraCryptoBotanInit (Key * errorKey);
int elektraCryptoBotanEncrypt (KeySet * pluginConfig, Key * k, Key * errorKey, Key * masterKey);
int elektraCryptoBotanDecrypt (KeySet * pluginConfig, Key * k, Key * errorKey, Key * masterKey);

#endif
