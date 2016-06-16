/**
 * @file
 *
 * @brief cryptographic interface using the Botan library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_BOTAN_OPERATIONS_H
#define ELEKTRA_PLUGIN_BOTAN_OPERATIONS_H

#include <kdb.h>

#define ELEKTRA_CRYPTO_BOTAN_KEYSIZE (32)
#define ELEKTRA_CRYPTO_BOTAN_BLOCKSIZE (16)
#define ELEKTRA_CRYPTO_BOTAN_ALGORITHM "AES-256/CBC"

int elektraCryptoBotanInit (Key * errorKey);
int elektraCryptoBotanEncrypt (KeySet * pluginConfig, Key * k, Key * errorKey);
int elektraCryptoBotanDecrypt (KeySet * pluginConfig, Key * k, Key * errorKey);

#endif
