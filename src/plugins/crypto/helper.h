/**
 * @file
 *
 * @brief helper functions for the crypto plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_HELPER_H
#define ELEKTRA_PLUGIN_CRYPTO_HELPER_H

#include <kdb.h>
#include <kdbtypes.h>

int elektraCryptoGetSaltFromMetaKey (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen);
int elektraCryptoGetSaltFromCryptoPayload (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen);
Key * elektraCryptoGetMasterPassword (Key * errorKey, KeySet * config);
kdb_unsigned_long_t elektraCryptoGetIterationCount (Key * errorKey, KeySet * config);
void elektraCryptoHex2Bin (Key * errorKey, const char * hexBuffer, kdb_octet_t ** output, kdb_unsigned_long_t * outputLen);
char * elektraCryptoBin2Hex (Key * errorKey, const kdb_octet_t * buffer, const size_t length);

#endif
