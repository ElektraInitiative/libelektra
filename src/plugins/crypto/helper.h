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

Key * elektraCryptoGetMasterPassword (Key * errorKey, KeySet * config);
kdb_unsigned_long_t elektraCryptoGetIterationCount (KeySet * config);
void elektraCryptoHex2Bin (Key * errorKey, const char * hexBuffer, kdb_octet_t ** output, size_t * outputLen);
char * elektraCryptoBin2Hex (Key * errorKey, const kdb_octet_t * buffer, const size_t length);

#endif
