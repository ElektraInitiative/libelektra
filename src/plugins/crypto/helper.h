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

kdb_unsigned_long_t elektraCryptoGetIterationCount (KeySet * config);
kdb_octet_t * elektraCryptoHex2Bin (Key * errorKey, const char * hexBuffer);
char * elektraCryptoBin2Hex (Key * errorKey, const kdb_octet_t * buffer, const size_t length);
void elektraCryptoNormalizeRandomString (kdb_octet_t * buffer, const kdb_unsigned_short_t length);

#endif
