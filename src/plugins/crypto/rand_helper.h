/**
 * @file
 *
 * @brief helper functions for dealing with random values
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_RAND_HELPER_H
#define ELEKTRA_PLUGIN_CRYPTO_RAND_HELPER_H

#include <kdbtypes.h>

void elektraCryptoNormalizeRandomString (kdb_octet_t * buffer, const kdb_unsigned_short_t length);

#endif
