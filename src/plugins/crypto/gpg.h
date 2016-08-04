/**
 * @file
 *
 * @brief module for calling the GPG binary
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_GPG_H
#define ELEKTRA_PLUGIN_CRYPTO_GPG_H

#include <kdb.h>
#include <kdbtypes.h>

#define ELEKTRA_CRYPTO_DEFAULT_GPG_BIN "/usr/bin/gpg2"
#define ELEKTRA_CRYPTO_MESSAGE_MAX_LEN (1024)

int elektraCryptoGpgCall (Key * errorKey, char * argv[], size_t argc, kdb_octet_t * input, kdb_unsigned_long_t inputBufferSize,
			  kdb_octet_t * output, kdb_unsigned_long_t outputBufferSize, kdb_unsigned_long_t * outputLength);

#endif
