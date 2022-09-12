/**
 * @file
 *
 * @brief helper functions for the crypto plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_HELPER_H
#define ELEKTRA_PLUGIN_CRYPTO_HELPER_H

#include "crypto.h"
#include <kdb.h>
#include <kdbtypes.h>

int ELEKTRA_PLUGIN_FUNCTION (getSaltFromMetakey) (ElektraKey * errorKey, ElektraKey * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen);
int ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (ElektraKey * errorKey, ElektraKey * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen);
ElektraKey * ELEKTRA_PLUGIN_FUNCTION (getMasterPassword) (ElektraKey * errorKey, ElektraKeyset * config);
kdb_unsigned_long_t ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (ElektraKey * errorKey, ElektraKeyset * config);

int ELEKTRA_PLUGIN_FUNCTION (gpgEncryptMasterPassword) (ElektraKeyset * conf, ElektraKey * errorKey, ElektraKey * msgKey);
int ELEKTRA_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (ElektraKeyset * conf, ElektraKey * errorKey, ElektraKey * msgKey);

int ELEKTRA_PLUGIN_FUNCTION (base64Encode) (ElektraKey * errorKey, const kdb_octet_t * input, const size_t inputLength, char ** output);
int ELEKTRA_PLUGIN_FUNCTION (base64Decode) (ElektraKey * errorKey, const char * input, kdb_octet_t ** output, size_t * outputLength);

#endif
