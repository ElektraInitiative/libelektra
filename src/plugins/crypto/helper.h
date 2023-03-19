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
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/type/types.h>

int ELEKTRA_PLUGIN_FUNCTION (getSaltFromMetakey) (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen);
int ELEKTRA_PLUGIN_FUNCTION (getSaltFromPayload) (Key * errorKey, Key * k, kdb_octet_t ** salt, kdb_unsigned_long_t * saltLen);
Key * ELEKTRA_PLUGIN_FUNCTION (getMasterPassword) (Key * errorKey, KeySet * config);
kdb_unsigned_long_t ELEKTRA_PLUGIN_FUNCTION (getIterationCount) (Key * errorKey, KeySet * config);

int ELEKTRA_PLUGIN_FUNCTION (gpgEncryptMasterPassword) (KeySet * conf, Key * errorKey, Key * msgKey);
int ELEKTRA_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (KeySet * conf, Key * errorKey, Key * msgKey);

int ELEKTRA_PLUGIN_FUNCTION (base64Encode) (Key * errorKey, const kdb_octet_t * input, const size_t inputLength, char ** output);
int ELEKTRA_PLUGIN_FUNCTION (base64Decode) (Key * errorKey, const char * input, kdb_octet_t ** output, size_t * outputLength);

#endif
