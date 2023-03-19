/**
 * @file
 *
 * @brief module for calling the GPG binary
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_GPG_H
#define ELEKTRA_PLUGIN_CRYPTO_GPG_H

#include "crypto.h"
#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/type/types.h>

#define ELEKTRA_SIGNATURE_KEY "/sign/key"
#define ELEKTRA_RECIPIENT_KEY "/encrypt/key"

#define ELEKTRA_CRYPTO_PARAM_GPG_BIN "/gpg/bin"
#define ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST "/gpg/unit_test"
#define ELEKTRA_CRYPTO_DEFAULT_GPG2_BIN "/usr/bin/gpg2"
#define ELEKTRA_CRYPTO_DEFAULT_GPG1_BIN "/usr/bin/gpg"

int ELEKTRA_PLUGIN_FUNCTION (gpgCall) (KeySet * conf, Key * errorKey, Key * msgKey, char * argv[], size_t argc);
char * ELEKTRA_PLUGIN_FUNCTION (getMissingGpgKeyErrorText) (KeySet * conf);
int ELEKTRA_PLUGIN_FUNCTION (gpgVerifyGpgKeysInConfig) (KeySet * conf, Key * errorKey);
int ELEKTRA_PLUGIN_FUNCTION (gpgGetBinary) (char ** gpgBin, KeySet * conf, Key * errorKey);

#endif
