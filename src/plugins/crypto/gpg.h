/**
 * @file
 *
 * @brief module for calling the GPG binary
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CRYPTO_GPG_H
#define ELEKTRA_PLUGIN_CRYPTO_GPG_H

#include "crypto.h"
#include <kdb.h>
#include <kdbtypes.h>

#define ELEKTRA_CRYPTO_PARAM_GPG_BIN "/gpg/bin"
#define ELEKTRA_CRYPTO_PARAM_GPG_KEY "/gpg/key"
#define ELEKTRA_CRYPTO_PARAM_GPG_UNIT_TEST "/gpg/unit_test"
#define ELEKTRA_CRYPTO_DEFAULT_GPG2_BIN "/usr/bin/gpg2"
#define ELEKTRA_CRYPTO_DEFAULT_GPG1_BIN "/usr/bin/gpg"

int CRYPTO_PLUGIN_FUNCTION (gpgEncryptMasterPassword) (KeySet * conf, Key * errorKey, Key * msgKey);
int CRYPTO_PLUGIN_FUNCTION (gpgDecryptMasterPassword) (KeySet * conf, Key * errorKey, Key * msgKey);
int CRYPTO_PLUGIN_FUNCTION (gpgCall) (KeySet * conf, Key * errorKey, Key * msgKey, char * argv[], size_t argc);

#endif
