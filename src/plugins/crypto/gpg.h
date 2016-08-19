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

#define ELEKTRA_CRYPTO_PARAM_GPG_BIN "/crypto/gpg"
#define ELEKTRA_CRYPTO_DEFAULT_GPG_BIN "/usr/bin/gpg2"
#define ELEKTRA_CRYPTO_MESSAGE_MAX_LEN (1024)

int elektraCryptoGpgEncryptMasterPassword (KeySet * conf, Key * errorKey, Key * msgKey);
int elektraCryptoGpgDecryptMasterPassword (KeySet * conf, Key * errorKey, Key * msgKey);
int elektraCryptoGpgCall (KeySet * conf, Key * errorKey, Key * msgKey, char * argv[], size_t argc);

#endif
