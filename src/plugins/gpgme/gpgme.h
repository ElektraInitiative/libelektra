/**
 * @file
 *
 * @brief filter plugin providing cryptographic operations using GPGME
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_GPGME_H
#define ELEKTRA_PLUGIN_GPGME_H

#include <elektra/kdbplugin.h>

// meta-keys
#define ELEKTRA_GPGME_META_ENCRYPT "crypto/encrypt"
#define ELEKTRA_GPGME_META_BINARY "gpg/binary"

// plugin configuration
#define ELEKTRA_SIGNATURE_KEY "/sign/key"
#define ELEKTRA_RECIPIENT_KEY "/encrypt/key"
#define ELEKTRA_GPGME_CONFIG_TEXTMODE "/gpgme/textmode"
#define ELEKTRA_GPGME_UNIT_TEST "/gpgme/unit_test"

// kdb functions
int elektraGpgmeOpen (Plugin * handle, Key * errorKey);
int elektraGpgmeClose (Plugin * handle, Key * errorKey);
int elektraGpgmeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGpgmeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGpgmeCheckconf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
