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

#include <kdbplugin.h>

// meta-keys
#define ELEKTRA_GPGME_META_ENCRYPT "gpg/encrypt"

// kdb functions
int elektraGpgmeOpen (Plugin * handle, Key * errorKey);
int elektraGpgmeClose (Plugin * handle, Key * errorKey);
int elektraGpgmeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGpgmeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGpgmeCheckconf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (gpgme);

#endif
