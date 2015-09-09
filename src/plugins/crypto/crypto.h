/**
* @file
*
* @brief filter plugin providing cryptographic operations
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef ELEKTRA_PLUGIN_CRYPTO_H
#define ELEKTRA_PLUGIN_CRYPTO_H

#include <kdbplugin.h>
#include <stdio.h>
#include "gcrypt_operations.h"

int elektraCryptoOpen(Plugin *handle, Key *errorKey);
int elektraCryptoClose(Plugin *handle, Key *errorKey);
int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto);

#endif
