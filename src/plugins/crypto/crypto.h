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

// We may support other libraries in the future so we make the crypto-handle exchangeable
#include <gcrypt.h>
typedef gcry_cipher_hd_t elektraCryptoHandle;

int elektraCryptoOpen(Plugin *handle, Key *errorKey);
int elektraCryptoClose(Plugin *handle, Key *errorKey);
int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey);


int elektraCryptoInit();
void elektraCryptoTeardown();
elektraCryptoHandle *elektraCryptoHandleCreate(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen);
void elektraCryptoHandleDestroy(elektraCryptoHandle *handle);
int elektraCryptoEncrypt(elektraCryptoHandle *handle, Key *k);
int elektraCryptoDecrypt(elektraCryptoHandle *handle, Key *k);

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto);

#endif
