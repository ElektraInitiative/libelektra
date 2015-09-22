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


enum ElektraCryptoHeaderFlags
{
	ELEKTRA_CRYPTO_FLAG_NONE = 0,
	ELEKTRA_CRYPTO_FLAG_STRING = 1,
	ELEKTRA_CRYPTO_FLAG_NULL = 2
};

struct ElektraCryptoHeader
{
	unsigned short flags;
	unsigned long contentLen;
};

#define ELEKTRA_CRYPTO_META_ENCRYPT ("crypto/encrypt")
#define ELEKTRA_CRYPTO_META_ENCRYPTED ("crypto/encrypted")

// We may support other libraries in the future so we make the crypto-handle exchangeable
#include <gcrypt.h>
typedef gcry_cipher_hd_t elektraCryptoHandle;

int elektraCryptoOpen(Plugin *handle, Key *errorKey);
int elektraCryptoClose(Plugin *handle, Key *errorKey);
int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey);


int elektraCryptoInit(Key *errorKey);
void elektraCryptoTeardown();
int elektraCryptoHandleCreate(elektraCryptoHandle **handle, KeySet *config, Key *errorKey);
void elektraCryptoHandleDestroy(elektraCryptoHandle *handle);
int elektraCryptoEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey);
int elektraCryptoDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto);

#endif
