/**
* @file
*
* @brief cryptographic interface using the gcrypt library
*
* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
*
*/

#ifndef ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H
#define ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H

#include <kdb.h>

// TODO adapt to Elektra's error handling facility
#define ELEKTRA_CRYPTO_GCRY_OK (1)
#define ELEKTRA_CRYPTO_GCRY_NOK (0)

#define ELEKTRA_CRYPTO_GCRY_KEYSIZE (32)
#define ELEKTRA_CRYPTO_GCRY_BLOCKSIZE (16)

void elektraCryptoGcryClearKeyIv();

int elektraCryptoGcryInit();
int elektraCryptoGcrySetKeyIv(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen);
int elektraCryptoGcryEncrypt(Key *k);
int elektraCryptoGcryDecrypt(Key *k);

#endif
