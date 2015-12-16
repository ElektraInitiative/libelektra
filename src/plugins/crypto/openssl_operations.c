/**
 * @file
 *
 * @brief cryptographic interface using the gcrypt library
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "crypto.h"
#include "openssl_operations.h"
#include <kdberrors.h>
#include <stdlib.h>

int elektraCryptoOpenSSLInit(Key *errorKey)
{

}

int elektraCryptoOpenSSLHandleCreate(elektraCryptoHandle **handle, KeySet *config, Key *errorKey)
{

}

void elektraCryptoOpenSSLHandleDestroy(elektraCryptoHandle *handle)
{

}

int elektraCryptoOpenSSLEncrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{

}

int elektraCryptoOpenSSLDecrypt(elektraCryptoHandle *handle, Key *k, Key *errorKey)
{

}
