/***************************************************************************
 gcrypt_operations.h  -  gcrypt cryptography interface
 -------------------
 begin                : Mon Aug 31 18:04:14 CEST 2015
 copyright            : (C) 2015 by Peter Nirschl
 email                : peter.nirschl@gmail.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#ifndef ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H
#define ELEKTRA_PLUGIN_GCRYPT_OPERATIONS_H

// TODO adapt to Elektra's error handling facility
#define ELEKTRA_CRYPTO_GCRY_OK (1)
#define ELEKTRA_CRYPTO_GCRY_NOK (0)

#define ELEKTRA_CRYPTO_GCRY_KEYSIZE (32)
#define ELEKTRA_CRYPTO_GCRY_BLOCKSIZE (16)

int elektraCryptoGcryInit();
int elektraCryptoGcrySetKeyIv(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen);

#endif
