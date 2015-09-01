/***************************************************************************
 gcrypt_operations.c  -  gcrypt cryptography interface
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

#include "gcrypt_operations.h"
#include <gcrypt.h>

static gcry_cipher_hd_t gcry_handle;

void elektraCryptoGcryClearKeyIv()
{
	gcry_cipher_close(gcry_handle);
}

int elektraCryptoGcryInit()
{
	if (!gcry_check_version(GCRYPT_VERSION))
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}
	gcry_control(GCRYCTL_DISABLE_SECMEM, 0);
	gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
	return ELEKTRA_CRYPTO_GCRY_OK;
}

int elektraCryptoGcrySetKeyIv(const unsigned char *key, const short keyLen, const unsigned char *iv, const short ivLen)
{
	if (gcry_cipher_open(&gcry_handle, GCRY_CIPHER_AES256, GCRY_CIPHER_MODE_CBC, 0) != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	if (gcry_cipher_setkey(gcry_handle, key, keyLen) != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}

	if (gcry_cipher_setiv(gcry_handle, iv, ivLen) != 0)
	{
		return ELEKTRA_CRYPTO_GCRY_NOK;
	}
	return ELEKTRA_CRYPTO_GCRY_OK;
}

int elektraCryptoGcryEncrypt(Key *k)
{
	return ELEKTRA_CRYPTO_GCRY_NOK;
}

int elektraCryptoGcryDecrypt(Key *k)
{
	return ELEKTRA_CRYPTO_GCRY_NOK;
}
