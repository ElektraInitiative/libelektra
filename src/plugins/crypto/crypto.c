/***************************************************************************
          crypto.c  -  Cryptographic filter plugin
                             -------------------
    begin                : Don Aug 20 11:28:43 CEST 2015
    copyright            : (C) 2015 by Peter Nirschl
    email                : peter.nirschl@gmail.com
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include "crypto.h"
#include <gcrypt.h>

#define ELEKTRA_PLUGIN_CRYPTO_SUCCESS (1)
#define ELEKTRA_PLUGIN_CRYPTO_ERROR (-1)

int initializeLibgcrypt();

int initializeLibgcrypt()
{
	// libgcrypt initialization
	if(!gcry_check_version (GCRYPT_VERSION))
	{
		// TODO proper error handling
		return ELEKTRA_PLUGIN_CRYPTO_ERROR;
	}
	gcry_control (GCRYCTL_SUSPEND_SECMEM_WARN);
	// allocate 16kB of secure memory
	gcry_control (GCRYCTL_INIT_SECMEM, 16384, 0);
	gcry_control (GCRYCTL_RESUME_SECMEM_WARN);
	gcry_control(GCRYCTL_INITIALIZATION_FINISHED, 0);
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoOpen(Plugin *handle, Key *errorKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoClose(Plugin *handle, Key *errorKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey)
{
	return ELEKTRA_PLUGIN_CRYPTO_SUCCESS;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto)
{
	return elektraPluginExport("crypto",
		ELEKTRA_PLUGIN_GET,	&elektraCryptoGet,
		ELEKTRA_PLUGIN_SET,	&elektraCryptoSet,
		ELEKTRA_PLUGIN_END);
}

