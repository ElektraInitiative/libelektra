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

#define ELEKTRA_PLUGIN_CRYPTO_SUCCESS 1

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

