/***************************************************************************
          crypto.h  -  Cryptographic filter plugin
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
 
#ifndef ELEKTRA_PLUGIN_CRYPTO_H
#define ELEKTRA_PLUGIN_CRYPTO_H

#include <kdbplugin.h>
#include <stdio.h>

int elektraCryptoOpen(Plugin *handle, Key *errorKey);
int elektraCryptoClose(Plugin *handle, Key *errorKey);
int elektraCryptoGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCryptoError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(crypto);

#endif
