/***************************************************************************
                     hexcode.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef ELEKTRA_PLUGIN_HEXCODE_H
#define ELEKTRA_PLUGIN_HEXCODE_H

#include <kdbplugin.h>

typedef struct
{
	/* Which chars to hex-encode */
	char hd[256];

	char escape;

	char *buf;
	size_t bufalloc;
} CHexData;

ssize_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);

void elektraHexcodeEncode (Key *cur, CHexData *hd);
void elektraHexcodeDecode (Key *cur, CHexData *hd);

int elektraHexcodeGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraHexcodeSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraHexcodeOpen(Plugin *handle, Key *);
int elektraHexcodeClose(Plugin *handle, Key *k);

Plugin *ELEKTRA_PLUGIN_EXPORT(hexcode);

#endif
