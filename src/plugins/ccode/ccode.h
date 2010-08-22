/***************************************************************************
                     ccode.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_CCODE_H
#define ELEKTRA_PLUGIN_CCODE_H

#include <kdbplugin.h>

typedef struct
{
	char encode [256];
	char decode [256];

	char escape;

	char *buf;
	size_t bufalloc;
} CCodeData;

ssize_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);

void elektraCcodeEncode (Key *cur, CCodeData *h);
void elektraCcodeDecode (Key *cur, CCodeData *h);

int elektraCcodeOpen(Plugin *handle, Key *k);
int elektraCcodeClose(Plugin *handle, Key *k);
int elektraCcodeGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCcodeSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(ccode);

#endif
