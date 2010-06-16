/***************************************************************************
            resolver.h  -  Skeleton of backends to access the Key Database
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
 *   to provide libelektra.so a valid backend.                             *
 *   Simple fill the empty _resolver functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef PLUGIN_resolver_H
#define PLUGIN_resolver_H

#include <kdbplugin.h>
#include <kdberrors.h>

#include <stdlib.h>
#include <ctype.h>

/* Needs posix */
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/stat.h>

#define ERROR_SIZE 1024

#define BACKENDNAME "resolver"
#define BACKENDVERSION "0.0.1"

typedef struct _pluginhandle pluginhandle;

int kdbOpen_resolver(Plugin *handle);
int kdbClose_resolver(Plugin *handle);
int kdbGet_resolver(Plugin *handle, KeySet *ks, Key *parentKey);
int kdbSet_resolver(Plugin *handle, KeySet *ks, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(resolver);

/* helper.c */
int elektraWriteLock(int fd);
int elektraReadLock(int fd);
int elektraUnlock(int fd);

ssize_t elektraEncode(void *elektraDecoded, size_t size, char *returned);
ssize_t elektraDecode(char *elektraEncoded,void *returned);
int elektraEncodeChar(char c, char *buffer, size_t bufSize);
int elektraDecodeChar(const char *from, char *into);
int elektraFilenameToKeyName(const char *string, char *buffer, size_t bufSize);
ssize_t elektraGetFullKeyName (const char *forFilename, const Key *parentKey, Key *returned);
int elektraKeyNameToRelativeFilename(const char *string, char *buffer, size_t bufSize);
ssize_t elektraKeyCalcRelativeFilename(const Key *key,char *relativeFilename,size_t maxSize);
ssize_t elektraGetFullFilename(const Key *forKey, char *returned, size_t maxSize);

#endif
