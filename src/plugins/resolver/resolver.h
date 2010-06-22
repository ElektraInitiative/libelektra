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

#include <kdbvar.h>
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

typedef struct _resolverHandle resolverHandle;

struct _resolverHandle
{
	int fd; /* Descriptor to the locking file */
	time_t mtime; /* Previous timestamp of the file */
	mode_t mode; /* The mode to set */

	int action;

	char *filename;
	char *userFilename;
	char *systemFilename;

	const char *path; /* The relative path to the filename.
		The user or system part will be prepended. */
};

int resolveFilename(Key* forKey, resolverHandle *p);

int kdbOpen_resolver(Plugin *handle, Key *errorKey);
int kdbClose_resolver(Plugin *handle, Key *errorKey);
int kdbGet_resolver(Plugin *handle, KeySet *ks, Key *parentKey);
int kdbSet_resolver(Plugin *handle, KeySet *ks, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(resolver);

int elektraWriteLock(int fd);
int elektraReadLock(int fd);
int elektraUnlock(int fd);

#endif
