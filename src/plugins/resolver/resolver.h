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


#ifndef PLUGIN_RESOLVER_H
#define PLUGIN_RESOLVER_H

#include <kdbconfig.h>
#include <kdbplugin.h>
#include <kdberrors.h>

#define ERROR_SIZE 1024

typedef struct _resolverHandle resolverHandle;

struct _resolverHandle
{
	int fd;       ///< Descriptor to the locking file
	time_t mtime; ///< Previous timestamp of the file
	mode_t mode;  ///< The mode to set

	char *dirname; ///< directory where real+temp file is
	char *filename;///< the full path to the configuration file
	char *tempfile;///< temporary file storages write to

	const char *path; ///< the configuration file name as passed from config
	const char *env;  ///< environment variables to search for files
	const char *fix;  ///< add
};

typedef struct _resolverHandles resolverHandles;

struct _resolverHandles
{
	resolverHandle user;
	resolverHandle system;
};

void resolverInit (resolverHandle *p, const char *path);
void resolverClose (resolverHandle *p);

int elektraResolveFilename(Key* forKey, resolverHandle *p, Key *warningKey);
int elektraResolverCheckFile (const char* filename);

int elektraResolverOpen(Plugin *handle, Key *errorKey);
int elektraResolverClose(Plugin *handle, Key *errorKey);
int elektraResolverGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraResolverSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraResolverError(Plugin *handle, KeySet *returned, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(resolver);

int elektraLockFile(int fd, Key *parentKey);
int elektraUnlockFile(int fd, Key *parentKey);
void elektraCloseFile(int fd, Key *parentKey);

#endif
