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

#define _GNU_SOURCE // needed for recursive mutex

#include <sys/stat.h>

#include <kdbconfig.h>
#include <kdbplugin.h>
#include <kdberrors.h>

#define ERROR_SIZE 1024

typedef struct _resolverHandle resolverHandle;

struct _resolverHandle
{
	int fd;       ///< Descriptor to the locking file
	struct timespec mtime; ///< Previous timestamp of the file
	mode_t filemode;  ///< The mode to set (from previous file)
	mode_t dirmode;  ///< The mode to set for new directories
	int removalNeeded; ///< Error on freshly created files need removal

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
	resolverHandle spec;
	resolverHandle dir;
	resolverHandle user;
	resolverHandle system;
};

int ELEKTRA_PLUGIN_FUNCTION(resolver, checkFile)
	(const char* filename);
int ELEKTRA_PLUGIN_FUNCTION(resolver, filename)
	(Key* forKey, resolverHandle *p, Key *warningsKey);

int ELEKTRA_PLUGIN_FUNCTION(resolver, open)
	(Plugin *handle, Key *errorKey);
int ELEKTRA_PLUGIN_FUNCTION(resolver, close)
	(Plugin *handle, Key *errorKey);
int ELEKTRA_PLUGIN_FUNCTION(resolver, get)
	(Plugin *handle, KeySet *ks, Key *parentKey);
int ELEKTRA_PLUGIN_FUNCTION(resolver, set)
	(Plugin *handle, KeySet *ks, Key *parentKey);
int ELEKTRA_PLUGIN_FUNCTION(resolver, error)
	(Plugin *handle, KeySet *returned, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(resolver);

#endif
