/***************************************************************************
                     path.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_PATH_H
#define ELEKTRA_PLUGIN_PATH_H

#include <kdbplugin.h>
#include <kdberrors.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

int elektraPathOpen(Plugin *handle, Key *errorKey);
int elektraPathClose(Plugin *handle, Key *errorKey);
int elektraPathGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraPathSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraPathError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(path);

#define ERRORMSG_LENGTH 1000

#endif
