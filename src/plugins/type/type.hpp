/***************************************************************************
                     type.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_TYPE_H
#define ELEKTRA_PLUGIN_TYPE_H

#include <kdbplugin.h>


extern "C"
{

int elektraTypeOpen(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraTypeClose(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraTypeGet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraTypeSet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraTypeError(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(type);

}

#endif
