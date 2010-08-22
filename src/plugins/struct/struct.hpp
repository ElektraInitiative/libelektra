/***************************************************************************
                     struct.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_STRUCT_H
#define ELEKTRA_PLUGIN_STRUCT_H

#include <kdbplugin.h>


extern "C"
{

int elektraStructOpen(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraStructClose(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraStructGet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraStructSet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraStructError(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(struct);

}

#endif
