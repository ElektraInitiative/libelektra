/***************************************************************************
                     yajl.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_YAJL_H
#define ELEKTRA_PLUGIN_YAJL_H

#include <kdbplugin.h>


int elektraYajlOpen(Plugin *handle, Key *errorKey);
int elektraYajlClose(Plugin *handle, Key *errorKey);
int elektraYajlGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraYajlSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(yajl);

#endif
