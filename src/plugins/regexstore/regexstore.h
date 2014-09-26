/***************************************************************************
                     regexstore.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_REGEXSTORE_H
#define ELEKTRA_PLUGIN_REGEXSTORE_H

#include <kdbplugin.h>


int elektraRegexstoreOpen(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraRegexstoreClose(ckdb::Plugin *handle, ckdb::Key *errorKey);
int elektraRegexstoreGet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraRegexstoreSet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

#endif
