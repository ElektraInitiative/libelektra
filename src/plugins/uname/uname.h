/***************************************************************************
            uname.c  -  Access the /etc/uname file
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Markus Raab
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
 *   This is a backend that takes /etc/uname file as its backend storage.  *
 *   The kdbGet() method will parse /etc/uname and generate a              *
 *   valid key tree. The kdbSet() method will take a KeySet with valid     *
 *   filesystem keys and print an equivalent regular uname in stdout.      *
 *                                                                         *
 ***************************************************************************/

#ifndef UNAME_H
#define UNAME_H

#include <kdbplugin.h>
#include <kdbextension.h>

int elektraUnameGet(Plugin *handle, KeySet *returned, Key *parentKey);
int elektraUnameSet(Plugin *handle, KeySet *ks, Key *parentKey);

#endif
