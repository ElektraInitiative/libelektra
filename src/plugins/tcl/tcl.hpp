/***************************************************************************
                     tcl.c  -  Skeleton of a plugin
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


#ifndef ELEKTRA_PLUGIN_TCL_H
#define ELEKTRA_PLUGIN_TCL_H

#include <kdbplugin.h>

#include <keyset.hpp>

#include <ostream>

namespace elektra
{

void serialize(std::ostream& os, kdb::KeySet & output);
void unserialize(std::istream& os, kdb::KeySet & output);

}

extern "C"
{

int elektraTclGet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraTclSet(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);
int elektraTclError(ckdb::Plugin *handle, ckdb::KeySet *ks, ckdb::Key *parentKey);

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(tcl);

} // end extern "C"

#endif
