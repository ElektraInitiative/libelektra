/***************************************************************************
            success.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _success functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#ifndef PLUGIN_SUCCESS_H
#define PLUGIN_SUCCESS_H

#include <kdbplugin.h>

int elektraSuccessCheckFile (const char* filename);

int elektraSuccessOpen(Plugin *handle, Key *errorKey);
int elektraSuccessClose(Plugin *handle, Key *errorKey);
int elektraSuccessGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraSuccessSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraSuccessError(Plugin *handle, KeySet *returned, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(success);

#endif
