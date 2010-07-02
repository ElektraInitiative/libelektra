/***************************************************************************
            timeofday.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _timeofday functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbplugin.h>

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>


#define BACKENDNAME "timeofday"
#define BACKENDVERSION "0.0.1"

int elektraTimeofdayOpen(Plugin *handle, Key *);
int elektraTimeofdayClose(Plugin *handle, Key *);
int elektraTimeofdayGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraTimeofdaySet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraTimeofdayError(Plugin *handle, KeySet *ks, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(timeofday);
