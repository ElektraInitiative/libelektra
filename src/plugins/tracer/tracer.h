/***************************************************************************
            tracer.h  -  Skeleton of backends to access the Key Database
                             -------------------
 *  begin                : Wed 19 May, 2010
 *  copyright            : (C) 2010 by Markus Raab
 *  email                : elektra@markus-raab.org
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
 *   Simple fill the empty _tracer functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbplugin.h>

#include <stdio.h>

int elektraTracerOpen(Plugin *handle, Key *errorKey);
int elektraTracerClose(Plugin *handle, Key *errorKey);
int elektraTracerGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraTracerSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraTracerError(Plugin *handle, KeySet *returned, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(tracer);
