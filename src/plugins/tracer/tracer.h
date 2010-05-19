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


#define BACKENDNAME "tracer"
#define BACKENDVERSION "0.0.1"

int kdbOpen_tracer(Plugin *handle);
int kdbClose_tracer(Plugin *handle);
ssize_t kdbGet_tracer(Plugin *handle, KeySet *ks, const Key *parentKey);
ssize_t kdbSet_tracer(Plugin *handle, KeySet *ks, const Key *parentKey);
Plugin *KDBEXPORT(tracer);
