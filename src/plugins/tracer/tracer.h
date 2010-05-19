/***************************************************************************
            tracer.h  -  Skeleton of backends to access the Key Database
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Avi Alkalay
    email                : avi@unix.sh
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



#include <kdbbackend.h>
#include <kdbplugin.h>

#include <stdio.h>


#define BACKENDNAME "tracer"
#define BACKENDVERSION "0.0.1"

int kdbOpen_tracer(KDB *handle);
int kdbClose_tracer(KDB *handle);
ssize_t kdbGet_tracer(KDB *handle, KeySet *ks, const Key *parentKey);
ssize_t kdbSet_tracer(KDB *handle, KeySet *ks, const Key *parentKey);
KDB *KDBEXPORT(tracer);
