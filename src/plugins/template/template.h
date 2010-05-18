/***************************************************************************
            template.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _template functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbbackend.h>
#include <errno.h>


#define BACKENDNAME "template"
#define BACKENDVERSION "0.0.1"

int kdbOpen_template(KDB *handle);
int kdbClose_template(KDB *handle);
ssize_t kdbGet_template(KDB *handle, KeySet *ks, const Key *parentKey);
ssize_t kdbSet_template(KDB *handle, KeySet *ks, const Key *parentKey);
KDB *KDBEXPORT(template);
