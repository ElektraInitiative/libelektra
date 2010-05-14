/***************************************************************************
            kwallet.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _kwallet functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbbackend.h>
#include <errno.h>


#define BACKENDNAME "kwallet"
#define BACKENDDISPLAYNAME "Elektra Kwallet Backend"
#define BACKENDVERSION "0.0.1"

extern "C" {

int kdbOpen_kwallet(ckdb::KDB *handle);
int kdbClose_kwallet(ckdb::KDB *handle);
ssize_t kdbGet_kwallet(ckdb::KDB *handle, ckdb::KeySet *ks, const ckdb::Key *parentKey);
ssize_t kdbSet_kwallet(ckdb::KDB *handle, ckdb::KeySet *ks, const ckdb::Key *parentKey);
ckdb::KDB *KDBEXPORT(kwallet);

} // extern "C"
