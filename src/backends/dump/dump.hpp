/***************************************************************************
            dump.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _dump functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbbackend.h>
#include <errno.h>

#include <iostream>
#include <fstream>
#include <string>

#include <cstring>

#define BACKENDNAME "dump"
#define BACKENDVERSION "0.0.1"
#define DUMP_PATH "/tmp/dump.edf"

// edf = elektra dump format

extern "C" {

int kdbOpen_dump(ckdb::KDB *handle);
int kdbClose_dump(ckdb::KDB *handle);
ssize_t kdbGet_dump(ckdb::KDB *handle, ckdb::KeySet *ks, const ckdb::Key *parentKey);
ssize_t kdbSet_dump(ckdb::KDB *handle, ckdb::KeySet *ks, const ckdb::Key *parentKey);
ckdb::KDB* KDBEXPORT(dump);

} // extern C
