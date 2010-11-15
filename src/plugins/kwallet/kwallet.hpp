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



#include <kdbplugin.h>



extern "C" {

int elektraKwalletOpen(ckdb::Plugin *handle, ckdb::Key *error);
int elektraKwalletClose(ckdb::Plugin *handle, ckdb::Key *error);
int elektraKwalletGet(ckdb::Plugin *, ckdb::KeySet *returned, ckdb::Key
		*parentKey);
int elektraKwalletSet(ckdb::Plugin *, ckdb::KeySet *returned, ckdb::Key
		*parentKey);
ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(kwallet);

} // extern "C"
