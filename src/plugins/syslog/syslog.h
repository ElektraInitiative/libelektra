/***************************************************************************
            syslog.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _syslog functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbplugin.h>
#include <syslog.h>


#define BACKENDNAME "syslog"
#define BACKENDVERSION "0.0.1"

int elektraSyslogOpen(Plugin *handle, Key *parentKey);
int elektraSyslogClose(Plugin *handle, Key *parentKey);
int elektraSyslogGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraSyslogSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraSyslogError(Plugin *handle, KeySet *returned, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(syslog);
