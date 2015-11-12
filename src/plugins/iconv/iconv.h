/***************************************************************************
            iconv.h  -  Skeleton of backends to access the Key Database
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
 *   Simple fill the empty _iconv functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/



#include <kdbplugin.h>
#include <kdberrors.h>

#include <iconv.h>
#include <locale.h>
#include <langinfo.h>

#include <string.h>
#include <stdlib.h>

#define UTF8_TO   1
#define UTF8_FROM 0


#define BACKENDNAME "iconv"
#define BACKENDVERSION "0.0.1"

int kdbbNeedsUTF8Conversion(Plugin *handle);
int kdbbUTF8Engine(Plugin *handle, int direction, char **string, size_t *inputOutputByteSize);

int elektraIconvGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraIconvSet(Plugin *handle, KeySet *ks, Key *parentKey);
Plugin *ELEKTRA_PLUGIN_EXPORT(iconv);
