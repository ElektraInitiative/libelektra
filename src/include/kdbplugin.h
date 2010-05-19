/***************************************************************************
                kdbplugin.h  -  Methods for plugin programing
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

/*
 * You have to include this file in order to write plugins.
 * You do not need this functions to use elektra otherwise!
 */


#ifndef KDBPLUGIN_H
#define KDBPLUGIN_H


#include <kdb.h>
#include <kdbcap.h>
#include <kdbextension.h>

#ifdef ELEKTRA_STATIC
        #define KDBEXPORT(module) libelektra_##module##_LTX_kdbBackendFactory(void)
#else
        #define KDBEXPORT(module) kdbBackendFactory(void)
#endif


typedef struct _Plugin	Plugin;

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

Plugin *pluginExport(const char *pluginName, ...);

KeySet *pluginGetConfig(Plugin *handle);

void *pluginGetData(Plugin *handle);
void pluginSetData(Plugin *handle, void *data);

int keyClearSync (Key *key);



#ifdef __cplusplus
}
}
#endif


#endif
