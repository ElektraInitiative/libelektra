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
#include <kdbextension.h>

#ifdef ELEKTRA_STATIC
        #define KDBEXPORT(module) libelektra_##module##_LTX_kdbPluginFactory(void)
#else
        #define KDBEXPORT(module) kdbPluginFactory(void)
#endif

/**
 * Switches to denote the backend methods. Used in calls to pluginExport().
 *
 * @ingroup backend
 */
typedef enum {
	KDB_PLUGIN_OPEN=1,		/*!< Next arg is backend for kdbOpen() */
	KDB_PLUGIN_CLOSE=1<<1,	/*!< Next arg is backend for kdbClose() */
	KDB_PLUGIN_GET=1<<2,	/*!< Next arg is backend for kdbGet() */
	KDB_PLUGIN_SET=1<<3,	/*!< Next arg is backend for kdbSet() */
	KDB_PLUGIN_VERSION=1<<4,	/*!< Next arg is char * for Version */
	KDB_PLUGIN_DESCRIPTION=1<<5,/*!< Next arg is char * for Description */
	KDB_PLUGIN_AUTHOR=1<<6,	/*!< Next arg is char * for Author*/
	KDB_PLUGIN_LICENCE=1<<7,	/*!< Next arg is char * for Licence*/
	KDB_PLUGIN_CAPABILITY=1<<8,	/*!< Next arg is char * describing which plugins
					  do not need before that plugin in the backend
					  because it is capable of handling it itself.
					  It is a simple comma separated c-string.*/
	KDB_PLUGIN_END=0		/*!< End of arguments */
} plugin_t;


typedef struct _Plugin	Plugin;

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

Plugin *pluginExport(const char *pluginName, ...);

KeySet *pluginGetConfig(Plugin *handle);

// needed for storage plugins?
int keyClearSync (Key *key);



#ifdef __cplusplus
}
}
#endif


#endif
