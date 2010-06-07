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

#ifdef ELEKTRA_STATIC
        #define ELEKTRA_PLUGIN_EXPORT(module) libelektra_##module##_LTX_kdbPluginFactory(void)
#else
        #define ELEKTRA_PLUGIN_EXPORT(module) kdbPluginFactory(void)
#endif

/**
 * Switches to denote the backend methods. Used in calls to elektraPluginExport().
 *
 * @ingroup backend
 */
typedef enum {
	ELEKTRA_PLUGIN_OPEN=1,		/*!< Next arg is backend for kdbOpen() */
	ELEKTRA_PLUGIN_CLOSE=1<<1,	/*!< Next arg is backend for kdbClose() */
	ELEKTRA_PLUGIN_GET=1<<2,	/*!< Next arg is backend for kdbGet() */
	ELEKTRA_PLUGIN_SET=1<<3,	/*!< Next arg is backend for kdbSet() */
	ELEKTRA_PLUGIN_VERSION=1<<4,	/*!< Next arg is char * for Version */
	ELEKTRA_PLUGIN_DESCRIPTION=1<<5,/*!< Next arg is char * for Description */
	ELEKTRA_PLUGIN_AUTHOR=1<<6,	/*!< Next arg is char * for Author*/
	ELEKTRA_PLUGIN_LICENCE=1<<7,	/*!< Next arg is char * for Licence*/
	ELEKTRA_PLUGIN_PROVIDES=1<<8,	/*!< Next arg is char * for Licence*/
	ELEKTRA_PLUGIN_NEEDS=1<<9,		/*!< Next arg is char * for Licence*/
	ELEKTRA_PLUGIN_END=0		/*!< End of arguments */
} plugin_t;


#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

typedef struct _Plugin	Plugin;

Plugin *elektraPluginExport(const char *pluginName, ...);

KeySet *elektraPluginGetConfig(Plugin *handle);
void elektraPluginSetHandle(Plugin *plugin, void *handle);
void* elektraPluginGetHandle(Plugin *plugin);

// needed for storage plugins?
int keyClearSync (Key *key);



#ifdef __cplusplus
}
}
#endif


#endif
