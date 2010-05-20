/***************************************************************************
          plugin.c  -  Everything related to a plugin
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


#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#if DEBUG && HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#endif

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include <kdbinternal.h>


/**
 * Takes the first key and cuts off this common part
 * for all other keys.
 *
 * The first key is removed.
 *
 * Will convert to a user-config.
 */
static int renamePluginConfig(KeySet *config)
{
	Key *root;
	Key *cur;
	ssize_t userSize = sizeof("user");
	ssize_t rootSize = 0;

	ksRewind(config);

	root = ksNext (config);
	rootSize = keyGetNameSize(root);
	if (rootSize == -1) return -1;

	keyDel (ksLookup (config, root, KDB_O_POP));

	while ((cur = ksNext(config)) != 0)
	{
		ssize_t curSize = keyGetNameSize(cur);
		if (curSize == -1) return -1;
		// cant use strcpy here, because it fills up everything with 0
		strcpy (cur->key, "user/");
		for (ssize_t i=0; i<curSize-rootSize; ++i)
		{
			cur->key[i+userSize] = cur->key[i+rootSize];
		}
		cur->keySize = curSize-rootSize+userSize;
	}

	return 0;
}

/**
 * Load a plugin.
 *
 * The array of plugins must be set to 0.
 * Its length is NR_OF_PLUGINS.
 *
 * systemConfig will only be used, not deleted.
 *
 * @param config the config with the information how the
 *        plugins should be put together
 * @param systemConfig the shared (system) config for the plugins.
 *        Every plugin additional get this config.
 *
 * @return -1 on failure
 */
int processPlugins(Plugin **plugins, KeySet *config, KeySet *systemConfig)
{
	Key *root;
	Key *cur;

	ksRewind (config);

	root = ksNext(config);

	while ((cur = ksNext(config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			// this describes a plugin!
			const char *fullname = keyBaseName(cur);
			const char *pluginName = 0;
			int pluginNumber = 0;
			Key *key;
			KeySet *pluginConfig;
			if (fullname[0] != '#')
			{
				kdbPrintDebug ("Names of Plugins must start with a #\n");
				goto error;
			}
			if (fullname[1] < '0' || fullname[1] > '9')
			{
				kdbPrintDebug ("Names of Plugins must start have the position number as second char\n");
				goto error;
			}
			pluginNumber = fullname[1]-'0';
			pluginName = &fullname[2];
			if (pluginNumber > NR_OF_PLUGINS)
			{
				kdbPrintDebug("Tried to set more plugins then definied in NR_OF_PLUGINS\n");
				goto error;
			}

			key = keyDup (cur);
			keyAddBaseName(key, "config");
			pluginConfig = ksCut (config, key);
			keyDel (key);

			renamePluginConfig(pluginConfig);
			ksAppend(pluginConfig, systemConfig);
			ksRewind(pluginConfig); /* TODO: bug ksAppend invalidates cursor */

			plugins[pluginNumber] = pluginOpen(pluginName, pluginConfig);
		} else {
#if DEBUG
			printf ("Unkown additional entries in plugin\n");
#endif
		}
	}

	ksDel (config);
	return 0;

error:
	ksDel (config);
	return -1;
}

/**
 * Opens a plugin.
 *
 * The config will be used as is. So be sure to transfer ownership
 * of the config to it, with e.g. ksDup().
 * pluginClose() will delete the config.
 *
 * @return a pointer to a new created plugin or 0 on error
 */
Plugin* pluginOpen(const char *pluginname, KeySet *config)
{
	Plugin* handle;
	char* plugin_name;

	kdbLibHandle dlhandle=0;
	typedef Plugin *(*KDBPluginFactory) (void);
	KDBPluginFactory kdbPluginFactory=0;

	plugin_name = malloc(sizeof("libelektra-")+strlen(pluginname));

	strncpy(plugin_name,"libelektra-",sizeof("libelektra-"));
	strncat(plugin_name,pluginname,strlen(pluginname));

	dlhandle=kdbLibLoad(plugin_name);
	if (dlhandle == 0) {
		/*errno=KDB_ERR_EBACKEND;*/
#if DEBUG && VERBOSE
		printf("kdbLibLoad(%s) failed\n", plugin_name);
#endif
		goto err_clup; /* error */
	}

	/* load the "kdbPluginFactory" symbol from plugin */
	kdbPluginFactory=(KDBPluginFactory)kdbLibSym(dlhandle, "kdbPluginFactory");
	if (kdbPluginFactory == 0) {
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
		printf("Could not kdbLibSym kdbPluginFactory for %s\n", plugin_name);
#endif
		goto err_clup; /* error */
	}

	handle=kdbPluginFactory();
	if (handle == 0)
	{
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
		printf("Could not call kdbPluginFactory for %s\n", plugin_name);
#endif
		goto err_clup; /* error */
	}

	/* save the libloader handle for future use */
	handle->dlHandle=dlhandle;

	/* let the plugin initialize itself */
	if (handle->kdbOpen)
	{
		handle->config = config;
		if ((handle->kdbOpen(handle)) == -1)
		{
#if DEBUG && VERBOSE
			printf("kdbOpen() failed for %s\n", plugin_name);
#endif
		}
	}
	else {
		/*errno=KDB_ERR_NOSYS;*/
#if DEBUG && VERBOSE
			printf("No kdbOpen supplied in %s\n", plugin_name);
#endif
		goto err_clup;
	}

#if DEBUG && VERBOSE
	printf("Finished loading Plugin %s\n", plugin_name);
#endif
	free(plugin_name);
	return handle;

err_clup:
#if DEBUG
	printf("Failed to load plugin %s\n", plugin_name);
#endif
	ksDel (config);
	free(plugin_name);
	return 0;
}

int pluginClose(Plugin *handle)
{
	int rc=0;

	if (!handle) return 0;

	if (handle->kdbClose)
	{
		rc=handle->kdbClose(handle);
	}

	kdbLibClose(handle->dlHandle);
	ksDel(handle->config);
	free(handle);

	return rc;
}



/**
 * This function must be called by a plugin's kdbPluginFactory() to
 * define the plugin's methods that will be exported.
 *
 * See KDBEXPORT() how to use it for plugins.
 *
 * The order and number of arguments are flexible (as in keyNew() and ksNew()) to let
 * libelektra.so evolve without breaking its ABI compatibility with plugins.
 * So for each method a plugin must export, there is a flag defined by
 * #plugin_t. Each flag tells kdbPluginExport() which method comes
 * next. A plugin can have no implementation for a few methods that have
 * default inefficient high-level implementations and to use these defaults, simply
 * don't pass anything to kdbPluginExport() about them.
 *
 * @param pluginName a simple name for this plugin
 * @return an object that contains all plugin informations needed by
 * 	libelektra.so
 * @ingroup plugin
 */
Plugin *pluginExport(const char *pluginName, ...) {
	va_list va;
	Plugin *returned;
	plugin_t method=0;

	if (pluginName == 0) return 0;

	returned=kdbiCalloc(sizeof(struct _Plugin));

	/* Start processing parameters */
	
	va_start(va,pluginName);
	returned->name = pluginName;

	returned->version =
	returned->description =
	returned->author =
	returned->licence =
	returned->provides =
	returned->needs = "";

	while ((method=va_arg(va,plugin_t))) {
		switch (method) {
			case KDB_PLUGIN_OPEN:
				returned->kdbOpen=va_arg(va,kdbOpenPtr);
				break;
			case KDB_PLUGIN_CLOSE:
				returned->kdbClose=va_arg(va,kdbClosePtr);
				break;
			case KDB_PLUGIN_GET:
				returned->kdbGet=va_arg(va,kdbGetPtr);
				break;
			case KDB_PLUGIN_SET:
				returned->kdbSet=va_arg(va,kdbSetPtr);
				break;
			case KDB_PLUGIN_VERSION:
				returned->version=va_arg(va, const char *);
				break;
			case KDB_PLUGIN_DESCRIPTION:
				returned->description=va_arg(va, const char *);
				break;
			case KDB_PLUGIN_AUTHOR:
				returned->author=va_arg(va, const char *);
				break;
			case KDB_PLUGIN_LICENCE:
				returned->licence=va_arg(va, const char *);
				break;
			case KDB_PLUGIN_PROVIDES:
				returned->provides=va_arg(va, const char*);
				break;
			case KDB_PLUGIN_NEEDS:
				returned->needs=va_arg(va, const char*);
				break;
			default:
#if DEBUG
				printf ("plugin passed something unexpected\n");
#endif
				// fallthrough, will end here
			case KDB_PLUGIN_END:
				va_end(va);
				return returned;
		}
	}
	return returned;
}



/**
 * Returns the configuration of that plugin.
 *
 * @ingroup plugin
 */
KeySet *pluginGetConfig(Plugin *handle)
{
	return handle->config;
}
