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


#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
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
#include <kdbversion.h>
#include <kdberrors.h>


/**
 * Takes the first key and cuts off this common part
 * for all other keys.
 *
 * The first key is removed.
 *
 * Will convert to a user-config.
 */
static int elektraRenamePluginConfig(KeySet *config)
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
 * @returns 1 and an allocated string of the pluginName if a new plugins should be created.
 * @returns 2 and an allocated string of the referenceName if an old plugin should be used
 * @returns 3 and both if a new plugin should be created and made available for later
 *          back referencing.
 * @returns -1 on error
 */
int elektraProcessPlugin(Key *cur, int *pluginNumber, char **pluginName, char **referenceName, Key *errorKey)
{
	const char *fullname = keyBaseName(cur);
	size_t fullsize = keyGetBaseNameSize(cur);

	if (fullname[0] != '#')
	{
		ELEKTRA_ADD_WARNING(18, errorKey, fullname);
		return -1;
	}
	if (fullname[1] < '0' || fullname[1] > '9')
	{
		ELEKTRA_ADD_WARNING(19, errorKey, fullname);
		return -1;
	}
	*pluginNumber = fullname[1]-'0';
	if (*pluginNumber > NR_OF_PLUGINS)
	{
		ELEKTRA_ADD_WARNING(20, errorKey, fullname);
		return -1;
	}

	if (fullname[2] == '#')
	{
		char prefixReferenceName[] = "system/elektra/plugins/";

		/* We have a back reference here */
		if (fullname[fullsize-2] == '#')
		{
			const char *iter = &fullname[3];
			size_t pluginNameSize = 1; /* For null character */
			size_t referenceNameSize = 0;
			/* We will introduce a new plugin */
			while (*iter != '#')
			{
				++iter;
				++pluginNameSize;
			}

			*pluginName = elektraMalloc (pluginNameSize);
			strncpy (*pluginName, &fullname[3], pluginNameSize);
			(*pluginName)[pluginNameSize-1] = 0;

			referenceNameSize = fullsize - pluginNameSize - 4;
			++iter; /* advance to one after hash */
			*referenceName = elektraMalloc(referenceNameSize + sizeof (prefixReferenceName));
			strncpy (*referenceName, prefixReferenceName, sizeof (prefixReferenceName));
			strncat (*referenceName, iter, referenceNameSize);
			(*referenceName)[referenceNameSize + sizeof (prefixReferenceName)-2] = 0;

			return 3;
		} else {
			/* We reference back to a plugin */

			*referenceName = elektraMalloc (fullsize-3 + sizeof (prefixReferenceName)-1);
			strncpy (*referenceName, prefixReferenceName, sizeof (prefixReferenceName));
			strncat (*referenceName, &fullname[3], fullsize-3);

			return 2;
		}
	} else {
		*pluginName = elektraMalloc (fullsize-2); /* don't alloc for #n */
		strncpy (*pluginName, &fullname[2],fullsize-2);

		return 1;
	}

	/* Should not be reached */
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
int elektraProcessPlugins(Plugin **plugins, KeySet *modules, KeySet *referencePlugins, KeySet *config, KeySet *systemConfig, Key *errorKey)
{
	Key *root;
	Key *cur;

	ksRewind (config);

	root = ksNext(config);

	while ((cur = ksNext(config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			char *pluginName = 0;
			char *referenceName = 0;
			int pluginNumber;

			KeySet *pluginConfig;
			Key *key;

			if (elektraProcessPlugin(cur, &pluginNumber, &pluginName, &referenceName, errorKey) == -1)
			{
				free (pluginName);
				free (referenceName);
				ksDel (config);
				return -1;
			}

			if (pluginName)
			{
				key = keyDup (cur);
				keyAddBaseName(key, "config");
				pluginConfig = ksCut (config, key);
				keyDel (key);

				elektraRenamePluginConfig(pluginConfig);
				ksAppend(pluginConfig, systemConfig);
				ksRewind(pluginConfig); /* TODO: bug ksAppend invalidates cursor */

				/* case 1, we create a new plugin,
				   note that errorKey is not passed here, because it would set error information
				   but we only want a warning instead. */
				plugins[pluginNumber] = elektraPluginOpen(pluginName, modules, pluginConfig, 0);
				if (!plugins[pluginNumber])
				{
					ELEKTRA_ADD_WARNING (64, errorKey, pluginName);
					/* Loading plugin did not work */
					free (pluginName);
					free (referenceName);
					ksDel (config);
					return -1;
				}

				/* case 2, we label it for later use */
				if (referenceName) ksAppendKey (referencePlugins,
						keyNew(referenceName,
							KEY_BINARY,
							KEY_SIZE, sizeof (plugins[pluginNumber]),
							KEY_VALUE, &plugins[pluginNumber],
							KEY_END));
			} else {
				/* case 3, we use an existing plugin */
				Key *lookup = ksLookup(referencePlugins, keyNew(referenceName, KEY_END), KDB_O_DEL);
				if (!lookup)
				{
					ELEKTRA_ADD_WARNING (65, errorKey, referenceName);
					/* Getting a reference plugin at a previous stage did not work.
					Note that this check is necessary, because loading the plugin could
					fail for example at errorplugins and at a later point, for example
					at setplugins it is tried to refer to that.*/
					free (referenceName);
					ksDel (config);
					return -1;
				}
				plugins[pluginNumber] = *(Plugin**)keyValue(lookup);
				++plugins[pluginNumber]->refcounter;
			}
			free (pluginName);
			free (referenceName);
		} else {
			ELEKTRA_ADD_WARNING(21, errorKey, keyString(cur));
		}
	}

	ksDel (config);
	return 0;
}

/**
 * Opens a plugin.
 *
 * The config will be used as is. So be sure to transfer ownership
 * of the config to it, with e.g. ksDup().
 * elektraPluginClose() will delete the config.
 *
 * @return a pointer to a new created plugin or 0 on error
 */
Plugin* elektraPluginOpen(const char *name, KeySet *modules, KeySet *config, Key *errorKey)
{
	Plugin* handle;
	const char* n;

	elektraPluginFactory pluginFactory=0;

	if (!name || name[0] == '\0')
	{
		ELEKTRA_SET_ERROR(39, errorKey, "name is null or empty");
		goto err_clup;
	}

	n = name;
	while (*n != '\0')
	{
		if (*n == '/') ++n;
		else break;
	}

	if (*n == '\0')
	{
		ELEKTRA_SET_ERROR(39, errorKey, "name contained slashes only");
		goto err_clup;
	}

	pluginFactory = elektraModulesLoad(modules, name, errorKey);
	if (pluginFactory == 0)
	{
		/* error already set by elektraModulesLoad */
		goto err_clup;
	}

	handle = pluginFactory();
	if (handle == 0)
	{
		ELEKTRA_SET_ERROR(6, errorKey, name);
		goto err_clup;
	}

	/* init reference counting */
	handle->refcounter = 1;
	handle->config = config;

	/* let the plugin initialize itself */
	if (handle->kdbOpen)
	{
		if ((handle->kdbOpen(handle, errorKey)) == -1)
		{
			ELEKTRA_ADD_WARNING(11, errorKey, name);
			goto err_clup;
		}
	}

#if DEBUG && VERBOSE
	printf("Finished loading plugin %s\n", name);
#endif
	return handle;

err_clup:
#if DEBUG
	printf("Failed to load plugin %s\n", name);
#endif
	ksDel (config);
	return 0;
}

int elektraPluginClose(Plugin *handle, Key *errorKey)
{
	int rc=0;

	if (!handle) return 0;

	--handle->refcounter;

	/* Check if we have the last reference on the plugin (unsigned!) */
	if (handle->refcounter > 0) return 0;

	if (handle->kdbClose)
	{
		rc=handle->kdbClose(handle, errorKey);
		if (rc == -1) ELEKTRA_ADD_WARNING(12, errorKey, "kdbClose() failed");
	}

	ksDel(handle->config);
	free(handle);

	return rc;
}

static int elektraMissingGet (Plugin *plugin ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *error)
{
	ELEKTRA_SET_ERROR(62, error, keyName(error));
	return -1;
}

static int elektraMissingSet (Plugin *plugin ELEKTRA_UNUSED, KeySet *ks ELEKTRA_UNUSED, Key *error)
{
	ELEKTRA_SET_ERROR(63, error, keyName(error));
	return -1;
}


Plugin *elektraPluginMissing(void)
{
	Plugin *returned;

	returned=elektraCalloc(sizeof(struct _Plugin));
	if (!returned) return 0;

	returned->name = "missing";
	returned->kdbGet=elektraMissingGet;
	returned->kdbSet=elektraMissingSet;
	return returned;
}

static int elektraVersionGet (Plugin *handle ELEKTRA_UNUSED,
		KeySet *returned, Key *error ELEKTRA_UNUSED)
{
	KeySet *info = elektraVersionKeySet();
	ksAppend(returned, info);
	ksDel (info);
	return 1;
}


static int elektraVersionSet (Plugin *handle ELEKTRA_UNUSED,
		KeySet *returned ELEKTRA_UNUSED, Key *error)
{
	ELEKTRA_SET_ERROR(84, error, keyName(error));

	return -1;
}

Plugin *elektraPluginVersion(void)
{
	Plugin *returned;

	returned=elektraCalloc(sizeof(struct _Plugin));
	if (!returned) return 0;

	returned->name = "version";
	returned->kdbGet=elektraVersionGet;
	returned->kdbSet=elektraVersionSet;
	return returned;
}



/**
 * This function must be called by a plugin's elektraPluginSymbol() to
 * define the plugin's methods that will be exported.
 *
 * See ELEKTRA_PLUGIN_EXPORT() how to use it for plugins.
 *
 * The order and number of arguments are flexible (as in keyNew() and ksNew()) to let
 * libelektra.so evolve without breaking its ABI compatibility with plugins.
 * So for each method a plugin must export, there is a flag defined by
 * #plugin_t.
 Each flag tells kdbPluginExport() which method comes
 * next. A plugin can have no implementation for a few methods that have
 * default inefficient high-level implementations and to use these defaults, simply
 * don't pass anything to kdbPluginExport() about them.
 *
 * @param pluginName a simple name for this plugin
 * @return an object that contains all plugin informations needed by
 * 	libelektra.so
 * @ingroup plugin
 */
Plugin *elektraPluginExport(const char *pluginName, ...)
{
	va_list va;
	Plugin *returned;
	plugin_t method=0;

	if (pluginName == 0) return 0;

	returned=elektraCalloc(sizeof(struct _Plugin));

	/* Start processing parameters */
	va_start(va,pluginName);
	returned->name = pluginName;

	while ((method=va_arg(va,plugin_t))) {
		switch (method) {
			case ELEKTRA_PLUGIN_OPEN:
				returned->kdbOpen=va_arg(va,kdbOpenPtr);
				break;
			case ELEKTRA_PLUGIN_CLOSE:
				returned->kdbClose=va_arg(va,kdbClosePtr);
				break;
			case ELEKTRA_PLUGIN_GET:
				returned->kdbGet=va_arg(va,kdbGetPtr);
				break;
			case ELEKTRA_PLUGIN_SET:
				returned->kdbSet=va_arg(va,kdbSetPtr);
				break;
			case ELEKTRA_PLUGIN_ERROR:
				returned->kdbError=va_arg(va,kdbErrorPtr);
				break;
			default:
#if DEBUG
				printf ("plugin passed something unexpected\n");
#endif
				// fallthrough, will end here
			case ELEKTRA_PLUGIN_END:
				va_end(va);
				return returned;
		}
	}
	return returned;
}



/**
 * Returns the configuration of that plugin.
 *
 * @param handle a pointer to the plugin
 * @ingroup plugin
 * @return keyset to the configuration for that plugin
 */
KeySet *elektraPluginGetConfig(Plugin *handle)
{
	return handle->config;
}

/**
 * Store a pointer to any plugin related data.
 *
 * @param plugin a pointer to the plugin
 * @param data the pointer to the data
 * @ingroup plugin
 */
void elektraPluginSetData(Plugin *plugin, void *data)
{
	plugin->data = data;
}

/**
 * Get a pointer to any plugin related data stored before.
 *
 * @param plugin a pointer to the plugin
 * @return a pointer to the data
 * @ingroup plugin
 */
void* elektraPluginGetData(Plugin *plugin)
{
	return plugin->data;
}
