/***************************************************************************
          backend.c  -  Everything related to a backend
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
 * Works only for system-configs.
 */
static int renameBackendConfig(KeySet *config)
{
	Key *root;
	Key *cur;
	ssize_t systemSize = sizeof("system");
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
		for (ssize_t i=0; i<curSize-rootSize; ++i)
		{
			cur->key[i+systemSize] = cur->key[i+rootSize];
		}
		cur->keySize = curSize-rootSize+systemSize;
	}

	return 0;
}


/**Builds a backend out of the configuration supplied
 * from:
 *
@verbatim
system/elektra/mountpoints/<name>
@endverbatim
 *
 * The root key must be like the above example. You do
 * not need to rewind the keyset. But every key must be
 * below the root key.
 *
 * The internal consistency will be checked in this
 * function. If necessary parts are missing, like
 * no plugins, they cant be loaded or similar 0
 * will be returned.
 *
 * ksCut() is perfectly suitable for cutting out the
 * configuration like needed.
 *
 * @note The given KeySet will be deleted within the function,
 * dont use it afterwards.
 *
 * @param elektraConfig the configuration to work with.
 *        It is used to build up this backend.
 * @param modules used to load new modules or get references
 *        to existing one
 * @return a pointer to a freshly allocated backend
 * @return 0 if it did not work, the elektraConfig then
 *         has the error information.
 * @ingroup backend
 */
Backend* elektraBackendOpen(KeySet *elektraConfig, KeySet *modules, Key *errorKey)
{
	Key * cur;
	Key * root;
	KeySet *referencePlugins = 0;
	KeySet *systemConfig = 0;

	referencePlugins = ksNew(0);
	ksRewind(elektraConfig);

	root = ksNext (elektraConfig);

	Backend *backend = elektraCalloc(sizeof(struct _Backend));

	while ((cur = ksNext(elektraConfig)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			// direct below root key
			KeySet *cut = ksCut (elektraConfig, cur);
			if (!strcmp(keyBaseName(cur), "config"))
			{
				systemConfig = cut;
				renameBackendConfig (systemConfig);
			}
			else if (!strcmp(keyBaseName(cur), "getplugins"))
			{
				if (elektraProcessPlugins(backend->getplugins, modules, referencePlugins,
							cut, systemConfig, errorKey) == -1)
				{
					ELEKTRA_ADD_WARNING(13, errorKey, "elektraProcessPlugins for get failed");
					goto error;
				}
			}
			else if (!strcmp(keyBaseName(cur), "mountpoint"))
			{
				backend->mountpoint=keyNew(keyValue(cur),KEY_VALUE,keyBaseName(root), KEY_END);
				keyIncRef(backend->mountpoint);
				if (!backend->mountpoint)
				{
					ELEKTRA_ADD_WARNING(14, errorKey, keyValue(cur));
					goto error;
				}
				ksDel (cut);
			}
			else if (!strcmp(keyBaseName(cur), "setplugins"))
			{
				if (elektraProcessPlugins(backend->setplugins, modules, referencePlugins,
							cut, systemConfig, errorKey) == -1)
				{
					ELEKTRA_ADD_WARNING(15, errorKey, "elektraProcessPlugins for set failed");
					goto error;
				}
			}
			else if (!strcmp(keyBaseName(cur), "errorplugins"))
			{
				if (elektraProcessPlugins(backend->errorplugins, modules, referencePlugins,
							cut, systemConfig, errorKey) == -1)
				{
					ELEKTRA_ADD_WARNING(15, errorKey, "elektraProcessPlugins for error failed");
					goto error;
				}
			} else {
				// no one cares about that config
				ELEKTRA_ADD_WARNING(16, errorKey, keyBaseName(cur));
				ksDel (cut);
			}
		}
	}

	ksDel (systemConfig);
	ksDel (elektraConfig);
	ksDel (referencePlugins);
	return backend;

error:
	ksDel (systemConfig);
	ksDel (elektraConfig);
	ksDel (referencePlugins);
	elektraBackendClose(backend, errorKey);
	return 0;
}

/**
 * Opens a default backend using the plugin named default.
 *
 * @param modules the modules to work with
 * @errorKey the key to issue warnings and errors to
 * @return the fresh allocated default backend or 0 if it failed
 */
Backend* elektraBackendOpenDefault(KeySet *modules, Key *errorKey)
{
	Backend *backend = elektraCalloc(sizeof(struct _Backend));

	KeySet *resolverConfig = ksNew(5,
		keyNew("system/path", KEY_VALUE, "default.ecf", KEY_END),
		KS_END);

	Plugin *resolver = elektraPluginOpen("resolver", modules, resolverConfig, errorKey);
	if (!resolver)
	{
		elektraFree(backend);
		/* error already set in elektraPluginOpen */
		return 0;
	}

	backend->getplugins[RESOLVER_PLUGIN] = resolver;
	backend->setplugins[RESOLVER_PLUGIN] = resolver;
	backend->setplugins[COMMIT_PLUGIN] = resolver;
	backend->errorplugins[STORAGE_PLUGIN] = resolver;
	resolver->refcounter = 4;

	KeySet *storageConfig = ksNew(5,
		KS_END);

	Plugin *storage = elektraPluginOpen("default", modules, storageConfig, errorKey);
	if (!storage)
	{
		elektraPluginClose(resolver, errorKey);
		elektraFree(backend);
		/* error already set in elektraPluginOpen */
		return 0;
	}

	backend->getplugins[STORAGE_PLUGIN] = storage;
	backend->setplugins[STORAGE_PLUGIN] = storage;
	storage->refcounter = 2;

	Key *mp = keyNew ("", KEY_VALUE, "default", KEY_END);
	backend->mountpoint = mp;
	keyIncRef(backend->mountpoint);

	return backend;
}

Backend* elektraBackendOpenModules(KeySet *modules, Key *errorKey)
{
	Backend *backend = elektraCalloc(sizeof(struct _Backend));

	cursor_t save = ksGetCursor (modules);
	KeySet *defaultConfig = ksNew(5,
		keyNew("system/module", KEY_VALUE, "1", KEY_END),
		keyNew("user/module", KEY_VALUE, "1", KEY_END),
		KS_END);
	Key *cur = ksCurrent(modules);

	Plugin *plugin = elektraPluginOpen(keyBaseName(cur), modules, defaultConfig, errorKey);
	if (!plugin)
	{
		/* Error already set in plugin */
		elektraFree(backend);
		return 0;
	}

	Key *mp = keyNew ("system/elektra/modules", KEY_VALUE, "modules", KEY_END);
	keyAddBaseName (mp, keyBaseName(cur));

	backend->getplugins[0] = plugin;
	plugin->refcounter = 1;

	backend->mountpoint = mp;
	keyIncRef(backend->mountpoint);

	ksSetCursor (modules, save);

	return backend;
}

int elektraBackendClose(Backend *backend, Key* errorKey)
{
	int ret = 0;
	int errorOccurred = 0;

	if (!backend) return -1;

	keyDecRef(backend->mountpoint);
	keyDel (backend->mountpoint);
	for (int i=0; i<NR_OF_PLUGINS; ++i)
	{
		ret = elektraPluginClose(backend->setplugins[i], errorKey);
		if (ret == -1) ++errorOccurred;

		ret = elektraPluginClose(backend->getplugins[i], errorKey);
		if (ret == -1) ++errorOccurred;

		ret = elektraPluginClose(backend->errorplugins[i], errorKey);
		if (ret == -1) ++errorOccurred;
	}
	elektraFree (backend);

	if (errorOccurred) return -1;
	else return 0;
}
