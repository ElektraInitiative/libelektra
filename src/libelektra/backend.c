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
 * don't use it afterwards.
 *
 * @param elektraConfig the configuration to work with.
 *        It is used to build up this backend.
 * @param modules used to load new modules or get references
 *        to existing one
 * @return a pointer to a freshly allocated backend
 *         this could be the requested backend or a so called
 *         "missing backend".
 * @return 0 if out of memory
 * @ingroup backend
 */
Backend* elektraBackendOpen(KeySet *elektraConfig, KeySet *modules, Key *errorKey)
{
	Key * cur;
	Key * root;
	KeySet *referencePlugins = 0;
	KeySet *systemConfig = 0;
	int failure = 0;

	referencePlugins = ksNew(0);
	ksRewind(elektraConfig);

	root = ksNext (elektraConfig);

	Backend *backend = elektraCalloc(sizeof(struct _Backend));
	backend->refcounter = 1;

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
					failure = 1;
				}
			}
			else if (!strcmp(keyBaseName(cur), "mountpoint"))
			{
				if (keyString(cur)[0] == '/')
				{
					backend->mountpoint = keyNew("",
							KEY_VALUE, keyBaseName(root), KEY_END);
					backend->mountpoint->key = elektraStrDup(keyString(cur));
					backend->mountpoint->keySize = cur->dataSize;
				} else {
					backend->mountpoint = keyNew(keyString(cur),
							KEY_VALUE, keyBaseName(root), KEY_END);
				}

				if (!backend->mountpoint)
				{
					ELEKTRA_ADD_WARNING(14, errorKey, keyValue(cur));
					failure = 1;
				}

				keyIncRef(backend->mountpoint);
				ksDel (cut);
			}
			else if (!strcmp(keyBaseName(cur), "setplugins"))
			{
				if (elektraProcessPlugins(backend->setplugins, modules, referencePlugins,
							cut, systemConfig, errorKey) == -1)
				{
					ELEKTRA_ADD_WARNING(15, errorKey, "elektraProcessPlugins for set failed");
					failure = 1;
				}
			}
			else if (!strcmp(keyBaseName(cur), "errorplugins"))
			{
				if (elektraProcessPlugins(backend->errorplugins, modules, referencePlugins,
							cut, systemConfig, errorKey) == -1)
				{
					ELEKTRA_ADD_WARNING(15, errorKey, "elektraProcessPlugins for error failed");
					failure = 1;
				}
			} else {
				// no one cares about that config
				ELEKTRA_ADD_WARNING(16, errorKey, keyBaseName(cur));
				ksDel (cut);
			}
		}
	}

	if (failure)
	{
		Backend *tmpBackend = elektraBackendOpenMissing(backend->mountpoint);
		elektraBackendClose(backend, errorKey);
		backend = tmpBackend;
	}

	ksDel (systemConfig);
	ksDel (elektraConfig);
	ksDel (referencePlugins);

	return backend;
}

/**
 * Opens the internal backend that indicates that a backend
 * is missing at that place.
 *
 * @return the fresh allocated backend or 0 if no memory
 */
Backend* elektraBackendOpenMissing(Key *mp)
{
	Backend *backend = elektraCalloc(sizeof(struct _Backend));
	backend->refcounter = 1;

	Plugin *plugin = elektraPluginMissing();
	if (!plugin)
	{
		/* Could not allocate plugin */
		elektraFree(backend);
		return 0;
	}

	backend->getplugins[0] = plugin;
	backend->setplugins[0] = plugin;
	plugin->refcounter = 2;

	keySetString (mp, "missing");
	backend->mountpoint = mp;
	keyIncRef(backend->mountpoint);

	return backend;
}

/**
 * Opens a default backend using the plugin named dump and resolver.
 *
 * @param modules the modules to work with
 * @param errorKey the key to issue warnings and errors to
 * @return the fresh allocated default backend or 0 if it failed
 */
Backend* elektraBackendOpenDefault(KeySet *modules, Key *errorKey)
{
	Backend *backend = elektraCalloc(sizeof(struct _Backend));
	backend->refcounter = 1;

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

	Plugin *storage = elektraPluginOpen("dump", modules, storageConfig, errorKey);
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

/**@return a backend which gives plugin configuration of the module
 * which is currently point to.
 *
 * @param modules the modules to work with
 * @param errorKey the key to issue warnings and errors to
 */
Backend* elektraBackendOpenModules(KeySet *modules, Key *errorKey)
{
	Backend *backend = elektraCalloc(sizeof(struct _Backend));
	backend->refcounter = 1;

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

/**
 * Opens the internal version backend.
 *
 * @param errorKey the key to issue warnings and errors to
 * @return the fresh allocated default backend or 0 if it failed
 */
Backend* elektraBackendOpenVersion(Key * errorKey ELEKTRA_UNUSED)
{
	Backend *backend = elektraCalloc(sizeof(struct _Backend));
	backend->refcounter = 1;

	Plugin *plugin = elektraPluginVersion();
	if (!plugin)
	{
		/* Could not allocate plugin */
		elektraFree(backend);
		return 0;
	}

	Key *mp = keyNew ("system/elektra/version", KEY_VALUE, "version", KEY_END);

	backend->getplugins[0] = plugin;
	backend->setplugins[0] = plugin;
	plugin->refcounter = 2;

	backend->mountpoint = mp;
	keyIncRef(backend->mountpoint);

	return backend;
}

int elektraBackendClose(Backend *backend, Key* errorKey)
{
	int ret = 0;
	int errorOccurred = 0;

	if (!backend) return -1;

	-- backend->refcounter;

	/* Check if we have the last reference on the backend (unsigned!) */
	if (backend->refcounter > 0) return 0;

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
