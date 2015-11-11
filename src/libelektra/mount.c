/***************************************************************************
            mount.c  -  high level functions for backend mounting.
                             -------------------
    begin                : Sat Nov 3
    copyright            : (C) 2007 by Patrick Sabin
    email                : patricksabin@gmx.at
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

#if DEBUG && defined(HAVE_STDIO_H)
#include <stdio.h>
#endif

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#include "kdbinternal.h"



/**
 * Creates a trie from a given configuration.
 *
 * The config will be deleted within this function.
 *
 * @note elektraMountDefault is not allowed to be executed before
 *
 * @param kdb the handle to work with
 * @param modules the current list of loaded modules
 * @param config the configuration which should be used to build up the trie.
 * @param errorKey the key used to report warnings
 * @retval -1 on failure
 * @retval 0 on success
 * @ingroup mount
 */
int elektraMountOpen(KDB *kdb, KeySet *config, KeySet *modules, Key *errorKey)
{
	Key *root;
	Key *cur;

	ksRewind(config);
	root=ksLookupByName(config, KDB_KEY_MOUNTPOINTS, KDB_O_CREATE);

	int ret = 0;
	while ((cur = ksNext(config)) != 0)
	{
		if (keyRel (root, cur) == 1)
		{
			KeySet *cut = ksCut(config, cur);
			Backend *backend = elektraBackendOpen(cut, modules, errorKey);

			if (!backend)
			{
				ELEKTRA_ADD_WARNING(24, errorKey, "could not create missing backend");
				ret = -1;
				continue;
			}

			if (!backend->mountpoint)
			{
				ELEKTRA_ADD_WARNING(25, errorKey, "no mountpoint");
				ret = -1;
				elektraBackendClose(backend, errorKey);
				continue;
			}

			if (elektraMountBackend(kdb, backend, errorKey) == -1)
			{
				ELEKTRA_ADD_WARNING(24, errorKey, "mounting of backend failed");
				ret = -1;
				/* elektraMountBackend modified the refcounter. */
				backend->refcounter = 1;
				elektraBackendClose(backend, errorKey);
				continue;
			}
		}
	}
	ksDel (config);

	return ret;
}


/** Reopens the default backend and mounts the default backend if needed.
 *
 * @pre Default Backend is closed. elektraMountOpen was executed before.
 *
 * @param kdb the handle to work with
 * @param modules the current list of loaded modules
 * @param errorKey the key used to report warnings
 * @retval -1 on error
 * @retval 0 on success
 * @ingroup mount
 */
int elektraMountDefault (KDB *kdb, KeySet *modules, Key *errorKey)
{
	/* Reopen the default Backend for fresh user experience (update issue) */
	kdb->defaultBackend = elektraBackendOpenDefault(modules, errorKey);

	if (!kdb->defaultBackend)
	{
		ELEKTRA_ADD_WARNING(43, errorKey, "could not reopen default backend");
		return -1;
	}

	Key *key = 0;
	Backend* backend = 0;

	for (elektraNamespace ns=KEY_NS_FIRST; ns<=KEY_NS_LAST; ++ns)
	{
	switch (ns)
	{
	case KEY_NS_SPEC:
		key = keyNew ("spec", KEY_VALUE, "default", KEY_END);
		backend = elektraMountGetBackend(kdb, key);
		if (backend != kdb->defaultBackend)
		{
			/* It does not matter that spec is not reachable anymore */
			keyDel (key);
		} else {
			/* User is reachable, so append that to split */
			elektraSplitAppend(kdb->split, backend, key, 2);
		}
		break;
	case KEY_NS_DIR:
		key = keyNew ("dir", KEY_VALUE, "default", KEY_END);
		backend = elektraMountGetBackend(kdb, key);
		if (backend != kdb->defaultBackend)
		{
			/* It does not matter that dir is not reachable anymore */
			keyDel (key);
		} else {
			/* User is reachable, so append that to split */
			elektraSplitAppend(kdb->split, backend, key, 2);
		}
		break;
	case KEY_NS_SYSTEM:
		/* We want system/elektra still reachable
		 * through default backend.
		 * First check if it is still reachable.
		 */
		key = keyNew ("system/elektra", KEY_END);
		backend = elektraMountGetBackend(kdb, key);
		keyDel (key);
		if (backend != kdb->defaultBackend)
		{
			/* It is not reachable, mount it */
			elektraMountBackend (kdb, kdb->defaultBackend, errorKey);
			/*elektraMountBackend will set refcounter*/
			++ kdb->defaultBackend->refcounter;
			kdb->split->syncbits[kdb->split->size-1] = 2;
		} else {
			/* Lets add the reachable default backend to split.
			 Note that it is not possible that system/elektra has the default
			 backend, but system has not. */
			elektraSplitAppend(kdb->split, backend,
					keyNew("system", KEY_VALUE, "default", KEY_END), 2);
		}
		break;
	case KEY_NS_USER:
		key = keyNew ("user", KEY_VALUE, "default", KEY_END);
		backend = elektraMountGetBackend(kdb, key);
		if (backend != kdb->defaultBackend)
		{
			/* It does not matter that user is not reachable anymore */
			keyDel (key);
		} else {
			/* User is reachable, so append that to split */
			elektraSplitAppend(kdb->split, backend, key, 2);
		}
		break;
	case KEY_NS_EMPTY:
	case KEY_NS_PROC:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
		break;
	}
	}

	return 0;
}
int elektraMountGlobals(KDB *kdb, KeySet *keys, KeySet *modules, Key *errorKey)
{
	Key *root = ksLookupByName(keys, "system/elektra/globalplugins", 0);
	if(!root)
	{
#if DEBUG && VERBOSE
		printf("no global configuration exists\n");
#endif
		ksDel(keys);
		return 0;
	}
	KeySet *global = ksCut(keys, root);
	Key *cur;
	KeySet *referencePlugins = ksNew(0, KS_END);
	while((cur = ksNext(global)) != NULL)
	{
		if(keyRel(root, cur) != 1)
			continue;
		const char *placement = keyBaseName(cur);
		const char *pluginName = keyString(cur);
		const char *globalPlacements[NR_GLOBAL_PLUGINS] = { "prerollback", "postrollback", 
			"pregetstorage", "postgetstorage", "presetstorage", 
			"precommit", "postcommit"};


		if(!strcmp(pluginName, ""))
		{
			continue;  //strict consistency: skip empty parent keys
		}
		for(GlobalpluginPositions i = 0; i < NR_GLOBAL_PLUGINS; ++i)
		{
			if(!strcmp(placement, globalPlacements[i]))
			{
#if DEBUG && VERBOSE
				printf("mounting global plugin %s to %s\n", pluginName, placement);
#endif	
				Plugin *plugin;
				Key *refKey;
				Key *searchKey = keyNew("/", KEY_END);
				keyAddBaseName(searchKey, keyString(cur));
				refKey=ksLookup(referencePlugins, searchKey, 0);
				keyDel(searchKey);
				if(refKey)
				{
					plugin = *(Plugin**)keyValue(refKey);
				}
				else
				{
					Key *sysConfigCutKey = keyDup(cur);
					keyAddBaseName(sysConfigCutKey, "system");
					Key *usrConfigCutKey = keyDup(cur);
					keyAddBaseName(usrConfigCutKey, "user");
					KeySet *sysConfigKS = ksCut(global, sysConfigCutKey);
					KeySet *usrConfigKS = ksCut(global, usrConfigCutKey);
					KeySet *renamedSysConfig = elektraRenameKeys(sysConfigKS, "system");
					KeySet *renamedUsrConfig = elektraRenameKeys(usrConfigKS, "user");
					ksDel(sysConfigKS);
					ksDel(usrConfigKS);
					keyDel(usrConfigCutKey);
					keyDel(sysConfigCutKey);
					KeySet *config = ksNew(0, KS_END);
					ksAppend(config, renamedSysConfig);
					ksAppend(config, renamedUsrConfig);
					ksDel(renamedSysConfig);
					ksDel(renamedUsrConfig);
					plugin = elektraPluginOpen(pluginName, modules, ksDup(config), errorKey);
					if(!plugin)
					{
						ELEKTRA_ADD_WARNING (64, errorKey, pluginName);
						return -1;
					}
					refKey = keyNew("/", KEY_BINARY, KEY_SIZE, sizeof(Plugin *), KEY_VALUE, &plugin, KEY_END);
					keyAddBaseName(refKey, keyString(cur));
					ksAppendKey(referencePlugins, refKey);
					keyDel(refKey);
					ksDel(config);
				}
				kdb->globalPlugins[i] = plugin;
			}
		}
			
	}
	ksDel(global);
	ksDel(keys);
	ksDel(referencePlugins);
	return 0;
}

/** Mount all module configurations.
 *
 * @param kdb the handle to work with
 * @param modules the current list of loaded modules
 * @param errorKey the key used to report warnings
 * @ingroup mount
 * @retval -1 if not rootkey was found
 * @retval 0 otherwise
 */
int elektraMountModules (KDB *kdb, KeySet *modules, Key *errorKey)
{
	Key *root;
	Key *cur;

	root=ksLookupByName(modules, "system/elektra/modules", 0);

	if (!root)
	{
		ELEKTRA_ADD_WARNING(23, errorKey, "no root key found for modules");
		return -1;
	}


	while ((cur = ksNext (modules)) != 0)
	{
		Backend * backend = elektraBackendOpenModules(modules, errorKey);
		elektraMountBackend(kdb, backend, errorKey);
	}

	return 0;
}

/** Mount the version backend
 *
 * @param kdb the handle to work with
 * @param errorKey the key used to report warnings
 * @ingroup mount
 * @retval 0 on success
 */
int elektraMountVersion (KDB *kdb, Key *errorKey)
{
	Backend * backend = elektraBackendOpenVersion(errorKey);
	elektraMountBackend(kdb, backend, errorKey);

	return 0;
}

/**
 * Mounts a backend into the trie.
 *
 * @pre user must pass correctly allocated backend
 * @post sets reference counter of backend
 *
 * @warning in case of default backends, the reference counter needs to
 * be modified *after* calling elektraMountBackend.
 *
 * @param kdb the handle to work with
 * @param backend the backend to mount
 * @param errorKey the key used to report warnings
 * @retval -1 on failure
 * @retval 1 on success
 * @ingroup mount
 */
int elektraMountBackend (KDB *kdb, Backend *backend, Key *errorKey ELEKTRA_UNUSED)
{

	char *mountpoint;
	/* 20 is enough for any of the combinations below. */
	mountpoint = elektraMalloc (keyGetNameSize(backend->mountpoint)+20);

	/* Note that you must set the refcounter to the number of insertions
	   into the trie */

	if (!strcmp(keyName(backend->mountpoint), ""))
	{
		/* Default backend */
		sprintf(mountpoint, "system/elektra/");
		kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
		elektraSplitAppend(kdb->split, backend, keyNew("system/elektra/", KEY_VALUE, "default", KEY_END), 0);
		backend->refcounter = 1;
	}
	else if (!strcmp (keyName(backend->mountpoint), "/"))
	{
		/* Root backend */
		backend->refcounter = 0;
		for (elektraNamespace ns=KEY_NS_FIRST; ns<=KEY_NS_LAST; ++ns)
		{
		switch (ns)
		{
		case KEY_NS_SPEC:
			sprintf(mountpoint, "spec%s", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend, keyNew("spec", KEY_VALUE, "root", KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_DIR:
			sprintf(mountpoint, "dir%s", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend, keyNew("dir", KEY_VALUE, "root", KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_USER:
			sprintf(mountpoint, "user%s", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend, keyNew("user", KEY_VALUE, "root", KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_SYSTEM:
			sprintf(mountpoint, "system%s", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend, keyNew("system", KEY_VALUE, "root", KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_PROC:
		case KEY_NS_EMPTY:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
			break;
		}
		}
	}
	else if (keyName(backend->mountpoint)[0] == '/')
	{
		/* Cascading Backend */
		backend->refcounter = 0;
		for (elektraNamespace ns=KEY_NS_FIRST; ns<=KEY_NS_LAST; ++ns)
		{
		switch (ns)
		{
		case KEY_NS_DIR:
			sprintf(mountpoint, "dir%s/", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend,
				keyNew(mountpoint, KEY_VALUE, keyString(backend->mountpoint), KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_USER:
			sprintf(mountpoint, "user%s/", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend,
				keyNew(mountpoint, KEY_VALUE, keyString(backend->mountpoint), KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_SYSTEM:
			sprintf(mountpoint, "system%s/", keyName(backend->mountpoint));
			kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
			elektraSplitAppend(kdb->split, backend,
				keyNew(mountpoint, KEY_VALUE, keyString(backend->mountpoint), KEY_END), 2);
			++backend->refcounter;
			break;
		case KEY_NS_SPEC:
			// excluded on purpose because mounting spec is a separate step
			// (see specmount)
		case KEY_NS_EMPTY:
		case KEY_NS_PROC:
		case KEY_NS_NONE:
		case KEY_NS_META:
		case KEY_NS_CASCADING:
			break;
		}
		}
	} else {
		/* Common single mounted backend */
		sprintf(mountpoint, "%s/", keyName(backend->mountpoint));
		kdb->trie = elektraTrieInsert(kdb->trie, mountpoint, backend);
		elektraSplitAppend(kdb->split, backend, keyDup (backend->mountpoint), 0);
		backend->refcounter = 1;
	}

	elektraFree(mountpoint);

	return 1;
}



/**
 * Lookup a mountpoint in a handle for a specific key.
 *
 * Will return a key representing the mountpoint or null
 * if there is no appropriate mountpoint e.g. its the
 * root mountpoint.
 *
 * @par Example:
 * @code
Key * key = keyNew ("system/template");
KDB * handle = kdbOpen();
Key *mountpoint=0;
mountpoint=kdbGetMountpoint(handle, key);

printf("The backend I am using is %s mounted in %s\n",
	keyValue(mountpoint),
	keyName(mountpoint));
kdbClose (handle);
keyDel (key);
 * @endcode
 *
 *
 * @param handle is the data structure, where the mounted directories are saved.
 * @param where the key, that should be looked up.
 * @return the mountpoint associated with the key
 * @ingroup mount
 */
Key* elektraMountGetMountpoint(KDB *handle, const Key *where)
{
	Backend *backend_handle;

	backend_handle=elektraMountGetBackend(handle,where);
	if (!backend_handle)
	{
		return 0;
	}

	return backend_handle->mountpoint;
}



/**
 * Lookup a backend handle for a specific key.
 *
 * The required canonical name is ensured by using a key as parameter,
 * which will transform the key to canonical representation.
 *
 * Will return handle when no more specific KDB could be
 * found.
 *
 * If key is 0 or invalid the default backend will be returned.
 *
 * @param handle is the data structure, where the mounted directories are saved.
 * @param key the key, that should be looked up.
 * @return the backend handle associated with the key
 * @ingroup mount
 */
Backend* elektraMountGetBackend(KDB *handle, const Key *key)
{
	if (!key || !strcmp(keyName(key), "")) return handle->defaultBackend;

	Backend *ret = elektraTrieLookup(handle->trie, key);
	if (!ret) return handle->defaultBackend;
	return ret;
}
