/**
 * @file
 *
 * @brief Everything related to a backend.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <kdbassert.h>

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
 * @brief Allocate a backend
 *
 * Initialize everything with zero, except: sizes with -1
 * and refcounter with 1
 *
 * @return
 */
static Backend * elektraBackendAllocate (void)
{
	Backend * backend = elektraCalloc (sizeof (struct _Backend));

	backend->refcounter = 1;

	backend->specsize = -1;
	backend->dirsize = -1;
	backend->usersize = -1;
	backend->systemsize = -1;
	return backend;
}


/**
 * @brief sets mountpoint
 *
 * @param backend where the mountpoint should be set
 * @param elektraConfig the config where the mountpoint can be found
 * @param [out] errorKey the name also has the mountpoint set
 *
 * @pre ksCurrent() is root key
 * @post ksCurrent() is root key
 *
 * @retval -1 if no mountpoint is found or memory allocation problem
 * @retval 0 on success
 */
int elektraBackendSetMountpoint (Backend * backend, KeySet * elektraConfig, Key * errorKey)
{
	Key * root = ksCurrent (elektraConfig);
	Key * searchMountpoint = keyDup (root);
	keyAddBaseName (searchMountpoint, "mountpoint");
	Key * foundMountpoint = ksLookup (elektraConfig, searchMountpoint, 0);
	keyDel (searchMountpoint);
	ksLookup (elektraConfig, root, 0); // reset ksCurrent()

	if (!foundMountpoint)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not find mountpoint within root %s", keyName (root));
		return -1;
	}

	backend->mountpoint = keyNew ("", KEY_VALUE, keyBaseName (root), KEY_END);
	elektraKeySetName (backend->mountpoint, keyString (foundMountpoint), KEY_CASCADING_NAME | KEY_EMPTY_NAME);

	keySetName (errorKey, keyName (backend->mountpoint));

	if (!backend->mountpoint)
	{
		ELEKTRA_ADD_INSTALLATION_WARNINGF (errorKey, "Could not create mountpoint with name '%s' and value %s",
						   keyString (foundMountpoint), keyBaseName (root));
		return -1;
	}

	keyIncRef (backend->mountpoint);
	return 0;
}

/**
 * Opens the internal backend that indicates that a backend
 * is missing at that place.
 *
 * @param global the global keyset of the KDB instance
 *
 * @return the fresh allocated backend or 0 if no memory
 */
static Backend * backendOpenMissing (KeySet * global, Key * mp)
{
	Backend * backend = elektraBackendAllocate ();

	Plugin * plugin = elektraPluginMissing ();
	if (!plugin)
	{
		/* Could not allocate plugin */
		elektraFree (backend);
		return 0;
	}
	plugin->global = global;

	backend->getplugins[0] = plugin;
	backend->setplugins[0] = plugin;
	plugin->refcounter = 2;

	keySetString (mp, "missing");
	backend->mountpoint = mp;
	keyIncRef (backend->mountpoint);

	return backend;
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
 * @param global the global keyset of the KDB instance
 * @param errorKey the key where an error and warnings are added
 *
 * @return a pointer to a freshly allocated backend
 *         this could be the requested backend or a so called
 *         "missing backend".
 * @retval 0 if out of memory
 * @ingroup backend
 */
Backend * backendOpen (KeySet * elektraConfig, KeySet * modules, KeySet * global, Key * errorKey)
{
	Key * cur;
	KeySet * referencePlugins = 0;
	KeySet * systemConfig = 0;
	int failure = 0;

	referencePlugins = ksNew (0, KS_END);
	ksRewind (elektraConfig);

	Key * root = ksNext (elektraConfig);

	Backend * backend = elektraBackendAllocate ();
	if (elektraBackendSetMountpoint (backend, elektraConfig, errorKey) == -1)
	{ // warning already set
		failure = 1;
	}

	while ((cur = ksNext (elektraConfig)) != 0)
	{
		if (keyIsDirectlyBelow (root, cur) == 1)
		{
			// direct below root key
			KeySet * cut = ksCut (elektraConfig, cur);
			if (!strcmp (keyBaseName (cur), "config"))
			{
				systemConfig = elektraRenameKeys (cut, "system");
				ksDel (cut);
			}
			else if (!strcmp (keyBaseName (cur), "errorplugins"))
			{
				if (elektraProcessPlugins (backend->errorplugins, modules, referencePlugins, cut, systemConfig, global,
							   errorKey) == -1)
				{
					if (!failure)
						ELEKTRA_ADD_INSTALLATION_WARNING (errorKey,
										  "Method 'elektraProcessPlugins' for error failed");
					failure = 1;
				}
			}
			else if (!strcmp (keyBaseName (cur), "getplugins"))
			{
				if (elektraProcessPlugins (backend->getplugins, modules, referencePlugins, cut, systemConfig, global,
							   errorKey) == -1)
				{
					if (!failure)
						ELEKTRA_ADD_INSTALLATION_WARNING (errorKey,
										  "Method 'elektraProcessPlugins' for get failed");
					failure = 1;
				}
			}
			else if (!strcmp (keyBaseName (cur), "mountpoint"))
			{
				ksDel (cut); // already handled by elektraBackendSetMountpoint
				continue;
			}
			else if (!strcmp (keyBaseName (cur), "setplugins"))
			{
				if (elektraProcessPlugins (backend->setplugins, modules, referencePlugins, cut, systemConfig, global,
							   errorKey) == -1)
				{
					if (!failure)
						ELEKTRA_ADD_INSTALLATION_WARNING (errorKey,
										  "Method 'elektraProcessPlugins' for set failed");
					failure = 1;
				}
			}
			else
			{
				// no one cares about that config
				if (!failure)
					ELEKTRA_ADD_VALIDATION_SYNTACTIC_WARNINGF (
						errorKey,
						"Found garbage within the backend configuration. found: %s but expected config, "
						"setplugins, getplugins, errorplugins or mountpoint",
						keyBaseName (cur));
				ksDel (cut);
			}
		}
	}

	if (failure)
	{
		Backend * tmpBackend = backendOpenMissing (global, backend->mountpoint);
		backendClose (backend, errorKey);
		backend = tmpBackend;
	}

	ksDel (systemConfig);
	ksDel (elektraConfig);
	ksDel (referencePlugins);

	return backend;
}

/**
 * Opens a default backend using the plugin named KDB_RESOLVER
 * and KDB_STORAGE.
 *
 * @param modules the modules to work with
 * @param global the global keyset of the KDB instance
 * @param errorKey the key to issue warnings and errors to
 * @return the fresh allocated default backend or 0 if it failed
 */
Backend * backendOpenDefault (KeySet * modules, KeySet * global, const char * file, Key * errorKey)
{
	Backend * backend = elektraBackendAllocate ();

	KeySet * resolverConfig = ksNew (5, keyNew ("system/path", KEY_VALUE, file, KEY_END), KS_END);

	elektraKeySetName (errorKey, "", KEY_CASCADING_NAME | KEY_EMPTY_NAME);

	Plugin * resolver = elektraPluginOpen (KDB_RESOLVER, modules, resolverConfig, errorKey);
	if (!resolver)
	{
		elektraFree (backend);
		/* error already set in elektraPluginOpen */
		return 0;
	}
	resolver->global = global;

#ifdef ENABLE_TRACER
	KeySet * tracerConfig = ksNew (5,
				       // does not matter because it is mounted differently in system/elektra/modules:
				       // keyNew("system/logmodule", KEY_VALUE, "1", KEY_END),
				       KS_END);
	Plugin * tracer = elektraPluginOpen ("tracer", modules, tracerConfig, errorKey);
	if (tracer)
	{
		backend->getplugins[RESOLVER_PLUGIN + 1] = tracer;
		backend->setplugins[RESOLVER_PLUGIN + 1] = tracer;
		backend->errorplugins[RESOLVER_PLUGIN + 1] = tracer;
		tracer->refcounter = 3;
		tracer->global = global;
	}
#endif

	backend->getplugins[RESOLVER_PLUGIN] = resolver;
	backend->setplugins[RESOLVER_PLUGIN] = resolver;
	backend->setplugins[COMMIT_PLUGIN] = resolver;
	backend->errorplugins[STORAGE_PLUGIN] = resolver;
	resolver->refcounter = 4;

	KeySet * storageConfig = ksNew (5, KS_END);

	Plugin * storage = elektraPluginOpen (KDB_STORAGE, modules, storageConfig, errorKey);
	if (!storage)
	{
		elektraPluginClose (resolver, errorKey);
		elektraFree (backend);
		/* error already set in elektraPluginOpen */
		return 0;
	}
	storage->global = global;

	backend->getplugins[STORAGE_PLUGIN] = storage;
	backend->setplugins[STORAGE_PLUGIN] = storage;
	storage->refcounter = 2;

	Key * mp = keyNew ("", KEY_VALUE, "default", KEY_END);
	backend->mountpoint = mp;
	keyIncRef (backend->mountpoint);

	return backend;
}

/**@return a backend which gives plugin configuration of the module
 * which is currently point to.
 *
 * @param modules the modules to work with
 * @param global the global keyset of the KDB instance
 * @param errorKey the key to issue warnings and errors to
 */
Backend * backendOpenModules (KeySet * modules, KeySet * global, Key * errorKey)
{
	Backend * backend = elektraBackendAllocate ();

	cursor_t save = ksGetCursor (modules);
	KeySet * defaultConfig =
		ksNew (5, keyNew ("system/module", KEY_VALUE, "1", KEY_END), keyNew ("user/module", KEY_VALUE, "1", KEY_END), KS_END);
	Key * cur = ksCurrent (modules);

	elektraKeySetName (errorKey, keyName (cur), KEY_CASCADING_NAME | KEY_EMPTY_NAME);

	Plugin * plugin = elektraPluginOpen (keyBaseName (cur), modules, defaultConfig, errorKey);
	if (!plugin)
	{
		/* Error already set in plugin */
		elektraFree (backend);
		return 0;
	}
	plugin->global = global;


	Key * mp = keyNew ("system/elektra/modules", KEY_VALUE, "modules", KEY_END);

	// for "virtual" plugins the keyBaseName (cur) would be "resolver" or "storage"
	// thus we use plugin->name here, which must be handled by kdbGet() of every
	// plugin properly by convention.
	keyAddBaseName (mp, plugin->name);

	backend->getplugins[0] = plugin;
	plugin->refcounter = 1;

	backend->mountpoint = mp;
	keyIncRef (backend->mountpoint);

	ksSetCursor (modules, save);

	return backend;
}

/**
 * Opens the internal version backend.
 *
 * @param global the global keyset of the KDB instance
 * @param errorKey the key to issue warnings and errors to
 * @return the fresh allocated default backend or 0 if it failed
 */
Backend * backendOpenVersion (KeySet * global, Key * errorKey ELEKTRA_UNUSED)
{
	Backend * backend = elektraBackendAllocate ();

	Plugin * plugin = elektraPluginVersion ();
	if (!plugin)
	{
		/* Could not allocate plugin */
		elektraFree (backend);
		return 0;
	}
	plugin->global = global;

	Key * mp = keyNew ("system/elektra/version", KEY_VALUE, "version", KEY_END);

	backend->getplugins[0] = plugin;
	backend->setplugins[0] = plugin;
	plugin->refcounter = 2;

	backend->mountpoint = mp;
	keyIncRef (backend->mountpoint);

	return backend;
}


/**
 * @brief Update internal size in backend
 *
 * @param backend the backend to update
 * @param parent for parent
 * @param size to update (-1 default, 0 empty, >0 otherwise)
 *
 * @pre parent must be serializable namespace
 *
 * @retval -1 if invalid parent (assert)
 * @retval 0 on success
 */
int backendUpdateSize (Backend * backend, Key * parent, int size)
{
	switch (keyGetNamespace (parent))
	{
	case KEY_NS_SPEC:
		backend->specsize = size;
		break;
	case KEY_NS_DIR:
		backend->dirsize = size;
		break;
	case KEY_NS_USER:
		backend->usersize = size;
		break;
	case KEY_NS_SYSTEM:
		backend->systemsize = size;
		break;
	case KEY_NS_PROC:
	case KEY_NS_EMPTY:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
	case KEY_NS_NONE:
		ELEKTRA_ASSERT (0, "invalid namespace %d", keyGetNamespace (parent));
		return -1;
	}

	ELEKTRA_LOG_DEBUG ("spec: %zd", backend->specsize);
	ELEKTRA_LOG_DEBUG ("dir: %zd", backend->dirsize);
	ELEKTRA_LOG_DEBUG ("user: %zd", backend->usersize);
	ELEKTRA_LOG_DEBUG ("system: %zd", backend->systemsize);

	return 0;
}

int backendClose (Backend * backend, Key * errorKey)
{
	int errorOccurred = 0;

	if (!backend) return -1;

	--backend->refcounter;

	/* Check if we have the last reference on the backend (unsigned!) */
	if (backend->refcounter > 0) return 0;

	keyDecRef (backend->mountpoint);
	keySetName (errorKey, keyName (backend->mountpoint));
	keyDel (backend->mountpoint);

	for (int i = 0; i < NR_OF_PLUGINS; ++i)
	{
		int ret = elektraPluginClose (backend->setplugins[i], errorKey);
		if (ret == -1) ++errorOccurred;

		ret = elektraPluginClose (backend->getplugins[i], errorKey);
		if (ret == -1) ++errorOccurred;

		ret = elektraPluginClose (backend->errorplugins[i], errorKey);
		if (ret == -1) ++errorOccurred;
	}
	elektraFree (backend);

	if (errorOccurred)
		return -1;
	else
		return 0;
}
