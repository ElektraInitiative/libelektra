/**
 * @file
 *
 * @brief Everything related to a backend.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
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

ElektraKey * backendGetMountpoint (const Plugin * backend)
{
	if (backend == NULL)
	{
		return NULL;
	}

	ElektraKey * mp = ksLookupByName (backend->config, "system:/mountpoint", 0);

	return mp;
}

/**
 * @brief adds a key to the keyset for opening the missing backend
 * @param mp the mountpoint for the plugin
 * @param nameEnding the rest of the key name after the mountpoint
 * @param config the config for the backend
 * @param value the value of the key if needed, 0 otherwise
 */
static void appendKeyToBackendKs (ElektraKey * mp, const char * nameEnding, ElektraKeyset * config, const char * value)
{
	ElektraKey * key = keyDup (mp, ELEKTRA_KEY_CP_ALL);
	keyAddName (key, nameEnding);
	keySetString (key, value);
	ksAppendKey (config, key);
}

/**
 * Opens the internal backend that indicates that a backend
 * is missing at that place.
 *
 * @param global the global keyset of the KDB instance
 *
 * @return the fresh allocated backend or 0 if no memory
 */
static Plugin * backendOpenMissing (ElektraKeyset * global, ElektraKeyset * modules, ElektraKey * mp, ElektraKey * errorKey)
{
	ElektraKeyset * missingConfig = ksNew (12, ELEKTRA_KS_END);

	appendKeyToBackendKs (mp, "", missingConfig, 0);
	appendKeyToBackendKs (mp, "/config", missingConfig, 0);
	appendKeyToBackendKs (mp, "/config/mountpoint", missingConfig, keyName (mp));
	appendKeyToBackendKs (mp, "/get", missingConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage", missingConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage/#0", missingConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage/#0/label", missingConfig, "missing");
	appendKeyToBackendKs (mp, "/get/getstorage/#0/name", missingConfig, "missing");
	appendKeyToBackendKs (mp, "/set", missingConfig, 0);
	appendKeyToBackendKs (mp, "/set/setstorage", missingConfig, 0);
	appendKeyToBackendKs (mp, "/set/setstorage/#0", missingConfig, 0);
	appendKeyToBackendKs (mp, "/set/setstorage/#0/reference", missingConfig, "missing");

	Plugin * backend = elektraPluginOpen ("backend", modules, missingConfig, errorKey);
	if (backend == NULL)
	{
		return NULL;
	}

	backend->global = global;

	return backend;
}

/**Builds a backend out of the configuration supplied
 * from:
 *
@verbatim
system:/elektra/mountpoints/<name>
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
Plugin * backendOpen (ElektraKeyset * elektraConfig, ElektraKeyset * modules, ElektraKeyset * global, ElektraKey * errorKey)
{
	Plugin * backend = elektraPluginOpen ("backend", modules, ksDup (elektraConfig), errorKey);
	if (backend == NULL)
	{
		ksRewind (elektraConfig);

		ElektraKey * mp = ksLookupByName (elektraConfig, "system:/mountpoint", 0);

		if (mp != NULL)
		{
			backend = backendOpenMissing (global, modules, mp, errorKey);
		}
	}

	ksDel (elektraConfig);

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
Plugin * backendOpenDefault (ElektraKeyset * modules, ElektraKeyset * global, const char * file, ElektraKey * errorKey)
{
	keySetName (errorKey, "/");

	ElektraKeyset * config = ksNew (
		30, keyNew ("system:/elektra/mountpoints/default", ELEKTRA_KEY_END), keyNew ("system:/elektra/mountpoints/default/config", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/config/mountpoint", ELEKTRA_KEY_VALUE, "system:/elektra/mountpoints/default", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/config/path", ELEKTRA_KEY_VALUE, file, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/error", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/error/rollback", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/error/rollback/#0", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/error/rollback/#0/label", ELEKTRA_KEY_VALUE, KDB_RESOLVER, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/error/rollback/#0/name", ELEKTRA_KEY_VALUE, KDB_RESOLVER, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getresolver", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getresolver/#0", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getresolver/#0/reference", ELEKTRA_KEY_VALUE, KDB_RESOLVER, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getstorage", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getstorage/#0", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getstorage/#0/label", ELEKTRA_KEY_VALUE, KDB_STORAGE, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/get/getstorage/#0/name", ELEKTRA_KEY_VALUE, KDB_STORAGE, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/commit", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/commit/#0", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/commit/#0/reference", ELEKTRA_KEY_VALUE, KDB_RESOLVER, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/setresolver", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/setresolver/#0", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/setresolver/#0/reference", ELEKTRA_KEY_VALUE, KDB_RESOLVER, ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/setstorage", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/setstorage/#0", ELEKTRA_KEY_END),
		keyNew ("system:/elektra/mountpoints/default/set/setstorage/#0/reference", ELEKTRA_KEY_VALUE, KDB_STORAGE, ELEKTRA_KEY_END), ELEKTRA_KS_END);

#ifdef ENABLE_TRACER
	ElektraKeyset * tracerConfig =
		ksNew (10, keyNew ("/default/error/prerollback", ELEKTRA_KEY_END), keyNew ("/default/error/prerollback/#0", ELEKTRA_KEY_END),
		       keyNew ("/default/error/prerollback/#0/label", ELEKTRA_KEY_VALUE, "tracer", ELEKTRA_KEY_END),
		       keyNew ("/default/error/prerollback/#0/name", ELEKTRA_KEY_VALUE, "tracer", ELEKTRA_KEY_END),
		       keyNew ("/default/get/pregetstorage", ELEKTRA_KEY_END), keyNew ("/default/get/pregetstorage/#0", ELEKTRA_KEY_END),
		       keyNew ("/default/get/pregetstorage/#0/reference", ELEKTRA_KEY_VALUE, "tracer", ELEKTRA_KEY_END),
		       keyNew ("/default/set/presetstorage", ELEKTRA_KEY_END), keyNew ("/default/set/presetstorage/#0", ELEKTRA_KEY_END),
		       keyNew ("/default/set/presetstorage/#0/reference", ELEKTRA_KEY_VALUE, "tracer"));
	ksAppend (config, tracerConfig);
#endif

	Plugin * backend = elektraPluginOpen ("backend", modules, config, errorKey);
	if (backend == NULL)
	{
		return NULL;
	}
	backend->global = global;

	return backend;
}

/**@return a backend which gives plugin configuration of the module
 * which is currently point to.
 *
 * @param modules the modules to work with
 * @param global the global keyset of the KDB instance
 * @param errorKey the key to issue warnings and errors to
 */
Plugin * backendOpenModules (ElektraKeyset * modules, ElektraKeyset * global, ElektraKey * errorKey)
{
	ElektraKey * mp = keyNew ("system:/elektra/modules", ELEKTRA_KEY_VALUE, "modules", ELEKTRA_KEY_END);
	ElektraKey * cur = ksCurrent (modules);

	keySetName (errorKey, keyName (cur));

	keyAddBaseName (mp, keyBaseName (cur));

	ElektraKeyset * moduleConfig = ksNew (12, ELEKTRA_KS_END);

	appendKeyToBackendKs (mp, "", moduleConfig, 0);
	appendKeyToBackendKs (mp, "/config", moduleConfig, 0);
	appendKeyToBackendKs (mp, "/config/module", moduleConfig, "1");
	appendKeyToBackendKs (mp, "/config/mountpoint", moduleConfig, keyName (mp));
	appendKeyToBackendKs (mp, "/get", moduleConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage", moduleConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage/#0", moduleConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage/#0/config", moduleConfig, 0);
	appendKeyToBackendKs (mp, "/get/getstorage/#0/config/module", moduleConfig, "1");
	appendKeyToBackendKs (mp, "/get/getstorage/#0/label", moduleConfig, keyBaseName (cur));
	appendKeyToBackendKs (mp, "/get/getstorage/#0/name", moduleConfig, keyBaseName (cur));

	keySetName (errorKey, keyName (cur));

	elektraCursor save = ksGetCursor (modules);

	Plugin * backend = elektraPluginOpen ("backend", modules, moduleConfig, errorKey);
	if (backend == NULL)
	{
		return NULL;
	}

	backend->global = global;

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
Plugin * backendOpenVersion (ElektraKeyset * global, ElektraKeyset * modules, ElektraKey * errorKey)
{
	ElektraKeyset * versionConfig =
		ksNew (12, keyNew ("system:/elektra/version", ELEKTRA_KEY_END), keyNew ("system:/elektra/version/config", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/config/mountpoint", ELEKTRA_KEY_VALUE, "system:/elektra/version", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/get", ELEKTRA_KEY_END), keyNew ("system:/elektra/version/get/getstorage", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/get/getstorage/#0", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/get/getstorage/#0/label", ELEKTRA_KEY_VALUE, "version", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/get/getstorage/#0/name", ELEKTRA_KEY_VALUE, "version", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/set", ELEKTRA_KEY_END), keyNew ("system:/elektra/version/set/setstorage", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/set/setstorage/#0", ELEKTRA_KEY_END),
		       keyNew ("system:/elektra/version/set/setstorage/#0/reference", ELEKTRA_KEY_VALUE, "version", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	Plugin * backend = elektraPluginOpen ("backend", modules, versionConfig, errorKey);
	if (backend == NULL)
	{
		/* Could not allocate plugin */
		return NULL;
	}
	backend->global = global;

	return backend;
}

#if 1 == 0
/**
 * @brief Update internal size values of a backend
 *
 * @param split the split containing the size to be updated
 * @param index the index of the backend in the split
 * @param parent for parent
 * @param size to update (-1 default, 0 empty, >0 otherwise)
 *
 * @pre parent must be serializable namespace
 *
 * @retval -1 if invalid parent (assert)
 * @retval 0 on success
 */
int backendUpdateSize (Split * split, int index, ElektraKey * parent, int size)
{
	elektraNamespace namespace = keyGetNamespace (parent);
	switch (namespace)
	{
	case ELEKTRA_NS_SPEC:
		split->specsizes[index] = size;
		break;
	case ELEKTRA_NS_DIR:
		split->dirsizes[index] = size;
		break;
	case ELEKTRA_NS_USER:
		split->usersizes[index] = size;
		break;
	case ELEKTRA_NS_SYSTEM:
		split->systemsizes[index] = size;
		break;
	case ELEKTRA_NS_PROC:
	case ELEKTRA_NS_META:
	case ELEKTRA_NS_CASCADING:
	case ELEKTRA_NS_NONE:
	case ELEKTRA_NS_DEFAULT:
		ELEKTRA_ASSERT (0, "invalid namespace %d", namespace);
		return -1;
	}

	ELEKTRA_LOG_DEBUG ("spec: %zd", split->specsizes[index]);
	ELEKTRA_LOG_DEBUG ("dir: %zd", split->dirsizes[index]);
	ELEKTRA_LOG_DEBUG ("user: %zd", split->usersizes[index]);
	ELEKTRA_LOG_DEBUG ("system: %zd", split->systemsizes[index]);

	return 0;
}
#endif
