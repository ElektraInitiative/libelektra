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

Key * backendGetMountpoint (Plugin * backend)
{
	Key * mp = ksLookupByName (backend->config, "system/mountpoint", 0);

	return mp;
}

/**
 * @brief adds a key to the keyset for opening the missing backend
 * @param mp the mountpoint for the plugin
 * @param nameEnding the rest of the key name after the mountpoint
 * @param config the config for the backend
 * @param value the value of the key if needed, 0 otherwise
 */
static void appendKeyToBackendKs (Key * mp, const char * nameEnding, KeySet * config, const char * value)
{
	char * dest = elektraMalloc (strlen (keyName (mp)) + strlen (nameEnding) + 1);

	strcpy (dest, keyName (mp));
	strcat (dest, nameEnding);

	Key * key;
	if (!value)
	{
		key = keyNew (dest, KEY_END);
	}
	else
	{
		char * copiedValue = elektraMalloc (sizeof (value));
		strcpy (copiedValue, value);
		key = keyNew (dest, KEY_VALUE, copiedValue, KEY_END);
	}
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
static Plugin * backendOpenMissing (KeySet * global, KeySet * modules, Key * mp, Key * errorKey)
{
	KeySet * missingConfig = ksNew (5, KS_END);

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

	if (!backend)
	{
		elektraFree (backend);
		return 0;
	}
	backend->global = global;

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
Plugin * backendOpen (KeySet * elektraConfig, KeySet * modules, KeySet * global, Key * errorKey)
{
	Plugin * backend = elektraPluginOpen ("backend", modules, elektraConfig, errorKey);
	if (!backend)
	{
		elektraPluginClose (backend, errorKey);

		ksRewind (elektraConfig);

		Key * cur;
		char * mp = 0;

		while ((cur = ksNext (elektraConfig)) != 0)
		{
			if (!strcmp (keyBaseName (cur), "mountpoint"))
			{
				mp = elektraMalloc (sizeof (keyString (cur)));
				strcpy (mp, keyString (cur));
			}
		}

		if (mp)
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
Plugin * backendOpenDefault (KeySet * modules, KeySet * global, const char * file, Key * errorKey)
{
	elektraKeySetName (errorKey, "", KEY_CASCADING_NAME | KEY_EMPTY_NAME);

	KeySet * config =
		ksNew (30, keyNew ("default", KEY_END), keyNew ("default/config", KEY_END),
		       keyNew ("default/config/mountpoint", KEY_VALUE, "default", KEY_END),
		       keyNew ("default/config/path", KEY_VALUE, file, KEY_END), keyNew ("default/error", KEY_END),
		       keyNew ("default/error/rollback", KEY_END), keyNew ("default/error/rollback/#0", KEY_END),
		       keyNew ("default/error/rollback/#0/label", KEY_VALUE, KDB_RESOLVER, KEY_END),
		       keyNew ("default/error/rollback/#0/name", KEY_VALUE, KDB_RESOLVER, KEY_END), keyNew ("default/get", KEY_END),
		       keyNew ("default/get/getresolver", KEY_END), keyNew ("default/get/getresolver/#0", KEY_END),
		       keyNew ("default/get/getresolver/#0/reference", KEY_VALUE, KDB_RESOLVER, KEY_END),
		       keyNew ("default/get/getstorage", KEY_END), keyNew ("default/get/getstorage/#0", KEY_END),
		       keyNew ("default/get/getstorage/#0/label", KEY_VALUE, KDB_STORAGE, KEY_END),
		       keyNew ("default/get/getstorage/#0/name", KEY_VALUE, KDB_STORAGE, KEY_END), keyNew ("default/set", KEY_END),
		       keyNew ("default/set/commit", KEY_END), keyNew ("default/set/commit/#0", KEY_END),
		       keyNew ("default/set/commit/#0/reference", KEY_VALUE, KDB_RESOLVER, KEY_END),
		       keyNew ("default/set/setresolver", KEY_END), keyNew ("default/set/setresolver/#0", KEY_END),
		       keyNew ("default/set/setresolver/#0/reference", KEY_VALUE, KDB_RESOLVER, KEY_END),
		       keyNew ("default/set/setstorage", KEY_END), keyNew ("default/set/setstorage/#0", KEY_END),
		       keyNew ("default/set/setstorage/#0/reference", KEY_VALUE, KDB_STORAGE, KEY_END), KS_END);

#ifdef ENABLE_TRACER
	KeySet * tracerConfig = ksNew (10, keyNew ("default/error/prerollback", KEY_END), keyNew ("default/error/prerollback/#0", KEY_END),
				       keyNew ("default/error/prerollback/#0/label", KEY_VALUE, "tracer", KEY_END),
				       keyNew ("default/error/prerollback/#0/name", KEY_VALUE, "tracer", KEY_END),
				       keyNew ("default/get/pregetstorage", KEY_END), keyNew ("default/get/pregetstorage/#0", KEY_END),
				       keyNew ("default/get/pregetstorage/#0/reference", KEY_VALUE, "tracer", KEY_END),
				       keyNew ("default/set/presetstorage", KEY_END), keyNew ("default/set/presetstorage/#0", KEY_END),
				       keyNew ("default/set/presetstorage/#0/reference", KEY_VALUE, "tracer"));
	ksAppend (config, tracerConfig);
#endif

	Plugin * backend = elektraPluginOpen ("backend", modules, config, errorKey);

	if (!backend)
	{
		elektraPluginClose (backend, errorKey);
		elektraFree (backend);
		return 0;
	}

	return backend;
}

/**@return a backend which gives plugin configuration of the module
 * which is currently point to.
 *
 * @param modules the modules to work with
 * @param global the global keyset of the KDB instance
 * @param errorKey the key to issue warnings and errors to
 */
Plugin * backendOpenModules (KeySet * modules, KeySet * global, Key * errorKey)
{
	Key * mp = keyNew ("system/elektra/modules", KEY_VALUE, "modules", KEY_END);

	Key * cur = ksCurrent (modules);

	keyAddBaseName (mp, keyBaseName (cur));

	KeySet * moduleConfig = ksNew (9, KS_END);

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

	elektraKeySetName (errorKey, keyName (cur), KEY_CASCADING_NAME | KEY_EMPTY_NAME);

	elektraCursor save = ksGetCursor (modules);

	Plugin * backend = elektraPluginOpen ("backend", modules, moduleConfig, errorKey);
	if (!backend)
	{
		elektraFree (backend);
		return 0;
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
Plugin * backendOpenVersion (KeySet * global, KeySet * modules, Key * errorKey)
{
	KeySet * versionConfig =
		ksNew (12, keyNew ("system/elektra/version", KEY_END), keyNew ("system/elektra/version/config", KEY_END),
		       keyNew ("system/elektra/version/config/mountpoint", KEY_VALUE, "system/elektra/version", KEY_END),
		       keyNew ("system/elektra/version/get", KEY_END), keyNew ("system/elektra/version/get/getstorage", KEY_END),
		       keyNew ("system/elektra/version/get/getstorage/#0", KEY_END),
		       keyNew ("system/elektra/version/get/getstorage/#0/label", KEY_VALUE, "version", KEY_END),
		       keyNew ("system/elektra/version/get/getstorage/#0/name", KEY_VALUE, "version", KEY_END),
		       keyNew ("system/elektra/version/set", KEY_END), keyNew ("system/elektra/version/set/setstorage", KEY_END),
		       keyNew ("system/elektra/version/set/setstorage/#0", KEY_END),
		       keyNew ("system/elektra/version/set/setstorage/#0/reference", KEY_VALUE, "version", KEY_END), KS_END);

	Plugin * backend = elektraPluginOpen ("backend", modules, versionConfig, errorKey);
	if (!backend)
	{
		/* Could not allocate plugin */
		elektraFree (backend);
		return 0;
	}
	backend->global = global;

	return backend;
}


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
int backendUpdateSize (Split * split, int index, Key * parent, int size)
{
	switch (keyGetNamespace (parent))
	{
	case KEY_NS_SPEC:
		split->specsizes[index] = size;
		break;
	case KEY_NS_DIR:
		split->dirsizes[index] = size;
		break;
	case KEY_NS_USER:
		split->usersizes[index] = size;
		break;
	case KEY_NS_SYSTEM:
		split->systemsizes[index] = size;
		break;
	case KEY_NS_PROC:
	case KEY_NS_EMPTY:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
	case KEY_NS_NONE:
		ELEKTRA_ASSERT (0, "invalid namespace %d", keyGetNamespace (parent));
		return -1;
	}

	ELEKTRA_LOG_DEBUG ("spec: %zd", split->specsizes[index]);
	ELEKTRA_LOG_DEBUG ("dir: %zd", split->dirsizes[index]);
	ELEKTRA_LOG_DEBUG ("user: %zd", split->usersizes[index]);
	ELEKTRA_LOG_DEBUG ("system: %zd", split->systemsizes[index]);

	return 0;
}
