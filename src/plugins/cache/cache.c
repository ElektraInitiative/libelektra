/**
 * @file
 *
 * @brief Source for cache plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "cache.h"

#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbprivate.h>
#include <kdbconfig.h>
#include <kdbmodule.h>

#define KDB_CACHE_STORAGE "mmapstorage"

typedef struct _cacheHandle CacheHandle;

struct _cacheHandle
{
	KeySet * modules;
	Plugin * resolver;
	Plugin * cacheStorage;
};

int elektraCacheOpen (Plugin * handle, Key * errorKey)
{
	// plugin initialization logic
	// this function is optional
	CacheHandle * ch = elektraMalloc (sizeof (CacheHandle));

	ch->modules = ksNew (0, KS_END);
	elektraModulesInit (ch->modules, 0);

	KeySet * resolverConfig = ksNew (0, KS_END);
	ch->resolver = elektraPluginOpen (KDB_RESOLVER, ch->modules, resolverConfig, errorKey);
	if (!ch->resolver)
	{
		elektraModulesClose (ch->modules, 0);
		ksDel (ch->modules);
		elektraFree (ch);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	KeySet * mmapstorageConfig = ksNew (0, KS_END);
	ch->cacheStorage = elektraPluginOpen (KDB_CACHE_STORAGE, ch->modules, mmapstorageConfig, errorKey);
	if (!ch->cacheStorage)
	{
		elektraPluginClose (ch->resolver, 0);
		elektraModulesClose (ch->modules, 0);
		ksDel (ch->modules);
		elektraFree (ch);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

	elektraPluginSetData (handle, ch);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch)
	{
		elektraPluginClose (ch->resolver, 0);
		elektraPluginClose (ch->cacheStorage, 0);

		elektraModulesClose (ch->modules, 0);
		ksDel (ch->modules);

		elektraFree (ch);
		elektraPluginSetData (handle, 0);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/cache"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/cache", KEY_VALUE, "cache plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/cache/exports", KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/open", KEY_FUNC, elektraCacheOpen, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/close", KEY_FUNC, elektraCacheClose, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/get", KEY_FUNC, elektraCacheGet, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/set", KEY_FUNC, elektraCacheSet, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/error", KEY_FUNC, elektraCacheError, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/checkconf", KEY_FUNC, elektraCacheCheckConfig, KEY_END),
#include ELEKTRA_README (cache)
			       keyNew ("system/elektra/modules/cache/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCacheSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraCacheError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (cache)
{
	// clang-format off
	return elektraPluginExport ("cache",
		ELEKTRA_PLUGIN_OPEN,	&elektraCacheOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCacheClose,
		ELEKTRA_PLUGIN_GET,	&elektraCacheGet,
		ELEKTRA_PLUGIN_SET,	&elektraCacheSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraCacheError,
		ELEKTRA_PLUGIN_END);
}
