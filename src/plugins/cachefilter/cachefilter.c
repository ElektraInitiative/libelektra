/**
 * @file
 *
 * @brief Source for cachefilter plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "cachefilter.h"

#include <errno.h>
#include <kdberrors.h>
#include <kdbhelper.h>

#include <kdblogger.h>


int elektraCachefilterGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/cachefilter"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/cachefilter", KEY_VALUE, "cachefilter plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/cachefilter/exports", KEY_END),
			keyNew ("system/elektra/modules/cachefilter/exports/get", KEY_FUNC, elektraCachefilterGet, KEY_END),
			keyNew ("system/elektra/modules/cachefilter/exports/set", KEY_FUNC, elektraCachefilterSet, KEY_END),
			keyNew ("system/elektra/modules/cachefilter/exports/close", KEY_FUNC, elektraCachefilterClose, KEY_END),
#include ELEKTRA_README (cachefilter)
			keyNew ("system/elektra/modules/cachefilter/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	ELEKTRA_LOG ("cachefilter get %s start %zd", keyName (parentKey), ksGetSize (returned));
	// initialize working KeySets
	KeySet * toBeCached = 0;

	// get old cache data
	void * cache = elektraPluginGetData (handle);
	if (cache == NULL)
	{
		toBeCached = ksNew (ksGetSize (returned), KS_END);
	}
	else
	{
		toBeCached = (KeySet *)cache;
	}

	// first ensure the cache is up to date with the
	// freshly requested keys
	ksAppend (toBeCached, returned);

	// then ensure to return only the requested keys
	// (matching parentKey)
	ksClear (returned);
	KeySet * requestedKeys = ksCut (toBeCached, parentKey);
	ksAppend (returned, requestedKeys);
	ksDel (requestedKeys);

	// and also keep the returned keys in cache as well
	// this is necessary for successive kdbGet() calls
	ksAppend (toBeCached, returned);

	// point the plugin data to the cached keyset
	elektraPluginSetData (handle, toBeCached);
	ELEKTRA_LOG ("cachefilter get %s end %zd", keyName (parentKey), ksGetSize (returned));

	return 1; // success
}

int elektraCachefilterSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	ELEKTRA_LOG ("cachefilter set %s begin %zd", keyName (parentKey), ksGetSize (returned));
	// get cached keys
	void * cache = elektraPluginGetData (handle);
	if (cache == NULL)
	{
		ELEKTRA_SET_ERROR (107, parentKey, "Cache was not initialized.");
		return -1; // did not call kdbGet() before and therefore
			   // also no elektraCachefilterGet()
	}

	KeySet * cachedKeys = (KeySet *)cache;

	// first remove all keys from the cache that match the parentKey
	// but are not in the returned keyset anymore (deleted keys)
	ksDel (ksCut (cachedKeys, parentKey));
	ksAppend (cachedKeys, returned);
	ksAppend (returned, cachedKeys);
	ELEKTRA_LOG ("cachefilter set %s end %zd", keyName (parentKey), ksGetSize (returned));

	return 1; // success
}

int elektraCachefilterClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * ks = (KeySet *)elektraPluginGetData (handle);
	if (ks) ksDel (ks);
	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (cachefilter)
{
	// clang-format off
	return elektraPluginExport ("cachefilter",
		ELEKTRA_PLUGIN_GET,	&elektraCachefilterGet,
		ELEKTRA_PLUGIN_SET,	&elektraCachefilterSet,
		ELEKTRA_PLUGIN_CLOSE, &elektraCachefilterClose,
		ELEKTRA_PLUGIN_END);
}

