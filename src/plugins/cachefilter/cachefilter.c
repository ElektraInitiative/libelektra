/**
 * @file
 *
 * @brief Source for cachefilter plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "cachefilter.h"

#include <errno.h>
#include <kdberrors.h>
#include <kdbhelper.h>


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

	// initialize working KeySets
	KeySet * toBeCached = 0;
	KeySet * temp = 0;

	// get old cache data
	void * cachedData = elektraPluginGetData (handle);
	if (cachedData != NULL)
	{
		toBeCached = (KeySet *)cachedData;
	}
	else
	{
		toBeCached = ksNew (ksGetSize (returned), KS_END);
	}

	// filter keys that need to be cached
	temp = ksCut (returned, parentKey);
	ksAppend (toBeCached, returned);
	ksClear (returned);
	ksAppend (returned, temp);
	ksDel (temp);

	// cache the filtered keys
	elektraPluginSetData (handle, toBeCached);

	return 1; // success
}

int elektraCachefilterSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	// get cached keys
	KeySet * cachedKeys = (KeySet *)elektraPluginGetData (handle);
	if (cachedKeys == NULL)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1; // did not call kdbGet() before and therefore
			   // also no elektraCachefilterGet()
	}

	// mix all keys that need to be stored together
	ksAppend (returned, cachedKeys);

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

