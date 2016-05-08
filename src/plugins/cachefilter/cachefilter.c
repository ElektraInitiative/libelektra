/**
 * @file
 *
 * @brief Source for cachefilter plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "cachefilter.h"

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
#include ELEKTRA_README (cachefilter)
			keyNew ("system/elektra/modules/cachefilter/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	// filter keys that need to be cached
	KeySet * temp = 0;
	KeySet * toBeCached = ksNew (ksGetSize (returned), KS_END);
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
	// get cached keys (duplicate in case of kdbSet() error!)
	KeySet * cachedKeys = ksDup ((KeySet *)elektraPluginGetData (handle));

	// add the keys that should be written to DB to the filtered keys
	ksAppend (cachedKeys, returned);

	// swap returned and filteredKeys
	ksClear (returned);
	ksAppend (returned, cachedKeys);

	// clean up
	ksDel (cachedKeys);

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (cachefilter)
{
	// clang-format off
	return elektraPluginExport ("cachefilter",
		ELEKTRA_PLUGIN_GET,	&elektraCachefilterGet,
		ELEKTRA_PLUGIN_SET,	&elektraCachefilterSet,
		ELEKTRA_PLUGIN_END);
}

