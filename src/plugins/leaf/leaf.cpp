/**
 * @file
 *
 * @brief Source for leaf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf.hpp"
#include "leaf_delegate.hpp"

#include <kdbhelper.h>

using elektra::LeafDelegate;

extern "C" {

typedef Delegator<LeafDelegate> delegator;

/** @see elektraDocOpen */
int elektraLeafOpen (Plugin * handle, Key * key)
{
	// After the call to `delegator::open` you can retrieve a pointer to the delegate via `coderDelegator::get (handle)`
	return delegator::open (handle, key);
}

/** @see elektraDocClose */
int elektraLeafClose (Plugin * handle, Key * key)
{
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraLeafGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/leaf"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/leaf", KEY_VALUE, "leaf plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/leaf/exports", KEY_END),
			keyNew ("system/elektra/modules/leaf/exports/open", KEY_FUNC, elektraLeafOpen, KEY_END),
			keyNew ("system/elektra/modules/leaf/exports/close", KEY_FUNC, elektraLeafClose, KEY_END),
			keyNew ("system/elektra/modules/leaf/exports/get", KEY_FUNC, elektraLeafGet, KEY_END),
			keyNew ("system/elektra/modules/leaf/exports/set", KEY_FUNC, elektraLeafSet, KEY_END),
			keyNew ("system/elektra/modules/leaf/exports/error", KEY_FUNC, elektraLeafError, KEY_END),
			keyNew ("system/elektra/modules/leaf/exports/checkconf", KEY_FUNC, elektraLeafCheckConfig, KEY_END),
#include ELEKTRA_README (leaf)
			keyNew ("system/elektra/modules/leaf/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocSet */
int elektraLeafSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocError */
int elektraLeafError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocCheckConf */
int elektraLeafCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (leaf)
{
	// clang-format off
	return elektraPluginExport ("leaf",
		ELEKTRA_PLUGIN_OPEN,	&elektraLeafOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraLeafClose,
		ELEKTRA_PLUGIN_GET,	&elektraLeafGet,
		ELEKTRA_PLUGIN_SET,	&elektraLeafSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraLeafError,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
