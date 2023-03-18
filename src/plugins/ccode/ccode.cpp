/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"
#include "coder.hpp"

#include <elektra/kdb/errors.h>
#include <kdbplugin.hpp>

using ckdb::keyNew;
using elektra::Coder;

using CppKeySet = kdb::KeySet;

namespace
{
typedef Delegator<Coder> coderDelegator;

/**
 * @brief This function returns a key set containing the contract of this plugin.
 *
 * @return A contract describing the functionality of this plugin.
 */
inline KeySet * contract (void)
{
	return ksNew (30, keyNew ("system:/elektra/modules/ccode", KEY_VALUE, "ccode plugin waits for your orders", KEY_END),
		      keyNew ("system:/elektra/modules/ccode/exports", KEY_END),
		      keyNew ("system:/elektra/modules/ccode/exports/open", KEY_FUNC, elektraCcodeOpen, KEY_END),
		      keyNew ("system:/elektra/modules/ccode/exports/close", KEY_FUNC, elektraCcodeClose, KEY_END),
		      keyNew ("system:/elektra/modules/ccode/exports/get", KEY_FUNC, elektraCcodeGet, KEY_END),
		      keyNew ("system:/elektra/modules/ccode/exports/set", KEY_FUNC, elektraCcodeSet, KEY_END),
#include "readme_ccode.c"
		      keyNew ("system:/elektra/modules/ccode/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
}

} // end namespace

extern "C" {

// ====================
// = Plugin Interface =
// ====================

/** @see elektraDocOpen */
int elektraCcodeOpen (Plugin * handle, Key * key)
{
	return coderDelegator::open (handle, key);
}

/** @see elektraDocClose */
int elektraCcodeClose (Plugin * handle ELEKTRA_UNUSED, Key * key)
{
	return coderDelegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraCcodeGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	if (!strcmp (keyName (parentKey), "system:/elektra/modules/ccode"))
	{
		KeySet * const pluginConfig = contract ();
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	CppKeySet keys{ returned };
	CppKeySet decoded = coderDelegator::get (handle)->decodeKeySet (keys);
	keys.release ();
	ksCopy (returned, decoded.getKeySet ());
	ksDel (decoded.release ());

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocSet */
int elektraCcodeSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	CppKeySet keys{ returned };

	CppKeySet encoded = coderDelegator::get (handle)->encodeKeySet (keys);
	keys.release ();
	ksCopy (returned, encoded.getKeySet ());
	ksDel (encoded.release ());

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("ccode",
		ELEKTRA_PLUGIN_OPEN,	&elektraCcodeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCcodeClose,
		ELEKTRA_PLUGIN_GET,	&elektraCcodeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCcodeSet,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
