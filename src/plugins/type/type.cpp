/**
 * @file
 *
 * @brief Implementation of entry points
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include "type.hpp"

#include "type_checker.hpp"

using namespace ckdb;
#include <kdberrors.h>
#include <kdbplugin.hpp>

typedef Delegator<elektra::TypeChecker> TC;

extern "C" {

int elektraTypeOpen (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	/* plugin initialization logic */
	return TC::open (handle, errorKey);
}

int elektraTypeClose (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	/* free all plugin resources and shut it down */
	return TC::close (handle, errorKey);
}

int elektraTypeGet (ckdb::Plugin *, ckdb::KeySet * returned, ckdb::Key *)
{
	/* configuration only */
	KeySet * n;
	ksAppend (returned, n = ksNew (30, keyNew ("system/elektra/modules/type", KEY_VALUE, "type plugin waits for your orders", KEY_END),
				       keyNew ("system/elektra/modules/type/exports", KEY_END),
				       keyNew ("system/elektra/modules/type/exports/open", KEY_FUNC, elektraTypeOpen, KEY_END),
				       keyNew ("system/elektra/modules/type/exports/close", KEY_FUNC, elektraTypeClose, KEY_END),
				       keyNew ("system/elektra/modules/type/exports/get", KEY_FUNC, elektraTypeGet, KEY_END),
				       keyNew ("system/elektra/modules/type/exports/set", KEY_FUNC, elektraTypeSet, KEY_END),
#include "readme_type.c"
				       keyNew ("system/elektra/modules/type/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraTypeSet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	/* set all keys */

	if (!TC::get (handle)->check (reinterpret_cast<kdb::KeySet &> (returned)))
	{
		std::string msg = "None of supplied types (";
		msg += keyString (keyGetMeta (ksCurrent (returned), "check/type"));
		msg += ") matched for ";
		const char * name = keyName (ksCurrent (returned));
		if (name)
			msg += name;
		msg += " with string: ";
		const char * value = keyString (ksCurrent (returned));
		if (value)
			msg += value;
		ELEKTRA_SET_ERROR (52, parentKey, msg.c_str ());
		return -1;
	}

	return 1; /* success */
}

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (type)
{
	// clang-format off
	return elektraPluginExport("type",
		ELEKTRA_PLUGIN_OPEN,	&elektraTypeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTypeClose,
		ELEKTRA_PLUGIN_GET,	&elektraTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraTypeSet,
		ELEKTRA_PLUGIN_END);
}

}
