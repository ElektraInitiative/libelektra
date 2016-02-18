/**
 * @file
 *
 * @brief Implementation of Struct checker
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include "struct.hpp"

#include <key.hpp>
#include <keyset.hpp>

#include "factory.hpp"

using namespace ckdb;
#include <kdberrors.h>
#include <kdbplugin.hpp>

typedef Delegator<elektra::Checker> Checker;


extern "C" {

int elektraStructOpen (ckdb::Plugin * handle, ckdb::Key * errorKey) { return Checker::open (handle, errorKey, elektra::buildChecker); }

int elektraStructClose (ckdb::Plugin * handle, ckdb::Key * errorKey) { return Checker::close (handle, errorKey); }

int elektraStructGet (ckdb::Plugin *, ckdb::KeySet * returned, ckdb::Key *)
{
	/* configuration only */
	KeySet * n;
	ksAppend (returned,
		  n = ksNew (30, keyNew ("system/elektra/modules/struct", KEY_VALUE, "struct plugin waits for your orders", KEY_END),
			     keyNew ("system/elektra/modules/struct/exports", KEY_END),
			     keyNew ("system/elektra/modules/struct/exports/open", KEY_FUNC, elektraStructOpen, KEY_END),
			     keyNew ("system/elektra/modules/struct/exports/close", KEY_FUNC, elektraStructClose, KEY_END),
			     keyNew ("system/elektra/modules/struct/exports/get", KEY_FUNC, elektraStructGet, KEY_END),
			     keyNew ("system/elektra/modules/struct/exports/set", KEY_FUNC, elektraStructSet, KEY_END),
#include "readme_struct.c"
			     keyNew ("system/elektra/modules/struct/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraStructSet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	/* set all keys */

	try
	{
		if (ksGetSize (returned) < 1)
			throw "Empty keyset will not be accepted";
		ksRewind (returned);
		const char * first_keyname = keyName (ksNext (returned));
		const char * parentkeyname = keyName (parentKey);
		if (strcmp (first_keyname, parentkeyname))
			throw "first keyname is not equal the parentKey";

		doCheck (Checker::get (handle), returned);
	}
	catch (const char * msg)
	{
		ELEKTRA_SET_ERROR (53, parentKey, msg);
		return -1;
	}

	return 1; /* success */
}

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT (struct)
{
	// clang-format off
	return elektraPluginExport("struct",
		ELEKTRA_PLUGIN_OPEN,	&elektraStructOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraStructClose,
		ELEKTRA_PLUGIN_GET,	&elektraStructGet,
		ELEKTRA_PLUGIN_SET,	&elektraStructSet,
		ELEKTRA_PLUGIN_END);
}

}
