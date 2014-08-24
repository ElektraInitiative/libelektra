/***************************************************************************
                     type.c  -  Skeleton of a plugin
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "type.hpp"

#include <key.hpp>
#include <keyset.hpp>

#include "type_checker.hpp"

using namespace ckdb;
#include <kdberrors.h>

/**This function avoid that many return pathes need to release the
  * configuration. */
inline static int elektraTypeOpenDelegator(ckdb::Plugin *handle, kdb::KeySet& config, ckdb::Key *errorKey)
{
	if (config.lookup("/module"))
	{
		// suppress warnings if it is just a module
		// don't buildup the struct then
		return 0;
	}

	try {
		elektraPluginSetData (handle, new elektra::TypeChecker(config));
	}
	catch (const char* msg)
	{
		// TODO: warnings are not always passed when plugin
		// creation failed?
		ELEKTRA_ADD_WARNING (69, errorKey, msg);
		return -1;
	}

	return 1;
}

extern "C"
{

int elektraTypeOpen(ckdb::Plugin *handle, ckdb::Key *errorKey)
{
	/* plugin initialization logic */
	int ret;

	kdb::KeySet config (elektraPluginGetConfig(handle));

	ret = elektraTypeOpenDelegator(handle, config, errorKey);

	config.release();

	return ret;

}

int elektraTypeClose(ckdb::Plugin *handle, ckdb::Key *)
{
	/* free all plugin resources and shut it down */

	delete static_cast<elektra::TypeChecker*>(elektraPluginGetData (handle));

	return 1; /* success */
}

int elektraTypeGet(ckdb::Plugin *, ckdb::KeySet *returned, ckdb::Key *)
{
	/* configuration only */
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/type",
			KEY_VALUE, "type plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/type/exports", KEY_END),
		keyNew ("system/elektra/modules/type/exports/open",
			KEY_FUNC, elektraTypeOpen,
			KEY_END),
		keyNew ("system/elektra/modules/type/exports/close",
			KEY_FUNC, elektraTypeClose,
			KEY_END),
		keyNew ("system/elektra/modules/type/exports/get",
			KEY_FUNC, elektraTypeGet,
			KEY_END),
		keyNew ("system/elektra/modules/type/exports/set",
			KEY_FUNC, elektraTypeSet,
			KEY_END),
#include "readme_type.c"
		keyNew ("system/elektra/modules/type/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraTypeSet(ckdb::Plugin *handle, ckdb::KeySet *returned, ckdb::Key *parentKey)
{
	/* set all keys */

	if (!static_cast<elektra::TypeChecker*>(elektraPluginGetData (handle))->check
			(reinterpret_cast<kdb::KeySet&>(returned)))
	{
		std::string msg = "None of supplied types matched for ";
		const char *name = keyName (ksCurrent(returned));
		if (name) msg += name;
		msg += " with string: ";
		const char *value = keyString (ksCurrent(returned));
		if (value) msg += value;
		ELEKTRA_SET_ERROR (52, parentKey, msg.c_str());
		return -1;
	}

	return 1; /* success */
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(type)
{
	return elektraPluginExport("type",
		ELEKTRA_PLUGIN_OPEN,	&elektraTypeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraTypeClose,
		ELEKTRA_PLUGIN_GET,	&elektraTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraTypeSet,
		ELEKTRA_PLUGIN_END);
}

}
