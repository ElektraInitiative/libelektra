/***************************************************************************
                     struct.c  -  Skeleton of a plugin
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


#include "struct.hpp"

#include <key.hpp>
#include <keyset.hpp>

#include "factory.hpp"

using namespace ckdb;
#include <kdberrors.h>

/** This function avoid that many return pathes need to release the
  * configuration. */
inline static int elektraStructOpenDelegator(ckdb::Plugin *handle, kdb::KeySet& config, ckdb::Key *errorKey)
{
	if (config.lookup("/module"))
	{
		// suppress warnings if it is just a module
		// don't buildup the struct then
		return 0;
	}

	try {
		elektra::Checker *c = static_cast<elektra::Checker*>(elektra::buildChecker(config));
		elektraPluginSetData (handle, c);
	}
	catch (const char* msg)
	{
		ELEKTRA_ADD_WARNING (58, errorKey, msg);
		return -1;
	}

	return 1;
}

extern "C"
{

int elektraStructOpen(ckdb::Plugin *handle, ckdb::Key *errorKey)
{
	int ret;

	/* plugin initialization logic */
	kdb::KeySet config (elektraPluginGetConfig(handle));

	ret = elektraStructOpenDelegator(handle, config, errorKey);

	config.release();


	return ret;
}

int elektraStructClose(ckdb::Plugin *handle, ckdb::Key *)
{
	/* free all plugin resources and shut it down */
	elektra::Checker *c = static_cast<elektra::Checker*>(elektraPluginGetData (handle));

	delete c;

	return 1; /* success */
}

int elektraStructGet(ckdb::Plugin *, ckdb::KeySet *returned, ckdb::Key *)
{
	/* configuration only */
	KeySet *n;
	ksAppend (returned, n=ksNew (30,
		keyNew ("system/elektra/modules/struct",
			KEY_VALUE, "struct plugin waits for your orders", KEY_END),
		keyNew ("system/elektra/modules/struct/exports", KEY_END),
		keyNew ("system/elektra/modules/struct/exports/open",
			KEY_FUNC, elektraStructOpen,
			KEY_END),
		keyNew ("system/elektra/modules/struct/exports/close",
			KEY_FUNC, elektraStructClose,
			KEY_END),
		keyNew ("system/elektra/modules/struct/exports/get",
			KEY_FUNC, elektraStructGet,
			KEY_END),
		keyNew ("system/elektra/modules/struct/exports/set",
			KEY_FUNC, elektraStructSet,
			KEY_END),
#include "readme_struct.c"
		keyNew ("system/elektra/modules/struct/infos/version",
			KEY_VALUE, PLUGINVERSION, KEY_END),
		KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraStructSet(ckdb::Plugin *handle, ckdb::KeySet *returned, ckdb::Key *parentKey)
{
	/* set all keys */

	try {
		if (ksGetSize(returned) < 1) throw "Empty keyset will not be accepted";
		ksRewind(returned);
		const char *first_keyname = keyName(ksNext(returned));
		const char *parentkeyname = keyName(parentKey);
		if (strcmp (first_keyname, parentkeyname))
			throw "first keyname is not equal the parentKey";

		elektra::Checker *c = static_cast<elektra::Checker*>(elektraPluginGetData (handle));
		doCheck (c, returned);
	}
	catch (const char* msg)
	{
		ELEKTRA_SET_ERROR (53, parentKey, msg);
		return -1;
	}

	return 1; /* success */
}

ckdb::Plugin *ELEKTRA_PLUGIN_EXPORT(struct)
{
	return elektraPluginExport("struct",
		ELEKTRA_PLUGIN_OPEN,	&elektraStructOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraStructClose,
		ELEKTRA_PLUGIN_GET,	&elektraStructGet,
		ELEKTRA_PLUGIN_SET,	&elektraStructSet,
		ELEKTRA_PLUGIN_END);
}

}
