/**
 * @file
 *
 * @brief Implementation of entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "cpptype.hpp"

#include "type_checker.hpp"

using namespace ckdb;
#include <kdberrors.h>
#include <kdbplugin.hpp>

typedef Delegator<elektra::TypeChecker> TC;

static void setError (Key * key, Key * errorKey)
{
	std::string msg = "The type ";
	msg += keyString (keyGetMeta (key, "check/type"));
	msg += " failed to match for ";
	const char * name = keyName (key);
	if (name) msg += name;
	msg += " with string: ";
	const char * value = keyString (key);
	if (value) msg += value;
	ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, msg.c_str ());
}

extern "C" {

int elektraCppTypeValidateKey (ckdb::Key * key, ckdb::Key * errorKey)
{
	kdb::KeySet config;
	elektra::TypeChecker tc (config);
	int ret = 1;

	kdb::Key k (key); // reinterpret_cast<kdb::Key &> (key)
	if (!tc.check (k))
	{
		setError (key, errorKey);
		ret = 0;
	}

	k.release ();
	return ret;
}

int elektraCppTypeOpen (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	/* plugin initialization logic */
	return TC::open (handle, errorKey);
}

int elektraCppTypeClose (ckdb::Plugin * handle, ckdb::Key * errorKey)
{
	/* free all plugin resources and shut it down */
	return TC::close (handle, errorKey);
}

int elektraCppTypeGet (ckdb::Plugin *, ckdb::KeySet * returned, ckdb::Key *)
{
	/* configuration only */
	KeySet * n;
	ksAppend (returned,
		  n = ksNew (30, keyNew ("system/elektra/modules/cpptype", KEY_VALUE, "cpptype plugin waits for your orders", KEY_END),
			     keyNew ("system/elektra/modules/cpptype/exports", KEY_END),
			     keyNew ("system/elektra/modules/cpptype/exports/open", KEY_FUNC, elektraCppTypeOpen, KEY_END),
			     keyNew ("system/elektra/modules/cpptype/exports/close", KEY_FUNC, elektraCppTypeClose, KEY_END),
			     keyNew ("system/elektra/modules/cpptype/exports/get", KEY_FUNC, elektraCppTypeGet, KEY_END),
			     keyNew ("system/elektra/modules/cpptype/exports/set", KEY_FUNC, elektraCppTypeSet, KEY_END),
			     keyNew ("system/elektra/modules/cpptype/exports/validateKey", KEY_FUNC, elektraCppTypeValidateKey, KEY_END),
#include ELEKTRA_README
			     keyNew ("system/elektra/modules/cpptype/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END));
	ksDel (n);

	return 1; /* success */
}

int elektraCppTypeSet (ckdb::Plugin * handle, ckdb::KeySet * returned, ckdb::Key * parentKey)
{
	int ret = 1;

	kdb::KeySet ks (returned); // reinterpret_cast<kdb::KeySet &> (returned)
	if (!TC::get (handle)->check (ks))
	{
		setError (ksCurrent (returned), parentKey);
		ret = -1;
	}

	ks.release ();
	return ret;
}

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("cpptype",
		ELEKTRA_PLUGIN_OPEN,	&elektraCppTypeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCppTypeClose,
		ELEKTRA_PLUGIN_GET,	&elektraCppTypeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCppTypeSet,
		ELEKTRA_PLUGIN_END);
}

}
