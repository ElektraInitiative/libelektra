/**
 * @file
 *
 * @brief Source for ansible plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "ansible.hpp"
#include "ansible_delegate.hpp"

#include <kdberrors.h>
#include <kdbhelper.h>

using ckdb::keyNew;
using std::exception;

using elektra::AnsibleDelegate;

using CppKey = kdb::Key;
using CppKeySet = kdb::KeySet;

namespace
{

/**
 * @brief This function returns a key set containing the plugin contract.
 *
 * @return A key set specifying the capabilities of the plugin
 */
CppKeySet getContract ()
{
	return CppKeySet{ 30,
			  keyNew ("system:/elektra/modules/ansible", KEY_VALUE, "ansible plugin waits for your orders", KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports", KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports/open", KEY_FUNC, elektraAnsibleOpen, KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports/close", KEY_FUNC, elektraAnsibleClose, KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports/get", KEY_FUNC, elektraAnsibleGet, KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports/set", KEY_FUNC, elektraAnsibleSet, KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports/error", KEY_FUNC, elektraAnsibleError, KEY_END),
			  keyNew ("system:/elektra/modules/ansible/exports/checkconf", KEY_FUNC, elektraAnsibleCheckConf, KEY_END),
#include ELEKTRA_README
			  keyNew ("system:/elektra/modules/ansible/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),
			  KS_END };
}

} // end namespace

extern "C" {

typedef Delegator<AnsibleDelegate> delegator;

/** @see elektraDocOpen */
int elektraAnsibleOpen (Plugin * handle, Key * key)
{
	int status = ELEKTRA_PLUGIN_STATUS_ERROR;

	try
	{
		// - The function below calls the constructor `AnsibleDelegate(config)`.
		// - After the call to `delegator::open` you can retrieve a pointer to the delegate via `delegator::get (handle)`.
		status = delegator::open (handle, key);
	}
	catch (exception const & error)
	{
		ELEKTRA_SET_PLUGIN_MISBEHAVIOR_ERRORF (key, "Uncaught Exception: %s", error.what ());
	}

	return status;
}

/** @see elektraDocClose */
int elektraAnsibleClose (Plugin * handle, Key * key)
{
	// The function `delegator::close` calls the destructor of `AnsibleDelegate`.
	return delegator::close (handle, key);
}

/** @see elektraDocGet */
int elektraAnsibleGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	if (parent.getName () == "system:/elektra/modules/ansible")
	{
		keys.append (getContract ());
	}
	else
	{
		// This is only an example, to show you how to call a method of the delegate
		keys.append (delegator::get (handle)->getConfig (parent));
	}

	parent.release ();
	keys.release ();
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocSet */
int elektraAnsibleSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	CppKeySet keys{ returned };
	CppKey parent{ parentKey };

	delegator::get (handle)->createPlaybook (keys, parent);

	parent.release ();
	keys.release ();

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

/** @see elektraDocError */
int elektraAnsibleError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

/** @see elektraDocCheckConf */
int elektraAnsibleCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("ansible",
		ELEKTRA_PLUGIN_OPEN,	&elektraAnsibleOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraAnsibleClose,
		ELEKTRA_PLUGIN_GET,	&elektraAnsibleGet,
		ELEKTRA_PLUGIN_SET,	&elektraAnsibleSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraAnsibleError,
		ELEKTRA_PLUGIN_END);
}

} // end extern "C"
