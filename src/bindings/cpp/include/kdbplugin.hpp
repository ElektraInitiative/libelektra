/**
 * @file
 *
 * @brief Helpers for creating plugins
 *
 * Make sure to include kdberrors.h before including this file if you want
 * warnings/errors to be added.
 *
 * Proper usage:
 * @code
using namespace ckdb;
#include <kdberrors.h>
#include <kdbplugin.hpp>

typedef Delegator<elektra::YourPluginClass> YPC;
// then e.g. YPC::open(handle, errorKey);
 * @endcode
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef KDBPLUGIN_HPP
#define KDBPLUGIN_HPP

#include <elektra/plugin/plugin.h>
#include <key.hpp>
#include <keyset.hpp>

template <typename Delegated>
class Delegator
{
public:
	typedef Delegated * (*Builder) (kdb::KeySet config);

	inline static Delegated * defaultBuilder (kdb::KeySet config)
	{
		return new Delegated (config);
	}

	inline static int open (ckdb::Plugin * handle, ckdb::Key * errorKey, Builder builder = defaultBuilder)
	{
		kdb::KeySet config (elektraPluginGetConfig (handle));
		int ret = openHelper (handle, config, errorKey, builder);
		config.release ();
		return ret;
	}

	inline static int close (ckdb::Plugin * handle, ckdb::Key *)
	{
		delete get (handle);
		return 1; // always successfully
	}

	inline static Delegated * get (ckdb::Plugin * handle)
	{
		return static_cast<Delegated *> (elektraPluginGetData (handle));
	}

private:
	/** This function avoids that every return path needs to release the configuration. */
	inline static int openHelper (ckdb::Plugin * handle, kdb::KeySet & config, ckdb::Key * errorKey, Builder builder)
	{
		if (config.lookup ("/module"))
		{
			// suppress warnings if it is just a module
			// don't buildup the Delegated then
			return 0;
		}

		try
		{
			elektraPluginSetData (handle, (*builder) (config));
		}
		catch (const char * msg)
		{
#ifdef KDBERRORS_H
			ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Could not create C++ plugin. Reason: %s", msg);
#endif
			return -1;
		}

		return get (handle) != nullptr ? 1 : -1;
	}
};

#endif
