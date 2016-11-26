/**
 * @file
 *
 * @brief implementation of the config service class
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <algorithm>
#include <regex>

#include <boost/algorithm/string/predicate.hpp>

#include <service.hpp>

namespace kdbrest
{

namespace service
{

const std::string REGEX_CONF_KEY = "^((?:[a-zA-Z_]+\\.?[a-zA-Z_]+)?)\\.?#_?([0-9]+)\\.?((?:[a-zA-Z0-9_#]+(?:\\.?[a-zA-Z0-9_#])+)?)";

/**
 * @brief can be used to load the configuration of the whole application
 * 
 * the configuration is loaded from the key database provided by elektra.
 * the result can be used to bootstrap the application (cppcms service).
 */
cppcms::json::value ConfigEngine::loadApplicationConfiguration () const
{
	cppcms::json::value result;
	kdb::KDB kdb;
	kdb::KeySet ks;

	kdb.get (ks, ELEKTRA_REST_CONFIG_ROOT);
	ks = ks.cut (kdb::Key (ELEKTRA_REST_CONFIG_ROOT, KEY_END));

	for (auto elem : ks)
	{
		std::string key =
			elem.getName ().substr (elem.getName ().find (ELEKTRA_REST_CONFIG_ROOT) + strlen (ELEKTRA_REST_CONFIG_ROOT) + 1);
		std::replace (key.begin (), key.end (), '/', '.');

		this->setValue (result, key, elem);
	}

	return result;
}

/**
 * @brief can be used to set a key value to a cppcms::json::value
 * 
 * checks the path for an array and recursively sets the value then.
 * for objects the path is simply set.
 * 
 * @param config the current configuration value
 * @param path remaining path to set
 * @param key the elektra key containing the value to set
 */
void ConfigEngine::setValue (cppcms::json::value & config, std::string path, kdb::Key & key) const
{
	// check if the key contains an array
	std::regex array_regex (REGEX_CONF_KEY);
	std::smatch matches;

	// there is an array in the path
	if (std::regex_match (path, matches, array_regex) && !matches.empty ())
	{
		// here we have a structure like: some.path.before.#_14.array
		// so we delegate call!
		if (!matches.str (1).empty ())
		{
			// next element will be an array, so make one
			try
			{
				config.at (matches.str (1));
			}
			catch (cppcms::json::bad_value_cast & e)
			{
				config.set (matches.str (1), cppcms::json::array ());
			}
			this->setValue (config.at (matches.str (1)), matches.str (0).erase (0, matches.str (1).length () + 1), key);
			return;
		}

		// here we have a structure like: #_14.var
		// while it is not yet sure if ".var" is really there
		int array_index = std::stoi (matches.str (2));
		if (config.type () != cppcms::json::is_array)
		{
			config.set_value (cppcms::json::array ());
		}

		// with remaining part like ".var" we need to call recursively
		if (!matches.str (3).empty ())
		{
			try
			{
				config[array_index];
			}
			catch (cppcms::json::bad_value_cast & e)
			{
				config[array_index] = cppcms::json::object ();
			}
			this->setValue (config[array_index], matches.str (3), key);
			return;
		}

		// otherwise we can set directly
		try
		{
			config[array_index] = key.get<int> ();
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}
		try
		{
			std::string val = key.getString ();
			if (val == "true")
			{
				config[array_index] = true;
			}
			else if (val == "false")
			{
				config[array_index] = false;
			}
			else
			{
				config[array_index] = val;
			}
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}
	}
	// there is no array in the path, set as object(s)
	else
	{
		try
		{
			config.set (path, key.get<int> ());
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}
		try
		{
			std::string val = key.getString ();
			if (val == "true")
			{
				config.set (path, true);
			}
			else if (val == "false")
			{
				config.set (path, false);
			}
			else
			{
				config.set (path, val);
			}
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}
	}
}

} // namespace service

} // namespace kdbrest
