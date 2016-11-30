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

/**
 * @brief can be used to load the configuration of the whole application
 * 
 * the configuration is loaded from the key database provided by elektra.
 * the result can be used to bootstrap the application (cppcms service).
 * 
 * @param profile the profile for which the configuration should be loaded
 * @return the loaded configuration as cppcms::json::value
 */
cppcms::json::value ConfigEngine::loadApplicationConfiguration (const std::string profile) const
{
	kdb::KDB kdb;
	kdb::KeySet ks;

	std::string conf_root = std::string (ELEKTRA_REST_CONFIG_ROOT) + profile;

	kdb.get (ks, conf_root);
	ks = ks.cut (kdb::Key (conf_root, KEY_END));

	cppcms::json::value result = this->transformKeysetToJsonValue (ks, conf_root);

	return result;
}

/**
 * @brief can be used to transform a kdb::KeySet into cppcms::json::value
 * 
 * will iterate through the keyset and use a helper method to add the key
 * values to the json::value
 * 
 * @param ks the keyset that needs to be transformed
 * @return a cppcms::json::value containing the configuration from the keyset
 */
cppcms::json::value ConfigEngine::transformKeysetToJsonValue (const kdb::KeySet & ks, const std::string & conf_root) const
{
	cppcms::json::value result;

	for (auto elem : ks)
	{
		std::string key = elem.getName ().substr (elem.getName ().find (conf_root) + conf_root.length () + 1);
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
void ConfigEngine::setValue (cppcms::json::value & config, const std::string path, const kdb::Key & key) const
{
	// check if the key contains an array
	std::regex array_regex (REGEX_CHECK_KEY_IS_ARRAY);
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
			if (static_cast<int> (config.array ().size ()) <= array_index)
			{
				config[array_index] = cppcms::json::object ();
			}
			this->setValue (config[array_index], matches.str (3), key);
			return;
		}

		// otherwise we can set directly
		try
		{
			config[array_index] = key.get<bool> ();
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}
		try
		{
			config[array_index] = key.get<int> ();
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}

		config[array_index] = key.getString ();
		return;
	}
	// there is no array in the path, set as object(s)
	else
	{
		try
		{
			config.set (path, key.get<bool> ());
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}
		try
		{
			config.set (path, key.get<int> ());
			return;
		}
		catch (kdb::KeyTypeConversion & e)
		{
			// do nothing, it's fine
		}

		config.set (path, key.getString ());
		return;
	}
}

} // namespace service

} // namespace kdbrest
