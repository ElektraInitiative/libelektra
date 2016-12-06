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
#include <helper/keyhelper.hpp>

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
		// and find the best configuration through ksLookup and the cascading key
		kdb::Key elemCascading = elem.dup ();
		kdb::tools::helper::removeNamespace (elemCascading);
		kdb::Key key = ks.lookup (elemCascading.getName ());

		// although we can safely assume here that we have found a valid key,
		// we better check and skip if necessary
		if (!key)
		{
			continue;
		}

		// transform the keyname into cppcms::json input representation
		std::string keyName = key.getName ().substr (key.getName ().find (conf_root) + conf_root.length () + 1);
		std::replace (keyName.begin (), keyName.end (), '/', '.');

		// finally set the value
		this->handleValueInsertion (result, keyName, key);
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
void ConfigEngine::handleValueInsertion (cppcms::json::value & config, const std::string path, const kdb::Key & key) const
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
			this->handleValueInsertion (config.at (matches.str (1)), matches.str (0).erase (0, matches.str (1).length () + 1),
						    key);
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
			this->handleValueInsertion (config[array_index], matches.str (3), key);
			return;
		}

		// otherwise we can set directly
		this->setValue (config[array_index], key);
		return;
	}
	// there is no array in the path, set as object(s)
	else
	{
		config.set (path, NULL);
		this->setValue (config.at (path), key);
		return;
	}
}

/**
 * @brief tries to retrieve the correct key value (correct format/type) and stores it
 * 
 * if the key does not have meta data specifying the type, it will first be
 * tried to convert the value as number, then as string
 * 
 * @param config a json value
 * @param key the key which holds the value to store
 */
void ConfigEngine::setValue (cppcms::json::value & config, const kdb::Key & key) const
{
	if (key.hasMeta ("check/type"))
	{
		std::string type = key.getMeta<std::string> ("check/type");
		if (type == "short" || type == "unsigned_short" || type == "long" || type == "unsigned_long" || type == "long_long" ||
		    type == "unsigned_long_long" || type == "float" || type == "double")
		{
			config.set_value (key.get<long> ());
		}
		else if (type == "boolean")
		{
			config.set_value (key.get<bool> ());
		}
		else if (type == "char")
		{
			config.set_value (key.get<char> ());
		}
		else // (type == "string")
		{
			config.set_value (key.get<std::string> ());
		}
		return;
	}

	try
	{
		config.set_value (key.get<long> ());
		return;
	}
	catch (kdb::KeyTypeConversion & e)
	{
		// this was only an attempt, use fallback below
	}
	config.set_value (key.get<std::string> ());
}

} // namespace service

} // namespace kdbrest
