/**
 * @file
 *
 * @brief configuration helper functions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <iostream>
#include <string>

#include <boost/algorithm/string/replace.hpp>
#include <cppcms/json.h>

#include <config.hpp>

namespace kdbrest
{

/**
 * @brief initializes application configuration variables
 * 
 * can use compile-time defaults if no dynamic configuration
 * is available. can also force the application to stop.
 * 
 * @param config the configuration to use
 * @return true in case of success, false otherwise
 */
bool Config::initializeConfiguration (const cppcms::json::value & cfg, const std::string profile)
{
	// store config
	try
	{
		this->m_config = cfg.at ("backend");
	}
	catch (cppcms::json::bad_value_cast & e)
	{
		std::cerr << "Missing configuration for the entire backend.\nPlease have a look at the manual and configure the backend "
			     "properly."
			  << std::endl;
		return false;
	}

	std::string conf_root = std::string (ELEKTRA_REST_CONFIG_ROOT) + profile;
	bool error = false;

	auto checkType = [this, &error, &conf_root](std::string key, cppcms::json::json_type type, std::string setToMsg) {
		if (this->m_config.type (key) != type)
		{
			error = true;
			std::cerr << "Missing configuration for " << key << ".\nPlease set the key " << conf_root << "/backend/"
				  << boost::replace_all_copy (key, ".", "/") << " " << setToMsg << "." << std::endl;
		}
	};

	// api/description settings
	checkType ("api.description.raw", cppcms::json::is_string, "to a link targeting the API blueprint");
	checkType ("api.description.html", cppcms::json::is_string, "to a link targeting the compiled API description");

	// JWT settings
	checkType ("jwt.encryption_key", cppcms::json::is_string, "to a random secret");
	checkType ("jwt.expiration_time", cppcms::json::is_number, "to the number of seconds a session token should be valid");

	// output settings
	checkType ("output.default.entry.sort", cppcms::json::is_string, "to the default sort direction of entries");
	checkType ("output.default.entry.sortby", cppcms::json::is_string, "to the default sort criteria of entries");
	checkType ("output.default.entry.filterby", cppcms::json::is_string, "to the default filter criteria of entries");
	checkType ("output.default.user.sort", cppcms::json::is_string, "to the default sort direction of users");
	checkType ("output.default.user.sortby", cppcms::json::is_string, "to the default sort criteria of users");
	checkType ("output.default.user.filterby", cppcms::json::is_string, "to the default filter criteria of users");

	// permissions settings
	checkType ("permissions.entry.create", cppcms::json::is_number,
		   "to the rank that is required to be able to create new snippet entries");
	checkType ("permissions.entry.edit", cppcms::json::is_number,
		   "to the rank that is required to be able to edit snippet entries of other users");
	checkType ("permissions.entry.delete", cppcms::json::is_number,
		   "to the rank that is required to be able to delete snippet entries of other users");
	checkType ("permissions.user.view", cppcms::json::is_number, "to the rank that is required to view account details of other users");
	checkType ("permissions.user.edit", cppcms::json::is_number, "to the rank that is required to edit account details of other users");
	checkType ("permissions.user.delete", cppcms::json::is_number, "to the rank that is required to delete accounts of other users");
	checkType ("permissions.default_rank", cppcms::json::is_number, "to the rank that new accounts will be assigned by default");

	// kdb settings
	checkType ("kdb.path.configs", cppcms::json::is_string, "to the elektra key where configuration snippets should be stored");
	checkType ("kdb.path.users", cppcms::json::is_string, "to the elektra key where user details should be stored");

	return !error;
}
}
