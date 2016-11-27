/**
 * @file
 *
 * @brief configuration helper functions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <iostream>
#include <string>

#include <config.hpp>

namespace kdbrest
{

/* set dummy variables for statics */

// JWT settings
std::string Config::jwt_encryption_key = std::string ();
int Config::jwt_expiration_time = 0;

// output settings
std::string Config::output_default_entry_sort = std::string ();
std::string Config::output_default_entry_sortby = std::string ();
std::string Config::output_default_entry_filterby = std::string ();
std::string Config::output_default_user_sort = std::string ();
std::string Config::output_default_user_sortby = std::string ();
std::string Config::output_default_user_filterby = std::string ();

// permissions settings
int Config::permissions_entry_create = 0;
int Config::permissions_entry_edit = 0;
int Config::permissions_entry_delete = 0;
int Config::permissions_user_view = 0;
int Config::permissions_user_edit = 0;
int Config::permissions_user_delete = 0;
int Config::permissions_default_rank = 0;

// kdb settings
std::string Config::kdb_path_configs = std::string ();
std::string Config::kdb_path_users = std::string ();

// api_specification settings
std::string Config::api_specification_raw = std::string ();
std::string Config::api_specification_html = std::string ();


/**
 * @brief initializes application configuration variables
 * 
 * can use compile-time defaults if no dynamic configuration
 * is available. can also force the application to stop.
 * 
 * @param config the configuration to use
 * @return true in case of success, false otherwise
 */
bool Config::initializeConfiguration (const cppcms::json::value & cfg)
{
	bool error = false;

	// clang-format off
	
	// JWT settings
	try {
		Config::jwt_encryption_key = cfg.get<std::string> ("backend.jwt.encryption_key");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for jwt.encryption_key.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/jwt/encryption_key to a random secret." << std::endl;
		error = true;
	}
	Config::jwt_expiration_time = cfg.get<int> ("backend.jwt.expiration_time", ELEKTRA_REST_DEFAULT_JWT_EXPIRATION_TIME);
	
	// output settings
	Config::output_default_entry_sort = cfg.get ("backend.output.default.entry.sort", ELEKTRA_REST_DEFAULT_OUTPUT_ENTRY_SORT);
	Config::output_default_entry_sortby = cfg.get ("backend.output.default.entry.sortby", ELEKTRA_REST_DEFAULT_OUTPUT_ENTRY_SORTBY);
	Config::output_default_entry_filterby = cfg.get ("backend.output.default.entry.filterby", ELEKTRA_REST_DEFAULT_OUTPUT_ENTRY_FILTERBY);
	Config::output_default_user_sort = cfg.get ("backend.output.default.user.sort", ELEKTRA_REST_DEFAULT_OUTPUT_USER_SORT);
	Config::output_default_user_sortby = cfg.get ("backend.output.default.user.sortby", ELEKTRA_REST_DEFAULT_OUTPUT_USER_SORTBY);
	Config::output_default_user_filterby = cfg.get ("backend.output.default.user.filterby", ELEKTRA_REST_DEFAULT_OUTPUT_USER_FILTERBY);
	
	// permissions settings
	try {
		Config::permissions_entry_create = cfg.get<int> ("backend.permissions.entry.create");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.entry.create.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/entry/create to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	try {
		Config::permissions_entry_edit = cfg.get<int> ("backend.permissions.entry.edit");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.entry.edit.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/entry/edit to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	try {
		Config::permissions_entry_delete = cfg.get<int> ("backend.permissions.entry.delete");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.entry.delete.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/entry/delete to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	try {
		Config::permissions_user_view = cfg.get<int> ("backend.permissions.user.view");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.user.view.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/user/view to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	try {
		Config::permissions_user_edit = cfg.get<int> ("backend.permissions.user.edit");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.user.edit.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/user/edit to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	try {
		Config::permissions_user_delete = cfg.get<int> ("backend.permissions.user.delete");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.user.delete.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/user/delete to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	try {
		Config::permissions_default_rank = cfg.get<int> ("backend.permissions.default_rank");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for permissions.default_rank.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/permissions/default_rank to a value between " << ELEKTRA_REST_USER_MIN_RANK << " and " << ELEKTRA_REST_USER_MAX_RANK << "." << std::endl;
		error = true;
	}
	
	// kdb settings
	Config::kdb_path_configs = cfg.get<std::string> ("backend.kdb.path.configs", ELEKTRA_REST_DEFAULT_PATH_CONFIGS);
	Config::kdb_path_users = cfg.get<std::string> ("backend.kdb.path.users", ELEKTRA_REST_DEFAULT_PATH_USERS);
	
	// api_specification settings
	try {
		Config::api_specification_raw = cfg.get<std::string> ("backend.api_specification.raw");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for api_specification.raw.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/api_specification/raw to a link targeting the API blueprint." << std::endl;
		error = true;
	}
	try {
		Config::api_specification_html = cfg.get<std::string> ("backend.api_specification.html");
	} catch (cppcms::json::bad_value_cast & e) {
		std::cerr << "Missing configuration for api_specification.html.\nPlease set the key " << ELEKTRA_REST_CONFIG_ROOT << "/backend/api_specification/html to a link targeting the compiled API description." << std::endl;
		error = true;
	}

	// clang-format on

	return !error;
}
}
