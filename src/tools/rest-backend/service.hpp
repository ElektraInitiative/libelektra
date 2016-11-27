/**
 * @file
 *
 * @brief header for the service classes
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_SERVICE_HPP
#define ELEKTRA_REST_SERVICE_HPP

#include <iostream>
#include <string>
#include <vector>

#include <boost/thread/locks.hpp>
#include <boost/thread/shared_mutex.hpp>
#include <cppcms/json.h>

#include <config.hpp>
#include <exceptions.hpp>
#include <kdb_includes.hpp>
#include <model_configformat.hpp>
#include <model_entry.hpp>
#include <model_importedconfig.hpp>
#include <model_pluginformat.hpp>
#include <model_user.hpp>
#include <singleton.hpp>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

/**
 * @brief namespace for service classes
 */
namespace service
{

/**
 * @brief service offering search and filter functionality
 * 
 * Service that is responsible for doing search specific tasks such as
 * filtering a result set based on search parameters.
 * 
 * It is designed as singleton, so all applications using it will share
 * the same instance, which allows for further logic if necessary.
 */
class SearchEngine : public singleton<SearchEngine>
{

public:
	// configuration entries
	void filterConfigurationsByName (std::vector<kdbrest::model::Entry> & entries, const std::string & startsWith) const;
	void findConfigurationsByFilter (std::vector<kdbrest::model::Entry> & entries, const std::string & filter,
					 const std::string filterby = Config::output_default_entry_filterby) const;

	// user entries
	void findUsersByFilter (std::vector<kdbrest::model::User> & users, const std::string & filter,
				const std::string filterby = Config::output_default_user_filterby) const;

private:
};

/**
 * @brief service offering storage functionality
 * 
 * Service that is responsible for storing configuration and user
 * entries in the kdb database. Besides storing it is also capable of
 * updating and deleting of single entries, as well as looking up
 * single and multiple entries at the same time.
 * 
 * It is designed as singleton, so all applications using it will share
 * the same instance, which allows for further logic if necessary.
 */
class StorageEngine : public singleton<StorageEngine>
{

public:
	// constructor
	StorageEngine ();

	// configuration entries
	bool createEntry (model::Entry & entry);
	bool updateEntry (model::Entry & entry);
	bool deleteEntry (model::Entry & entry);

	bool entryExists (const std::string & key);
	model::Entry getEntry (const std::string & key);
	std::vector<model::Entry> getAllEntries (bool force = false);
	std::vector<model::Entry> & getAllEntriesRef (bool force = false);

	// user entries
	bool createUser (model::User & user);
	bool updateUser (model::User & user);
	bool deleteUser (model::User & user);

	bool userExists (const std::string & username);
	model::User getUser (const std::string & username);
	std::vector<model::User> getAllUsers (bool force = false);
	std::vector<model::User> & getAllUsersRef (bool force = false);

private:
	void loadAllEntries ();
	void loadAllUsers ();

	std::vector<model::Entry> m_entryCache;
	boost::shared_mutex m_mutex_entryCache;

	std::vector<model::User> m_userCache;
	boost::shared_mutex m_mutex_userCache;
};

/**
 * @brief service offering conversion functionality
 * 
 * Service that is responsible for converting configuration entries
 * into the various configuration formats with help of enabled plugins.
 * 
 * It is designed as singleton, so all applications using it will share
 * the same instance, which allows for further logic if necessary.
 */
class ConvertEngine : public singleton<ConvertEngine>
{

public:
	ConvertEngine ()
	{
		this->m_enabledFormats = loadEnabledFormats ();
	}

	std::vector<kdbrest::model::PluginFormat> getEnabledFormats () const
	{
		return m_enabledFormats;
	}
	void setEnabledFormats (std::vector<model::PluginFormat> & formats)
	{
		this->m_enabledFormats = formats;
	}

	std::vector<kdbrest::model::PluginFormat> loadEnabledFormats ();
	model::PluginFormat findSuitablePlugin (const std::string & format) const;

	kdbrest::model::ConfigFormat exportTo (const std::string format, model::Entry & entry) const;
	kdbrest::model::ConfigFormat exportTo (const model::PluginFormat & plugin, model::Entry & entry) const;
	std::vector<kdbrest::model::ConfigFormat> exportToAll (model::Entry & entry) const;

	model::ImportedConfig import (const std::string & config, const std::string & format, const model::Entry & forEntry) const;

private:
	std::vector<kdbrest::model::PluginFormat> m_enabledFormats;
};

/**
 * @brief service offering application configuration retrieval
 * 
 * this service can be used to load the application configuration
 * on start up. it is based on elektra.
 */
class ConfigEngine : public singleton<ConfigEngine>
{

public:
	cppcms::json::value loadApplicationConfiguration () const;

private:
	void setValue (cppcms::json::value & config, const std::string path, const kdb::Key & key) const;
};

} // namespace service

} // namespace kdbrest

#endif
