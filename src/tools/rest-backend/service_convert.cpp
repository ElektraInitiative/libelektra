/**
 * @file
 *
 * @brief implementation of the conversion service class
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/filesystem.hpp>

#include <backendparser.hpp>
#include <modules.hpp>
#include <plugin.hpp>
#include <plugindatabase.hpp>
#include <pluginspec.hpp>
#include <service.hpp>

namespace kdbrest
{

namespace service
{

/**
 * @brief tries to find a suitable plugin that can handle the supplied format.
 *
 * @param format a format that needs to be handled
 *
 * @return an object containing the format and the name of the suitable plugin
 */
model::PluginFormat ConvertEngine::findSuitablePlugin (const std::string & format) const
{
	using namespace kdb;
	using namespace kdb::tools;

	ModulesPluginDatabase db;

	try
	{
		std::string pluginname;
		std::string conf;
		if (format.find_first_of (' ') != std::string::npos)
		{
			pluginname = format.substr (0, format.find_first_of (' '));
			conf = format.substr (format.find_first_of (' ') + 1);
		}
		else
		{
			pluginname = format;
		}

		PluginSpec plugin (pluginname);

		// find status
		std::string statusString = db.lookupInfo (plugin, m_pluginStatus);
		std::vector<std::string> statuses;
		boost::split (statuses, statusString, boost::is_any_of (" "));

		// find format
		std::string providers = db.lookupInfo (plugin, m_pluginProvides);
		std::string actualFormat = this->extractFormatFromProviderList (providers);

		// parse config
		kdb::KeySet config (parsePluginArguments (conf, "system"));

		return model::PluginFormat (actualFormat, plugin.getName (), statuses, config);
	}
	catch (kdb::tools::NoPlugin & e)
	{
		throw exception::UnsupportedConfigurationFormatException ();
	}
}

/**
 * @brief loads a list of enabled formats and plugins
 * 
 * Loads the configuration for the enabled formats from the
 * key database, parses them and stores them as enabled formats.
 *
 * @return A vector containing the enabled convert formats with their
 * corresponding plugins.
 */
std::vector<model::PluginFormat> ConvertEngine::loadEnabledFormats ()
{
	using namespace kdb;
	using namespace kdb::tools;

	ModulesPluginDatabase db;
	std::vector<PluginSpec> plugins = db.lookupAllProvides (m_pluginProviderStorage);

	// sort the plugins by status before processing
	std::sort (plugins.begin (), plugins.end (), [this, &db](const PluginSpec & l, const PluginSpec & r) -> bool {
		return db.calculateStatus (db.lookupInfo (l, m_pluginStatus)) > db.calculateStatus (db.lookupInfo (r, m_pluginStatus));
	});

	std::vector<model::PluginFormat> result;
	for (auto & plugin : plugins)
	{
		// find format
		std::string providers = db.lookupInfo (plugin, m_pluginProvides);
		std::string format = this->extractFormatFromProviderList (providers);
		// find statuses
		std::string statusString = db.lookupInfo (plugin, m_pluginStatus);
		std::vector<std::string> statuses;
		boost::split (statuses, statusString, boost::is_any_of (" "));
		// push plugin to result list
		result.push_back (model::PluginFormat (format, plugin.getName (), statuses));

		for (auto & elem : db.getPluginVariants (plugin))
		{
			std::string pluginName = elem.getName ();
			result.push_back (model::PluginFormat (format, pluginName, statuses, elem.getConfig ()));
		}
	}

	return result;
}

/**
 * @brief export entry to specific configuration format
 * 
 * If the format is not supported, an exception is thrown.
 *
 * @param format a format that shall be used for the export
 * @param entry a snippet entry
 * @return an object containing the converted snippet as well as the used format & plugin
 */
model::ConfigFormat ConvertEngine::exportTo (const std::string format, model::Entry & entry) const
{
	try
	{
		model::PluginFormat pluginFormat = this->findSuitablePlugin (format);
		return this->exportTo (pluginFormat, entry);
	}
	catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
	{
		throw; // re-throw explicitely
	}
	catch (kdbrest::exception::ParseConfigurationException & e)
	{
		throw; // re-throw explicitely
	}

	throw exception::UnsupportedConfigurationFormatException ();
}

/**
 * @brief can be used to export an entry with the specified plugin.
 * 
 * @note This function does a round-trip check to validated whether
 *       the snippet was exported successfully (completely) without
 *       losing any information.
 *
 * @param plugin the plugin to use for the export
 * @param entry a snippet entry
 *
 * @return an object containing the converted snippet as well as the used format & plugin
 */
model::ConfigFormat ConvertEngine::exportTo (const model::PluginFormat & plugin, model::Entry & entry) const
{
	using namespace kdb;
	using namespace kdb::tools;

	model::ConfigFormat result;

	// create temporary file used in communication with plugin
	boost::filesystem::path tmpPath = boost::filesystem::temp_directory_path () / boost::filesystem::unique_path ();
	Key pathKey (entry.getName (), KEY_END);
	pathKey.setString (tmpPath.string ());

	Modules modules;

	KeySet ks = entry.getSubkeys ();

	PluginPtr export_plugin = modules.load (plugin.getPluginname (), plugin.getConfig ());
	try
	{
		if (export_plugin->set (ks, pathKey) <= 0)
		{
			throw exception::ParseConfigurationException ();
		}
	}
	catch (kdb::tools::MissingSymbol & e)
	{
		throw exception::UnsupportedConfigurationFormatException ();
	}

	std::ifstream tmpFile (tmpPath.string ());
	if (tmpFile.is_open ())
	{
		std::stringstream strStream;
		strStream << tmpFile.rdbuf ();

		result = model::ConfigFormat (plugin, strStream.str ());

		tmpFile.close ();
	}

	std::remove (tmpPath.c_str ());

	if (result.getConfig ().empty ())
	{
		// obviously this configuration format is not suitable for an export
		throw exception::ParseConfigurationException ();
	}

	// do round-trip validation
	try
	{
		model::Entry valEntry (entry.getPublicName ());
		std::string format = result.getPluginformat ().getPluginname ();
		for (auto elem : result.getPluginformat ().getConfig ())
		{
			format.append (" " + elem.getBaseName () + "=" + elem.getString ());
		}
		model::ImportedConfig importCfg = this->import (result.getConfig (), format, valEntry);
		// compare
		if (importCfg.getKeySet ().size () != entry.getSubkeys ().size ())
		{
			throw exception::EntryValidationException ();
		}
		for (int i = 0; i < importCfg.getKeySet ().size (); i++)
		{
			kdb::Key key1 = importCfg.getKeySet ().at (i);
			kdb::Key key2 = entry.getSubkeys ().at (i);
			if (key1.getName () != key2.getName () || key1.getString () != key2.getString ())
			{
				throw exception::EntryValidationException ();
			}
		}
		// if we reach this point, export is valid
		result.setValidated (true);
	}
	catch (kdbrest::exception::ElektraRestException & e)
	{
		result.setValidated (false);
	}

	return result;
}

/**
 * @brief Can be used to convert an entry into all enabled file formats.
 * 
 * The sub keys of the entry will be taken and converted into the
 * enabled formats. To do this, a temporary file will be used, because
 * the storage plugins, which do the conversion, can operate on files
 * only.
 *
 * @param entry The entry that should be converted into the enabled
 * file formats.
 * @return A vector containing all conversions with the information
 * which plugin created them and which format they are.
 */
std::vector<model::ConfigFormat> ConvertEngine::exportToAll (model::Entry & entry) const
{
	using namespace kdb;
	using namespace kdb::tools;

	std::vector<model::ConfigFormat> result;
	for (auto & elem : this->m_enabledFormats)
	{
		try
		{
			result.push_back (this->exportTo (elem, entry));
		}
		catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
		{
			// do nothing, we can leave this configuration out
		}
		catch (kdbrest::exception::ParseConfigurationException & e)
		{
			// do also nothing
		}
	}

	return result;
}

/**
 * @brief converts a config string with format into an entry object
 * 
 * Can be used to convert a configuration given as string into
 * a usable kdb::KeySet which is stored in an ImportedConfig model.
 *
 * @param config The configuration which should be converted
 * @return An ImportedConfig object containing the imported config as
 * kdb::KeySet if the import was successful.
 * @throws kdbrest::exception::ImportFormatUnknownException in case there
 * is no suitable storage plugin for the conversion available.
 */
model::ImportedConfig ConvertEngine::import (const std::string & config, const std::string & format, const model::Entry & forEntry) const
{
	using namespace kdb;
	using namespace kdb::tools;

	Modules modules;
	try
	{
		model::PluginFormat pluginFormat = this->findSuitablePlugin (format);

		// create temporary file used in communication with plugin
		boost::filesystem::path tmpPath = boost::filesystem::temp_directory_path () / boost::filesystem::unique_path ();
		Key pathKey (forEntry.getName (), KEY_END);
		pathKey.setString (tmpPath.string ());

		std::ofstream tmpFile (tmpPath.string ());
		if (tmpFile.is_open ())
		{
			tmpFile << config;
			tmpFile.close ();

			KeySet ks;

			PluginPtr export_plugin = modules.load (pluginFormat.getPluginname (), pluginFormat.getConfig ());
			export_plugin->get (ks, pathKey);

			std::remove (tmpPath.c_str ());

			if (ks.size () > 0)
			{
				return model::ImportedConfig (pluginFormat, ks);
			}
			else
			{
				throw exception::ParseConfigurationException ();
			}
		}
	}
	catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
	{
		throw; // re-throw explicitely
	}

	// in case we accidentally reach this point somehow
	throw exception::ParseConfigurationException ();
}

/**
 * @brief takes a provider list and extracts the storage format
 * 
 * @param providers a string containing provided functionality of a plugin
 * @return the storage format or "none" in case none was found
 */
std::string ConvertEngine::extractFormatFromProviderList (const std::string & providers) const
{
	std::stringstream ss (providers);
	std::string provider;
	std::string format = "none";
	while (ss >> provider)
	{
		if (boost::starts_with (provider, m_pluginProviderStorage + m_pluginProviderDelim))
		{
			format = provider.substr (sizeof (m_pluginProviderStorage));
			break;
		}
	}
	return format;
}

} // namespace service

} // namespace kdbrest
