#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/filesystem.hpp>

#include "modules.hpp"
#include "plugin.hpp"
#include "plugindatabase.hpp"
#include "pluginspec.hpp"
#include "service.hpp"

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
model::PluginFormat ConvertEngine::findSuitablePlugin (const std::string & format)
{
	using namespace kdb;
	using namespace kdb::tools;

	ModulesPluginDatabase db;

	try
	{
		PluginSpec plugin = db.lookupProvides (format);
		std::string statusString = db.lookupInfo (plugin, "status");
		std::vector<std::string> statuses;
		boost::split (statuses, statusString, boost::is_any_of (" "));
		return model::PluginFormat (format, plugin.getName (), statuses);
	}
	catch (kdb::tools::NoPlugin & e)
	{
		throw exception::UnsupportedConfigurationFormatException ();
	}
}

/**
 * @brief Loads the configuration for the enabled formats from the
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
	std::vector<PluginSpec> plugins = db.lookupAllProvides ("storage");

	// sort the plugins by status before processing
	std::sort (plugins.begin (), plugins.end (), [&db](const PluginSpec & l, const PluginSpec & r) -> bool {
		return db.calculateStatus (db.lookupInfo (l, "status")) > db.calculateStatus (db.lookupInfo (r, "status"));
	});

	std::vector<model::PluginFormat> result;
	for (auto & plugin : plugins)
	{
		// find format
		std::string provides = db.lookupInfo (plugin, "provides");
		std::stringstream ss (provides);
		std::string provider;
		std::string format = "none";
		while (ss >> provider)
		{
			if (boost::starts_with (provider, "storage/"))
			{
				format = provider.substr (8);
				break;
			}
		}
		// find statuses
		std::string statusString = db.lookupInfo (plugin, "status");
		std::vector<std::string> statuses;
		boost::split (statuses, statusString, boost::is_any_of (" "));
		// push plugin to result list
		result.push_back (model::PluginFormat (format, plugin.getName (), statuses));
	}

	return result;
}

/**
 * @brief can be used to export an entry to the specified format, if it is supported.
 *
 * @param format a format that shall be used for the export
 * @param entry a snippet entry
 *
 * @return an object containing the converted snippet as well as the used format & plugin
 */
model::ConfigFormat ConvertEngine::exportTo (const std::string format, model::Entry & entry)
{
	try
	{
		model::PluginFormat pluginFormat = this->findSuitablePlugin (format);
		return this->exportTo (pluginFormat, entry);
	}
	catch (kdbrest::exception::UnsupportedConfigurationFormatException & e)
	{
		throw e; // re-throw explicitely
	}
	catch (kdbrest::exception::ParseConfigurationException & e)
	{
		throw e; // re-throw explicitely
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
model::ConfigFormat ConvertEngine::exportTo (model::PluginFormat & plugin, model::Entry & entry)
{
	using namespace kdb;
	using namespace kdb::tools;

	model::ConfigFormat result;

	// create temporary file used in communication with plugin
	boost::filesystem::path tmpPath = boost::filesystem::unique_path ();
	std::string tmpPathStr = "/tmp/" + tmpPath.string ();
	Key pathKey (entry.getName (), KEY_END);
	pathKey.setString (tmpPathStr);

	Modules modules;

	KeySet ks = entry.getSubkeys ();

	PluginPtr export_plugin = modules.load (plugin.getPluginname ());
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

	std::ifstream tmpFile (tmpPathStr);
	if (tmpFile.is_open ())
	{
		std::stringstream strStream;
		strStream << tmpFile.rdbuf ();

		result = model::ConfigFormat (plugin, strStream.str ());

		tmpFile.close ();
	}

	std::remove (tmpPathStr.c_str ());

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
 * The sub keys of the entry will be taken and converted into the
 * enabled formats. To do this, a temporary file will be used, because
 * the storage plugins, which do the conversion, can operate on files
 * only.
 *
 * @param entry The entry that should be converted into the enabled
 * file formats.
 *
 * @return A vector containing all conversions with the information
 * which plugin created them and which format they are.
 */
std::vector<model::ConfigFormat> ConvertEngine::exportToAll (model::Entry & entry)
{
	using namespace kdb;
	using namespace kdb::tools;

	std::vector<model::ConfigFormat> result;
	for (auto & elem : this->_enabledFormats)
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
 * @brief Can be used to convert a configuration given as string into
 * a usable kdb::KeySet which is stored in an ImportedConfig model.
 *
 * @param config The configuration which should be converted
 *
 * @return An ImportedConfig object containing the imported config as
 * kdb::KeySet if the import was successful.
 *
 * @throws kdbrest::exception::ImportFormatUnknownException in case there
 * is no suitable storage plugin for the conversion available.
 */
model::ImportedConfig ConvertEngine::import (std::string & config, std::string & format, model::Entry & forEntry)
{
	using namespace kdb;
	using namespace kdb::tools;

	Modules modules;
	try
	{
		model::PluginFormat pluginFormat = this->findSuitablePlugin (format);

		// create temporary file used in communication with plugin
		boost::filesystem::path tmpPath = boost::filesystem::unique_path ();
		std::string tmpPathStr = "/tmp/" + tmpPath.string ();
		Key pathKey (forEntry.getName (), KEY_END);
		pathKey.setString (tmpPathStr);

		std::ofstream tmpFile (tmpPathStr);
		if (tmpFile.is_open ())
		{
			tmpFile << config;
			tmpFile.close ();

			KeySet ks;

			PluginPtr export_plugin = modules.load (pluginFormat.getPluginname ());
			export_plugin->get (ks, pathKey);

			std::remove (tmpPathStr.c_str ());

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
		throw e;
	}

	// in case we accidentally reach this point somehow
	throw exception::ParseConfigurationException ();
}

} // namespace service

} // namespace kdbrest