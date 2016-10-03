#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>

#include <boost/algorithm/string/predicate.hpp>
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
		return model::PluginFormat (format, plugin.getName ());
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

	std::vector<model::PluginFormat> result;
	for (auto & plugin : plugins)
	{
		result.push_back (model::PluginFormat ("TODO", plugin.getName ()));
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
		export_plugin->set (ks, pathKey);
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