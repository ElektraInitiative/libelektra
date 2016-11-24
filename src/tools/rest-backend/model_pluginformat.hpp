#ifndef ELEKTRA_REST_MODEL_PLUGINFORMAT_HEADER_GUARD
#define ELEKTRA_REST_MODEL_PLUGINFORMAT_HEADER_GUARD

#include <string>
#include <vector>

#include <exceptions.hpp>
#include <kdb_includes.hpp>

namespace kdbrest
{

namespace model
{

class PluginFormat
{

public:
	inline PluginFormat ()
	{
	}
	/**
                 * Constructor based on a foreign PluginFormat, basically
                 * copy constructor.
                 * @param pf The foreign PluginFormat
                 */
	inline PluginFormat (const PluginFormat & pf)
	{
		m_fileformat = pf.m_fileformat;
		m_pluginname = pf.m_pluginname;
		m_pluginstatuses = std::vector<std::string> (pf.m_pluginstatuses);
	}
	/**
                 * Constructor based on a string containing both relevant
                 * informations (fileformat + pluginname). The string has to be
                 * of the format "fileformat:pluginname".
                 * @param both The string containing both fileformat and
                 *      pluginname
				 * @param statuses A vector containing plugin statuses
                 */
	inline PluginFormat (const std::string & both, const std::vector<std::string> statuses = std::vector<std::string> ())
	{
		std::size_t splitIndex = both.find (":");
		if (splitIndex == std::string::npos)
		{
			throw kdbrest::exception::FileformatPluginException (
				"The given fileformat plugin combination is invalid.\nSynopsis: <fileformat>:<pluginname>\nExample: "
				"xml:xmltool");
		}
		m_fileformat = both.substr (0, splitIndex);
		m_pluginname = both.substr (splitIndex + 1);
		if (m_fileformat.empty () || m_pluginname.empty ())
		{
			throw kdbrest::exception::FileformatPluginException ("Neither format, nor plugin may be empty.");
		}
		m_pluginstatuses = statuses;
	}
	/**
                 * Constructor based on the format and the pluginname.
                 * @param format The fileformat as string (e.g. ini, xml)
                 * @param plugin The pluginname as string (e.g. ni, xmltool)
				 * @param statuses The plugin statuses as vector (e.g. maintained, limited)
                 */
	inline PluginFormat (const std::string & format, const std::string & plugin,
			     const std::vector<std::string> statuses = std::vector<std::string> ())
	{
		if (format.empty () || plugin.empty ())
		{
			throw kdbrest::exception::FileformatPluginException ("Neither format, nor plugin may be empty.");
		}
		m_fileformat = std::string (format);
		m_pluginname = std::string (plugin);
		m_pluginstatuses = statuses;
	}

	/**
                 * Getter for the file format as string.
                 * @return File format as string
                 */
	std::string getFileformat ()
	{
		return m_fileformat;
	}

	/**
                 * Getter for the plugin name as string.
                 * @return Plugin name as string
                 */
	std::string getPluginname ()
	{
		return m_pluginname;
	}

	/**
	 * Getter for the plugin statuses as vector.
	 * @return All plugin statuses in a vector
	 */
	std::vector<std::string> getPluginstatuses ()
	{
		return m_pluginstatuses;
	}

private:
	std::string m_fileformat;
	std::string m_pluginname;
	std::vector<std::string> m_pluginstatuses;
};

} // namespace model

} // namespace kdbrest

#endif
