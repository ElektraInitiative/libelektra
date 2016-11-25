#ifndef ELEKTRA_REST_MODEL_PLUGINFORMAT_HEADER_GUARD
#define ELEKTRA_REST_MODEL_PLUGINFORMAT_HEADER_GUARD

#include <string>
#include <vector>

#include <exceptions.hpp>
#include <kdb_includes.hpp>

/**
 * @brief main namespace for the REST service
 */
namespace kdbrest
{

/**
 * @brief namespace for models
 */
namespace model
{

/**
 * @brief model class for a plugin name along with its format
 * 
 * this class encapsulates all information that is necessary to
 * find a plugin and configuration snippets supported by it.
 */
class PluginFormat
{

public:
	/**
	 * @brief standard constructor
	 */
	inline PluginFormat ()
	{
	}

	/**
     * @brief copy constructor
	 * 
     * @param pf The foreign PluginFormat
     */
	inline PluginFormat (const PluginFormat & pf)
	{
		m_fileformat = pf.m_fileformat;
		m_pluginname = pf.m_pluginname;
		m_pluginstatuses = std::vector<std::string> (pf.m_pluginstatuses);
	}

	/**
	 * @brief constructor based on the format and the pluginname
	 * 
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
     * @brief getter for the file format as string
	 * 
     * @return File format as string
     */
	std::string getFileformat ()
	{
		return m_fileformat;
	}

	/**
     * @brief getter for the plugin name as string
	 * 
     * @return Plugin name as string
     */
	std::string getPluginname ()
	{
		return m_pluginname;
	}

	/**
	 * @brief getter for the plugin statuses as vector
	 * 
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
