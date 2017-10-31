#ifndef ELEKTRA_REST_MODEL_PLUGINFORMAT_HEADER_GUARD
#define ELEKTRA_REST_MODEL_PLUGINFORMAT_HEADER_GUARD

#include <string>
#include <vector>

#include <exceptions.hpp>
#include <helper/keyhelper.hpp>
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
	PluginFormat ()
	{
	}

	/**
     * @brief copy constructor
	 * 
     * @param pf The foreign PluginFormat
     */
	PluginFormat (const PluginFormat & pf)
	{
		m_fileformat = pf.m_fileformat;
		m_pluginname = pf.m_pluginname;
		m_pluginstatuses = std::vector<std::string> (pf.m_pluginstatuses);
		m_config = kdb::KeySet (pf.m_config);
	}

	/**
	 * @brief constructor based on the format and the pluginname
	 * 
     * @param format The fileformat as string (e.g. ini, xml)
     * @param plugin The pluginname as string (e.g. ni, xmltool)
	 * @param statuses The plugin statuses as vector (e.g. maintained, limited)
     */
	PluginFormat (const std::string & format, const std::string & plugin,
		      const std::vector<std::string> statuses = std::vector<std::string> (), const kdb::KeySet config = kdb::KeySet ())
	{
		if (format.empty () || plugin.empty ())
		{
			throw kdbrest::exception::FileformatPluginException ("Neither format, nor plugin may be empty.");
		}
		m_fileformat = std::string (format);
		m_pluginname = std::string (plugin);
		m_pluginstatuses = statuses;
		m_config = config;
	}

	/**
     * @brief getter for the file format as string
	 * 
     * @return File format as string
     */
	std::string getFileformat () const
	{
		return m_fileformat;
	}

	/**
     * @brief getter for the plugin name as string
	 * 
     * @return Plugin name as string
     */
	std::string getPluginname () const
	{
		return m_pluginname;
	}

	/**
	 * @brief getter for the plugin name with appended config params
	 * 
	 * example:
	 * - plugin name: simpleini
	 * - plugin config:
	 *     system/format = % %
	 * - returns: simpleini format=% %
	 * 
	 * @return plugin name with config params
	 */
	std::string getPluginnameWithConfig () const
	{
		std::string result = m_pluginname;
		for (auto elem : m_config)
		{
			result.append (" " + elem.getName ().substr (elem.getNamespace ().length () + 1) + "=" + elem.getString ());
		}
		return result;
	}

	/**
	 * @brief getter for the plugin statuses as vector
	 * 
	 * @return All plugin statuses in a vector
	 */
	std::vector<std::string> getPluginstatuses () const
	{
		return m_pluginstatuses;
	}

	/**
	 * @brief getter for the plugin configuration
	 * 
	 * @return A keyset containing the plugin config
	 */
	const kdb::KeySet getConfig () const
	{
		return m_config;
	}

private:
	std::string m_fileformat;
	std::string m_pluginname;
	std::vector<std::string> m_pluginstatuses;
	kdb::KeySet m_config;
};

} // namespace model

} // namespace kdbrest

#endif
