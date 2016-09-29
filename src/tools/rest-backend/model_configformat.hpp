#ifndef ELEKTRA_REST_MODEL_CONFIGFORMAT_HEADER_GUARD
#define ELEKTRA_REST_MODEL_CONFIGFORMAT_HEADER_GUARD

#include <string>

#include "model_pluginformat.hpp"

namespace kdbrest
{

namespace model
{

class ConfigFormat
{

public:
	inline ConfigFormat ()
	{
	}
	/**
                 * Constructor based on a PluginFormat and a configuration.
                 * It is used to represent a fully converted configuration
                 * along with its file format, so it can be used for further
                 * operations.
                 * @param pf The PluginFormat to be used
                 * @param cfg The configuration in the given format as string
                 */
	inline ConfigFormat (const PluginFormat & pf, const std::string & cfg) : m_pluginformat (pf), m_config (cfg)
	{
	}

	/**
                 * Getter for the PluginFormat.
                 * @return PluginFormat that is used
                 */
	PluginFormat & getPluginformat ()
	{
		return m_pluginformat;
	}

	/**
                 * Getter for the converted configuration as string.
                 * @return Configuration as string
                 */
	std::string & getConfig ()
	{
		return m_config;
	}

private:
	PluginFormat m_pluginformat;
	std::string m_config;
};

} // namespace model

} // namespace kdbrest

#endif
