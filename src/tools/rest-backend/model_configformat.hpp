/**
 * @file
 *
 * @brief model for configuration + format
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_REST_MODEL_CONFIGFORMAT_HPP
#define ELEKTRA_REST_MODEL_CONFIGFORMAT_HPP

#include <string>

#include <model_pluginformat.hpp>

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
 * @brief model class for a pluginformat and config snippet
 * 
 * the model encapsulates a pluginformat and a config snippet in
 * string representation. the pluginformat tells in what format
 * the snippet is in. additionally, validation information is
 * available (used during output when round-trip checks are performed).
 */
class ConfigFormat
{

public:
	/**
	 * @brief standard constructor
	 */
	ConfigFormat ()
	{
	}

	/**
	 * @brief constructor based on pluginformat and configuration.
	 * 
	 * this constructor is used to represent a fully converted configuration
     * along with its file format, so it can be used for further
     * operations.
	 * 
     * @param pf The PluginFormat to be used
     * @param cfg The configuration in the given format as string
	 * @param validated If the configuration snippet has successfully
	 *	      passed a validation round-trip
     */
	ConfigFormat (const PluginFormat & pf, const std::string & cfg, const bool validated = false)
	: m_pluginformat (pf), m_config (cfg), m_validated (validated)
	{
	}

	/**
	 * @brief getter for the pluginformat
	 * 
     * @return PluginFormat that is used
     */
	PluginFormat getPluginformat () const
	{
		return m_pluginformat;
	}

	/**
     * @brief getter for the converted configuration as string
	 * 
     * @return Configuration as string
     */
	std::string getConfig () const
	{
		return m_config;
	}

	/**
	 * @brief getter for the validation status
	 * 
	 * @return True if conversion has passed validation
	 */
	bool isValidated () const
	{
		return m_validated;
	}

	/**
	 * @brief setter for validation status
	 * 
	 * @param val If the conversion has passed validation
	 */
	void setValidated (bool val)
	{
		this->m_validated = val;
	}

private:
	PluginFormat m_pluginformat;
	std::string m_config;
	bool m_validated;
};

} // namespace model

} // namespace kdbrest

#endif
