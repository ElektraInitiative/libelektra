#ifndef ELEKTRA_REST_MODEL_IMPORTEDCONFIG_HEADER_GUARD
#define ELEKTRA_REST_MODEL_IMPORTEDCONFIG_HEADER_GUARD

#include <kdb_includes.hpp>
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
 * @brief model class for imported configuration snippets
 * 
 * this class encapsulates a pluginformat and a keyset representing
 * a configuration snippet.
 */
class ImportedConfig
{

public:
	/**
	 * @brief constructor based on a PluginFormat and keyset
	 * 
     * It is used to represent a fully imported configuration
     * along with its file format, so it can be used for further
     * operations.
	 * 
     * @param pf The PluginFormat to be used
     * @param ks The configuration as kdb::Keys in a kdb::KeySet
     */
	inline ImportedConfig (const PluginFormat & pf, kdb::KeySet & ks) : m_pluginformat (pf), m_ks (ks)
	{
	}

	/**
     * @brief getter for the PluginFormat
	 * 
     * @return PluginFormat that is used
     */
	PluginFormat & getPluginformat ()
	{
		return m_pluginformat;
	}

	/**
     * @brief getter for the kdb::KeySet
	 * 
     * @return kdb::KeySet that contains the configuration keys
     */
	kdb::KeySet & getKeySet ()
	{
		return m_ks;
	}

private:
	PluginFormat m_pluginformat;
	kdb::KeySet m_ks;
};

} // namespace model

} // namespace kdbrest

#endif
