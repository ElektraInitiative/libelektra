#ifndef ELEKTRA_REST_MODEL_IMPORTEDCONFIG_HEADER_GUARD
#define ELEKTRA_REST_MODEL_IMPORTEDCONFIG_HEADER_GUARD

#include <kdb_includes.hpp>
#include <model_pluginformat.hpp>

namespace kdbrest
{

namespace model
{

class ImportedConfig
{

public:
	/**
                 * Constructor based on a PluginFormat and a configuration in
                 * form of a kdb::KeySet.
                 * It is used to represent a fully imported configuration
                 * along with its file format, so it can be used for further
                 * operations.
                 * @param pf The PluginFormat to be used
                 * @param ks The configuration as kdb::Keys in a kdb::KeySet
                 */
	inline ImportedConfig (const PluginFormat & pf, kdb::KeySet & ks) : m_pluginformat (pf), m_ks (ks)
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
                 * Getter for the kdb::KeySet.
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
