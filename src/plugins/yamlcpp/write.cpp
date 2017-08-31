/**
 * @file
 *
 * @brief Write key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "write.hpp"
#include "yaml.h"

#include <kdbease.h>
#include <kdblogger.h>
#include <kdbplugin.h>

#include <fstream>

using namespace std;
using namespace kdb;

namespace
{
/**
* @brief Save a key set in a YAML emitter
*
* @param mappings The key set that should be stored in `emitter`
* @param parent This key stores the common prefix for the key name
* @param node This emitter stores the converted key set
*/
void convertKeySetToEmitter (KeySet const & mappings, Key const & parent, YAML::Emitter & emitter)
{
	for (auto key : mappings)
	{
		const char * name = elektraKeyGetRelativeName (key.getKey (), parent.getKey ());
		emitter << YAML::Key << name << YAML::Value << key.get<string> ();
		ELEKTRA_LOG_DEBUG ("%s: %s", key.get<string> ().c_str (), key.getName ().c_str ());
	}
}
} // end namespace

/**
 * @brief This function saves the key-value pairs stored in `mappings` as YAML data in the location specified via `parent`.
 *
 * @param mappings This key set stores the mappings that should be saved as YAML data.
 * @param parent This key specifies the path to the YAML data file that should be written.
 */
void yamlcpp::yamlWrite (KeySet const & mappings, Key const & parent)
{
	ofstream output (parent.getString ());
	YAML::Emitter emitter (output);
	emitter << YAML::BeginMap;

	convertKeySetToEmitter (mappings, parent, emitter);

	emitter << YAML::EndMap;
}
