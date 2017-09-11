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
void addKey (YAML::Node data, NameIterator & keyIterator, Key const & key)
{
	if (keyIterator == --key.end ())
	{
		data[*keyIterator] = YAML::Node (key.getString ());
		return;
	}

	YAML::Node dictionary = (data[*keyIterator] && data[*keyIterator].IsMap ()) ? data[*keyIterator] : YAML::Node (YAML::NodeType::Map);
	data[*keyIterator] = dictionary;

	addKey (dictionary, ++keyIterator, key);
}

void addKeys (YAML::Node data, KeySet const & mappings, Key const & parent)
{
	for (auto key : mappings)
	{
		auto parentIterator = parent.begin ();
		auto keyIterator = key.begin ();
		while (parentIterator != parent.end () && keyIterator != key.end ())
		{
			parentIterator++;
			keyIterator++;
		}

		addKey (data, keyIterator, key);
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
	auto data = YAML::Node (YAML::NodeType::Map);
	addKeys (data, mappings, parent);

	output << data;
}
