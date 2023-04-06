/**
 * @file
 *
 * @brief Delegate implementation for the `ansible` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "ansible_delegate.hpp"
#include <yaml-cpp/yaml.h>
#include <fstream>
#include <sstream>
#include <kdbprivate.h> // ksBelow

using kdb::Key;
using kdb::KeySet;

namespace elektra
{

/**
 * @brief This constructor creates a new delegate object used by the `ansible` plugin
 *
 * @param config This key set contains configuration values provided by the `ansible` plugin
 */
AnsibleDelegate::AnsibleDelegate (KeySet config)
{
	configuration = config;
}

/**
 * @brief This method returns the configuration of the plugin, prefixing key names with the name of `parent`.
 *
 *  This is only an example to show you how to use the delegate. You can add any method you want here and then call it in
 *  `ansible.cpp` via `delegator::get (handle)->functionName(parameter1, parameter2, …)`.
 *
 * @param parent This key specifies the name this function adds to the stored configuration values.
 *
 * @return A key set storing the configuration values of the plugin
 */
kdb::KeySet AnsibleDelegate::getConfig (Key const & parent)
{
	KeySet keys;

	for (auto configKey : configuration)
	{
		Key key{ parent.getName (), KEY_END };
		key.addBaseName (configKey.getBaseName ());
		if (configKey.isString ()) key.setString (configKey.getString ());
		keys.append (key);
	}

	return keys;
}

void addKey (YAML::Node & parent, kdb::Key const & key, kdb::NameIterator & nameIterator)
{
	if (nameIterator == key.end())
	{
		std::string value = key.getString ();
		if (key.getNamespace() != kdb::ElektraNamespace::META && key.hasMeta ("meta:/elektra/export/variable"))
		{
			auto variableName = key.getMeta<std::string>("meta:/elektra/export/variable");
			std::stringstream ansibleVariableStream;
			ansibleVariableStream << "{{ " << variableName << " | default('" << value << "') }}";
			value = ansibleVariableStream.str();
		}

		ckdb::KeySet * metaKeys = ckdb::keyMeta (*key);

		// Set value of the node
		bool needsValueNode = key.getNamespace() != kdb::ElektraNamespace::META;

		if (needsValueNode)
		{
			parent["value"] = value;
		}
		else
		{
			parent = value;
		}

		// Append meta keys
		if (ckdb::ksGetSize (metaKeys) > 0)
		{
			auto metaNode = parent["meta"];

			for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
			{
				const kdb::Key curMeta = ckdb::ksAtCursor (metaKeys, it);
				auto metaIterator = curMeta.begin();
				metaIterator++;
				addKey (metaNode, curMeta, metaIterator);
			}
		}

		return;
	}

	auto part = *nameIterator;
	YAML::Node node = parent[part] ? parent[part] : YAML::Node ();
	addKey (node, key, ++nameIterator);
	parent[part] = node;
}

void addMountpoint (YAML::Emitter & out, kdb::KeySet const & keySet, kdb::Key const & parentKey)
{
	std::stringstream taskNameStream;
	taskNameStream  << "Set Elektra Keys (" << parentKey.getName().substr (0, parentKey.getName().find (':')) << ")";
	auto taskName = taskNameStream.str();

	out << YAML::BeginMap;
	out << YAML::Key << "name";
	out << YAML::Value << taskName;
	out << YAML::Key << "mountpoint";
	out << YAML::Value << parentKey.getName();
	out << YAML::Key << "keys";

	YAML::Node keysNode;

	for (const auto & key : keySet)
	{
		if (key.isString())
		{
			auto nameIterator = key.begin();
			nameIterator++;
			addKey (keysNode, key, nameIterator);
		}
	}

	out << keysNode;

	out << YAML::EndMap;
}

void AnsibleDelegate::createPlaybook (kdb::KeySet const & keySet, kdb::Key const & parentKey)
{
	std::string hosts = "all";
	if (parentKey.hasMeta ("meta:/ansible/hosts"))
	{
		hosts = parentKey.getMeta<std::string>("meta:/ansible/hosts");
	}

	std::string name = "My Elektra Ansible Playbook";
	if (parentKey.hasMeta ("meta:/ansible/name"))
	{
		hosts = parentKey.getMeta<std::string>("meta:/ansible/name");
	}

	YAML::Emitter out;

	out << YAML::BeginDoc;

	out << YAML::BeginSeq;
	out << YAML::BeginMap;

	out << YAML::Key << "name";
	out << YAML::Value << name;

	out << YAML::Key << "hosts";
	out << YAML::Value << hosts;

	out << YAML::Key << "collections";
	out << YAML::BeginSeq;
	out << YAML::Value << "elektra_initiative.libelektra";
	out << YAML::EndSeq;

	out << YAML::Key << "tasks";
	out << YAML::Value;
	out << YAML::BeginSeq;

	kdb::Key namespaceKey ("/");
	for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ns++)
	{
		ckdb::keySetNamespace (*namespaceKey, ns);
		kdb::KeySet below = ckdb::ksBelow (keySet.getKeySet(), *namespaceKey);
		if (below.size() > 0)
		{
			addMountpoint (out, below, namespaceKey);
		}
	}

	out << YAML::EndSeq;
	out << YAML::EndMap;
	out << YAML::EndSeq;

	out << YAML::EndDoc;

	std::ofstream output (parentKey.getString ());
	output << out.c_str() << std::endl;
}


} // end namespace elektra
