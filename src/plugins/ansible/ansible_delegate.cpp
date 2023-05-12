/**
 * @file
 *
 * @brief Delegate implementation for the `ansible` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "ansible_delegate.hpp"
#include <fstream>
#include <kdbprivate.h> // ksBelow
#include <keyset.hpp>
#include <regex>
#include <sstream>
#include <vector>
#include <yaml-cpp/yaml.h>

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

	if (!configuration.lookup ("/playbook/name").isNull ())
	{
		playbook_name = configuration.lookup ("/playbook/name").getString ();
	}

	if (!configuration.lookup ("/playbook/hosts").isNull ())
	{
		hosts = configuration.lookup ("/playbook/hosts").getString ();
	}

	if (!configuration.lookup ("/playbook").isNull ())
	{
		auto value = configuration.lookup ("/playbook").getString ();
		std::transform (value.begin (), value.end (), value.begin (), ::toupper);
		only_tasks = value == "FALSE";
	}

	if (!configuration.lookup ("/task/main/name").isNull ())
	{
		main_task_name = configuration.lookup ("/task/main/name").getString ();
	}
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
	if (nameIterator == key.end ())
	{
		std::string value = key.getString ();
		if (key.getNamespace () != kdb::ElektraNamespace::META && key.hasMeta ("meta:/elektra/export/variable"))
		{
			auto variableName = key.getMeta<std::string> ("meta:/elektra/export/variable");
			std::stringstream ansibleVariableStream;
			ansibleVariableStream << "{{ " << variableName << " | default('" << value << "') }}";
			value = ansibleVariableStream.str ();
		}

		ckdb::KeySet * metaKeys = ckdb::keyMeta (*key);

		if (ckdb::ksLookupByName (metaKeys, "meta:/elektra/deleted", 0) != nullptr)
		{
			// This key should be deleted.
			YAML::Node removedNode;
			removedNode["remove"] = true;
			parent.push_back (removedNode);
		}
		else
		{
			// Set value of the node
			YAML::Node valueNode;
			valueNode["value"] = value;
			parent.push_back (valueNode);

			// Append meta keys
			if (ckdb::ksGetSize (metaKeys) > 0)
			{
				YAML::Node metaNode;
				for (ssize_t it = 0; it < ckdb::ksGetSize (metaKeys); ++it)
				{
					const kdb::Key curMeta = ckdb::ksAtCursor (metaKeys, it);
					auto metaIterator = curMeta.begin ();
					metaIterator++;
					addKey (metaNode, curMeta, metaIterator);
				}

				YAML::Node mnode;
				mnode["meta"] = metaNode;
				parent.push_back (mnode);
			}
		}

		return;
	}

	auto part = *nameIterator;
	// replace all occurences of / with \/
	// this is crucial when exporting mountpoints
	// otherwise, system:/mountpoint/user:\/test becomes system:/mountpoint/user:/test in the YAML file
	part = std::regex_replace (part, std::regex ("/"), "\\/");

	YAML::Node node = parent[part] ? parent[part] : YAML::Node ();
	if (node.IsSequence ())
	{
		// Look whether the sequence already has a "keys" entry
		for (auto element : node)
		{
			if (element["keys"])
			{
				YAML::Node keyNode (element);
				auto kn = keyNode["keys"];
				addKey (kn, key, ++nameIterator);
				return;
			}
		}

		YAML::Node keysNode;
		node.push_back (keysNode);

		auto kn = keysNode["keys"];
		addKey (kn, key, ++nameIterator);
		return;
	}
	else
	{
		parent[part] = node;
	}

	++nameIterator;
	addKey (node, key, nameIterator);
}

void addNamespace (YAML::Emitter & out, kdb::KeySet const & keySet)
{
	YAML::Node keysNode;

	for (const auto & key : keySet)
	{
		if (key.isString ())
		{
			auto nameIterator = key.begin ();
			// The first part of the iterator is the namespace in binary form -> skip
			nameIterator++;
			addKey (keysNode, key, nameIterator);
		}
	}

	out << keysNode;
}

void createTask (YAML::Emitter & out, kdb::KeySet const & keySet, std::string const & taskName)
{
	out << YAML::BeginMap;
	out << YAML::Key << "name";
	out << YAML::Value << taskName;
	out << YAML::Key << "elektra";

	out << YAML::BeginMap;
	out << YAML::Key << "keys";
	out << YAML::Value;
	out << YAML::BeginSeq;

	kdb::Key namespaceKey;
	for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ns++)
	{
		ckdb::keySetNamespace (*namespaceKey, ns);
		kdb::KeySet below = ckdb::ksBelow (keySet.getKeySet (), *namespaceKey);
		if (below.size () > 0)
		{
			out << YAML::BeginMap;
			auto nskn = namespaceKey.getName ().substr (0, namespaceKey.getName ().find (':'));
			out << YAML::Key << nskn;
			out << YAML::Value;

			addNamespace (out, below);
			out << YAML::EndMap;
		}
	}

	out << YAML::EndSeq; // keys
	out << YAML::EndMap; // elektra
	out << YAML::EndMap;
}

void AnsibleDelegate::createPlaybook (kdb::KeySet & keySet, kdb::Key const & parentKey)
{
	// Cut out oll the blacklisted keys
	for (const auto & key : blacklist)
	{
		keySet.cut (key);
	}

	if (keySet.size () == 0)
	{
		// If keyset is empty, no need to do anything
		return;
	}

	YAML::Emitter out;

	out << YAML::BeginDoc;

	if (!only_tasks)
	{
		out << YAML::BeginSeq;
		out << YAML::BeginMap;

		out << YAML::Key << "name";
		out << YAML::Value << playbook_name;

		out << YAML::Key << "hosts";
		out << YAML::Value << hosts;

		out << YAML::Key << "collections";
		out << YAML::BeginSeq;
		out << YAML::Value << "elektra_initiative.libelektra";
		out << YAML::EndSeq;

		out << YAML::Key << "tasks";
		out << YAML::Value;
	}

	out << YAML::BeginSeq;

	// Create a separate task for mountpoints
	// In a perfect world, we'd just use the 'mounts' section of the Ansible module
	// However, therefore we'd need to create our own mountpoints parser here.
	// Also, if this exports from a recording session, there's a high likelihood that we only get partial mountpoints
	// The reason it is a separate task here is that for the "real" keys we already want to use the mountpoints.
	// Alternatively, we could modify the Ansible module so that it internally separates system:/elektra/mountpoint keys from the rest.
	auto mountpointKeys = keySet.cut ("system:/elektra/mountpoints");
	if (mountpointKeys.size () > 0)
	{
		createTask (out, mountpointKeys, "Mount Configuration");
	}

	auto recordKeys = keySet.cut ("/elektra/record");

	if (keySet.size () > 0)
	{
		createTask (out, keySet, main_task_name);
	}

	// We don't need to transfer record config
	recordKeys.cut ("/elektra/record/config");
	if (recordKeys.size () > 0)
	{
		createTask (out, recordKeys, "Set session recording state");
	}

	out << YAML::EndSeq; // tasks

	if (!only_tasks)
	{
		out << YAML::EndMap;
		out << YAML::EndSeq;
	}

	out << YAML::EndDoc;

	std::ofstream output (parentKey.getString ());
	output << out.c_str () << std::endl;
}


} // end namespace elektra
