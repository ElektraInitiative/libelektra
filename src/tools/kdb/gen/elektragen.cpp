#include <utility>

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "elektragen.hpp"

#include <command.hpp>
#include <kdbtypes.h>

#include <memory>
#include <set>

const char * ElektraGenTemplate::Params::InitFunctionName = "initFn";
const char * ElektraGenTemplate::Params::TagPrefix = "tagPrefix";
const char * ElektraGenTemplate::Params::OptimizeFromString = "optimizeFromString";
const char * ElektraGenTemplate::Params::AdditionalHeaders = "headers";

struct EnumTrie
{
	explicit EnumTrie (const std::set<std::pair<std::string, std::string>> & values);

	EnumTrie () : children (), stringValue (), name ()
	{
	}

	std::string createSwitch ();

private:
	std::unordered_map<char, std::unique_ptr<EnumTrie>> children;
	std::string stringValue;
	std::string name;

	void insert (const std::string & prefix, const std::set<std::pair<std::string, std::string>> & values);
	bool createSwitch (std::stringstream & ss, size_t index);
};

static inline const std::unordered_set<std::string> getAllowedTypes ()
{
	return { "enum",
		 "string",
		 "boolean",
		 "char",
		 "octet",
		 "short",
		 "unsigned_short",
		 "long",
		 "unsigned_long",
		 "long_long",
		 "unsigned_long_long",
		 "float",
		 "double",
		 "long_double" };
}

static std::string createIncludeGuard (const std::string & fileName)
{
	std::string result;
	result.resize (fileName.length ());
	std::transform (fileName.begin (), fileName.end (), result.begin (), ::toupper);
	std::replace_if (result.begin (), result.end (), std::not1 (std::ptr_fun (isalnum)), '_');
	return result;
}

static inline bool hasType (const kdb::Key & key)
{
	return key.hasMeta ("type");
}

static inline std::string getType (const kdb::Key & key)
{
	return key.getMeta<std::string> ("type");
}

static std::string getTagName (const kdb::Key & key, const std::string & parentKey, const std::string & prefix)
{
	auto name = key.getName ();
	name.erase (0, parentKey.length () + 1);

	if (name[name.length () - 1] == '#')
	{
		name.erase (name.length () - 1);
	}

	if (name[name.length () - 1] == '/')
	{
		name.erase (name.length () - 1);
	}

	std::replace (name.begin (), name.end (), '/', '_');
	name.erase (std::remove (name.begin (), name.end (), '#'), name.end ());

	return prefix + name;
}

static std::string snakeCaseToCamelCase (const std::string & s)
{
	std::string result;
	result.resize (s.size ());
	auto upcase = true;
	std::transform (s.begin (), s.end (), result.begin (), [&upcase](char c) {
		int x = upcase ? toupper (c) : c;
		upcase = c == '_';
		return x;
	});
	result.erase (std::remove (result.begin (), result.end (), '_'), result.end ());
	return result;
}

static std::string snakeCaseToMacroCase (const std::string & s)
{
	std::string result;
	result.resize (s.size ());
	std::transform (s.begin (), s.end (), result.begin (), ::toupper);
	return result;
}

static std::string camelCaseToMacroCase (const std::string & s)
{
	std::stringstream ss;
	std::for_each (s.begin (), s.end (), [&ss](char c) {
		if (ss.tellp () != std::stringstream::beg && isupper (c))
		{
			ss << '_';
		}
		ss << static_cast<char> (toupper (c));
	});
	return ss.str ();
}

static kainjow::mustache::list getEnumValues (const std::string & prefix, const kdb::Key & key, std::string & fromStringSwitch)
{
	using namespace kainjow::mustache;

	if (!key.hasMeta ("check/enum"))
	{
		return {};
	}

	list values;

	std::set<std::pair<std::string, std::string>> stringValues;

	const auto end = key.getMeta<std::string> ("check/enum");
	kdb::long_long_t i = 0;
	auto cur = "#" + std::to_string (i);
	while (cur <= end)
	{
		if (key.hasMeta ("check/enum/" + cur))
		{
			auto name = prefix + "_";
			const std::string & stringValue = key.getMeta<std::string> ("check/enum/" + cur);
			name += camelCaseToMacroCase (stringValue);
			const auto value = std::to_string (i);
			// TODO: custom values
			values.emplace_back (object{ { "name", name }, { "value", value }, { "string_value", stringValue } });
			stringValues.insert ({ stringValue, name });
		}
		++i;
		cur = "#" + std::to_string (i);
	}

	EnumTrie trie (stringValues);
	fromStringSwitch = trie.createSwitch ();

	return values;
}

static inline std::string getEnumType (const kdb::Key & key, const std::string & tagName, bool & genType)
{
	genType = key.hasMeta ("gen/enum/type");
	return genType ? key.getMeta<std::string> ("gen/enum/type") : snakeCaseToCamelCase (tagName);
}

static inline bool shouldGenerateTypeDef (const kdb::Key & key)
{
	return !key.hasMeta ("gen/enum/create") || key.getMeta<std::string> ("gen/enum/create") == "1";
}

kainjow::mustache::data ElektraGenTemplate::getTemplateData (const std::string & outputName, const kdb::KeySet & ks,
							     const std::string & parentKey) const
{
	// TODO: check illegal names
	if (parentKey[0] != '/')
	{
		throw CommandAbortException ("parentKey has to be cascading");
	}

	using namespace kainjow::mustache;

	auto headerFile = outputName + ".h";
	auto includeGuard = createIncludeGuard (headerFile);
	auto initFunctionName = getParameter (Params::InitFunctionName, "loadConfiguration");
	auto additionalHeaders = split (getParameter (Params::AdditionalHeaders), ',');
	auto optimizeFromString = getParameter (Params::OptimizeFromString, "off") != "off";

	auto data = object{ { "header_file", headerFile },
			    { "include_guard", includeGuard },
			    { "parent_key", parentKey },
			    { "init_function_name", initFunctionName },
			    { "more_headers", list (additionalHeaders.begin (), additionalHeaders.end ()) } };

	auto enums = list{};
	auto keys = list{};

	auto specParent = kdb::Key ("spec" + parentKey, KEY_END);


	std::unordered_map<std::string, std::pair<std::string, std::string>> enumTypes;

	for (const kdb::Key & key : ks)
	{
		if (!key.isSpec () || !key.isBelow (specParent) || !hasType (key))
		{
			continue;
		}

		auto name = key.getName ();
		name.erase (0, sizeof ("spec") - 1);

		if (!key.getMeta<const kdb::Key> ("default"))
		{
			throw CommandAbortException ("The key '" + name + "' doesn't have a default value!");
		}

		auto type = getType (key);

		const auto & allowedTypes = getAllowedTypes ();
		if (allowedTypes.find (type) == allowedTypes.end ())
		{
			auto msg = "The key '" + name;
			msg += "' has an unsupported type ('" + type + "')!";
			throw CommandAbortException (msg);
		}

		auto tagName = getTagName (key, specParent.getName (), getParameter (Params::TagPrefix));
		object keyObject = { { "name", name },
				     { "tag_name", snakeCaseToMacroCase (tagName) },
				     { "type_name", snakeCaseToCamelCase (type) } };

		if (type == "enum")
		{
			bool genType;
			auto enumType = getEnumType (key, tagName, genType);
			auto typeName = "Enum" + enumType;

			auto nativeType = genType ? enumType : "Elektra" + typeName;
			std::string fromStringSwitch;

			auto values = getEnumValues (camelCaseToMacroCase (nativeType), key, fromStringSwitch);

			auto generateTypeDef = shouldGenerateTypeDef (key);
			if (genType && generateTypeDef)
			{
				std::stringstream ss;
				std::for_each (values.begin (), values.end (), [&](const kainjow::mustache::data & d) {
					ss << d.get ("name")->string_value () << "=" << d.get ("value")->string_value () << "\n";
				});
				auto valuesString = "";

				auto other = enumTypes.find (typeName);
				if (other != enumTypes.end ())
				{
					auto otherValuesString = other->second.second;
					if (otherValuesString != valuesString)
					{
						auto otherKey = other->second.first;
						auto msg = "The key '" + name;
						msg += "' uses the same 'gen/enum/type' as the key '" + otherKey +
						       "', but their 'check/enum' values are different!";
						throw CommandAbortException (msg);
					}

					// generate typedef only once
					generateTypeDef = false;
				}

				enumTypes[typeName] = std::make_pair (name, valuesString);
			}

			keyObject["type_name"] = typeName;

			enums.emplace_back (object{ { "name", name },
						    { "tag_name", snakeCaseToMacroCase (tagName) },
						    { "type_name", typeName },
						    { "native_type", nativeType },
						    { "generate_typedef?", generateTypeDef },
						    { "values", values },
						    { "switch_from_string?", optimizeFromString },
						    { "from_key", "elektraKeyTo" + nativeType },
						    { "to_string", "elektra" + nativeType + "ToString" },
						    { "from_string_code", fromStringSwitch } });
		}

		keys.emplace_back (keyObject);
	}

	data["keys_count"] = std::to_string (keys.size ());
	data["keys"] = keys;
	data["enums"] = enums;

	return data;
}

EnumTrie::EnumTrie (const std::set<std::pair<std::string, std::string>> & values)
{
	for (auto v = values.begin (); v != values.end ();)
	{
		char c = v->first.at (0);
		std::string p;
		if (c != '\0')
		{
			p += c;
		}
		std::set<std::pair<std::string, std::string>> vals;

		while (v != values.end () && v->first.at (0) == c)
		{
			vals.insert (*v);
			++v;
		}
		insert (p, vals);
	}
}

void EnumTrie::insert (const std::string & prefix, const std::set<std::pair<std::string, std::string>> & values)
{
	std::unique_ptr<EnumTrie> child (new EnumTrie ());
	if (values.size () == 1)
	{
		child->stringValue = values.begin ()->first;
		child->name = values.begin ()->second;
	}
	else
	{
		for (auto v = values.begin (); v != values.end ();)
		{
			if (v->first.size () == prefix.size ())
			{
				child->stringValue = values.begin ()->first;
				child->name = values.begin ()->second;
				++v;
				continue;
			}

			char c = v->first.at (prefix.size ());
			std::string p = prefix;
			p += c;
			std::set<std::pair<std::string, std::string>> vals;

			while (v != values.end () && (v->first.size () > prefix.size () ? v->first.at (prefix.size ()) : '\0') == c)
			{
				vals.insert (*v);
				++v;
			}
			child->insert (p, vals);
		}
	}

	children[prefix.back ()] = std::move (child);
}

std::string EnumTrie::createSwitch ()
{
	std::stringstream ss;
	createSwitch (ss, 0);
	return ss.str ();
}

bool EnumTrie::createSwitch (std::stringstream & ss, size_t index)
{
	if (children.empty ())
	{
		if (stringValue.empty ())
		{
			return false;
		}

		ss << "return " << name << ";" << std::endl;
		return false;
	}

	ss << "switch (string[" << index << "])" << std::endl;
	ss << "{" << std::endl;
	for (auto & child : children)
	{
		ss << "case '" << child.first << "':" << std::endl;
		if (child.second->createSwitch (ss, index + 1))
		{
			ss << "break;" << std::endl;
		}
	}
	ss << "}" << std::endl;

	if (!stringValue.empty ())
	{
		ss << "return " << name << ";" << std::endl;
		return false;
	}
	return true;
}
