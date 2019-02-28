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
#include <modules.hpp>

#include <fstream>
#include <memory>
#include <set>
#include <streambuf>
#include <string>

const char * ElektraGenTemplate::Params::InitFunctionName = "initFn";
const char * ElektraGenTemplate::Params::TagPrefix = "tagPrefix";
const char * ElektraGenTemplate::Params::OptimizeFromString = "optimizeFromString";
const char * ElektraGenTemplate::Params::AdditionalHeaders = "headers";
const char * ElektraGenTemplate::Params::ExperimentalStructs = "structs";

class EnumTrie
{
public:
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

class EnumProcessor
{
private:
	std::unordered_map<std::string, std::pair<std::string, std::string>> enumTypes;

	static kainjow::mustache::list getValues (const std::string & prefix, const kdb::Key & key, std::string & fromStringSwitch,
						  std::string & valuesString);

	static inline bool shouldGenerateTypeDef (const kdb::Key & key);
	static inline std::string getType (const kdb::Key & key, const std::string & tagName, bool & genType);

public:
	kainjow::mustache::object process (const kdb::Key & key, const std::string & tagName);
};

class StructProcessor
{
private:
	std::unordered_map<std::string, std::pair<std::string, std::string>> structTypes;

	static kainjow::mustache::list getFields (const kdb::Key & parentKey, const kdb::KeySet & keys, bool allocating,
						  size_t & maxFieldNameLen, std::string & fieldsString);

	static inline std::string getFieldName (const kdb::Key & key);
	static inline bool shouldGenerateTypeDef (const kdb::Key & key);
	static inline bool shouldAllocate (const kdb::Key & key);
	static inline std::string getType (const kdb::Key & key, const std::string & tagName, bool & genType);

public:
	kainjow::mustache::object process (const kdb::Key & key, const kdb::KeySet & subkeys, const std::string & tagName);
};

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

static std::string keySetToCCode (kdb::KeySet set)
{
	using namespace kdb;
	using namespace kdb::tools;

	Modules modules;
	PluginPtr plugin = modules.load ("c", KeySet ());

	char file[PATH_MAX] = "/tmp/elektra.elektragen.XXXXXX";
	mkstemp (file);

	Key errorKey ("", KEY_VALUE, file, KEY_END);
	plugin->set (set, errorKey);

	std::ifstream is (file);
	return std::string ((std::istreambuf_iterator<char> (is)), std::istreambuf_iterator<char> ());
}

kainjow::mustache::list EnumProcessor::getValues (const std::string & prefix, const kdb::Key & key, std::string & fromStringSwitch,
						  std::string & valuesString)
{
	using namespace kainjow::mustache;

	if (!key.hasMeta ("check/enum"))
	{
		return {};
	}

	list values;
	std::stringstream ss;
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
			ss << name << "=" << value << "\n";
		}
		++i;
		cur = "#" + std::to_string (i);
	}

	EnumTrie trie (stringValues);
	fromStringSwitch = trie.createSwitch ();

	valuesString = ss.str ();

	return values;
}

std::string EnumProcessor::getType (const kdb::Key & key, const std::string & tagName, bool & genType)
{
	genType = key.hasMeta ("gen/enum/type");
	return genType ? key.getMeta<std::string> ("gen/enum/type") : snakeCaseToCamelCase (tagName);
}

bool EnumProcessor::shouldGenerateTypeDef (const kdb::Key & key)
{
	return !key.hasMeta ("gen/enum/create") || key.getMeta<std::string> ("gen/enum/create") == "1";
}

kainjow::mustache::object EnumProcessor::process (const kdb::Key & key, const std::string & tagName)
{
	using namespace kainjow::mustache;

	auto name = key.getName ();
	name.erase (0, sizeof ("spec") - 1);

	bool genType;
	auto enumType = getType (key, tagName, genType);
	auto typeName = "Enum" + enumType;

	auto nativeType = genType ? enumType : "Elektra" + typeName;
	std::string fromStringSwitch;

	std::string valuesString;
	auto values = getValues (camelCaseToMacroCase (nativeType), key, fromStringSwitch, valuesString);

	auto generateTypeDef = shouldGenerateTypeDef (key);
	if (genType && generateTypeDef)
	{
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

	return object{ { "name", name },
		       { "tag_name", snakeCaseToMacroCase (tagName) },
		       { "type_name", typeName },
		       { "native_type", nativeType },
		       { "generate_typedef?", generateTypeDef },
		       { "values", values },
		       { "from_string_code", fromStringSwitch } };
}

std::string StructProcessor::getType (const kdb::Key & key, const std::string & tagName, bool & genType)
{
	genType = key.hasMeta ("gen/struct/type");
	return genType ? key.getMeta<std::string> ("gen/struct/type") : snakeCaseToCamelCase (tagName);
}

bool StructProcessor::shouldGenerateTypeDef (const kdb::Key & key)
{
	return !key.hasMeta ("gen/struct/create") || key.getMeta<std::string> ("gen/struct/create") == "1";
}

bool StructProcessor::shouldAllocate (const kdb::Key & key)
{
	return key.hasMeta ("gen/struct/alloc") && key.getMeta<std::string> ("gen/struct/alloc") == "1";
}

std::string StructProcessor::getFieldName (const kdb::Key & key)
{
	return key.hasMeta ("gen/struct/field") ? key.getMeta<std::string> ("gen/struct/field") : key.getBaseName ();
}

kainjow::mustache::list StructProcessor::getFields (const kdb::Key & parentKey, const kdb::KeySet & keys, bool allocating,
						    size_t & maxFieldNameLen, std::string & fieldsString)
{
	using namespace kainjow::mustache;

	list fields;
	std::stringstream ss;

	maxFieldNameLen = 0;
	for (const kdb::Key & key : keys)
	{
		const std::string & keyName = key.getName ();

		auto name = getFieldName (key);
		maxFieldNameLen = std::max (maxFieldNameLen, name.size ());

		const std::string & type = ::getType (key);

		std::unordered_set<std::string> allowedTypes = { "struct_ref", "enum",		"string",     "boolean",
								 "char",       "octet",		"short",      "unsigned_short",
								 "long",       "unsigned_long", "long_long",  "unsigned_long_long",
								 "float",      "double",	"long_double" };

		if (allowedTypes.find (type) == allowedTypes.end ())
		{
			auto msg = "The key '" + keyName;
			msg += "' has an unsupported type ('" + type + "')!";
			throw CommandAbortException (msg);
		}

		if (type == "struct")
		{
			auto msg = "The key '" + keyName;
			msg += "' has an unsupported type ('" + type + "')! Cannot have structs inside structs, please use struct_ref.";
			throw CommandAbortException (msg);
		}

		auto isStruct = type == "struct_ref";
		auto allocate = type != "struct_ref" || shouldAllocate (key);

		// TODO: resolve struct_ref and change:
		//  - allocate (only if gen/struct/alloc not given on ref)
		//  - typeName
		//  - native_type
		//  to the values of the referenced struct

		if (!allocating && isStruct)
		{
			auto msg = "Cannot have struct_refs inside non-allocating structs. The key '" + keyName;
			msg += "' is a struct_ref appearing inside '" + parentKey.getName () + ", which is a non-allocating struct.";
			throw CommandAbortException (msg);
		}

		auto typeName = snakeCaseToCamelCase (type);
		auto nativeType = type == "string" ? "const char *" : "kdb_" + type + "_t";

		if (isStruct)
		{
			std::cout << "Warning: struct_ref fields not fully supported; generating void* field" << std::endl;
			nativeType = "void *";
		}

		fields.emplace_back (object{ { "name", name },
					     { "key_name", keyName },
					     { "native_type", nativeType },
					     { "type_name", typeName },
					     { "alloc?", allocate },
					     { "is_struct?", isStruct } });

		ss << nativeType << " " << name << "\n";
	}

	fieldsString = ss.str ();

	return fields;
}

kainjow::mustache::object StructProcessor::process (const kdb::Key & key, const kdb::KeySet & subkeys, const std::string & tagName)
{
	using namespace kainjow::mustache;

	auto name = key.getName ();
	name.erase (0, sizeof ("spec") - 1);

	bool genType;
	auto structType = getType (key, tagName, genType);
	auto typeName = "Struct" + structType;

	auto nativeType = genType ? structType : "Elektra" + typeName;

	std::string fieldsString;
	size_t maxFieldNameLen;

	auto allocate = shouldAllocate (key);
	auto fields = getFields (key, subkeys, allocate, maxFieldNameLen, fieldsString);

	auto generateTypeDef = shouldGenerateTypeDef (key);
	if (genType && generateTypeDef)
	{
		auto other = structTypes.find (typeName);
		if (other != structTypes.end ())
		{
			auto otherFieldsString = other->second.second;
			if (otherFieldsString != fieldsString)
			{
				auto otherKey = other->second.first;
				auto msg = "The key '" + name;
				msg += "' uses the same 'gen/struct/type' as the key '" + otherKey + "', but their fields are different!";
				throw CommandAbortException (msg);
			}

			// generate typedef only once
			generateTypeDef = false;
		}

		structTypes[typeName] = std::make_pair (name, fieldsString);
	}

	return object{ { "type_name", typeName },
		       { "native_type", nativeType },
		       { "generate_typedef?", generateTypeDef },
		       { "fields", fields },
		       { "max_field_len", std::to_string (maxFieldNameLen) },
		       { "alloc?", allocate } };
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
	auto optimizeFromString = getParameter (Params::OptimizeFromString, "on") != "off";
	auto experimentalStructs = getParameter (Params::ExperimentalStructs, "") == "on";

	auto data = object{ { "header_file", headerFile },
			    { "include_guard", includeGuard },
			    { "parent_key", parentKey },
			    { "init_function_name", initFunctionName },
			    { "generate_structs?", experimentalStructs },
			    { "switch_from_string?", optimizeFromString },
			    { "more_headers", list (additionalHeaders.begin (), additionalHeaders.end ()) } };

	list enums;
	list structs;
	list keys;

	auto specParent = kdb::Key ("spec" + parentKey, KEY_END);

	EnumProcessor enumProcessor;
	StructProcessor structProcessor;

	kdb::KeySet spec;

	for (auto it = ks.begin (); it != ks.end (); ++it)
	{
		kdb::Key key = *it;

		if (!key.isSpec () || !key.isBelow (specParent) || !hasType (key))
		{
			continue;
		}
		spec.append (key);

		auto name = key.getName ();
		name.erase (0, sizeof ("spec") - 1);

		if (name.find ("/_/") != std::string::npos || name.find ("/_") == name.size () - 2 || name.find ("_/") == 0)
		{
			// TODO: dynamic tags
			std::cout << "Warning: Ignoring globbed key '" << name << "'; currently unsupported" << std::endl;
			continue;
		}

		if (name.find ("/#/") != std::string::npos || name.find ("/#") == name.size () - 2 || name.find ("#/") == 0)
		{
			// TODO: dynamic tags
			std::cout << "Warning: Ignoring globbed key '" << name << "'; currently unsupported" << std::endl;
			continue;
		}

		if (!key.getMeta<const kdb::Key> ("default"))
		{
			throw CommandAbortException ("The key '" + name + "' doesn't have a default value!");
		}

		auto type = getType (key);

		std::unordered_set<std::string> allowedTypes = { "enum",
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
		if (experimentalStructs)
		{
			allowedTypes.insert ("struct");
			allowedTypes.insert ("struct_ref");
		}

		if (allowedTypes.find (type) == allowedTypes.end ())
		{
			auto msg = "The key '" + name;
			msg += "' has an unsupported type ('" + type + "')!";
			throw CommandAbortException (msg);
		}

		if (type == "struct_ref")
		{
			std::cout << "Warning: Ignoring struct_ref key '" << name << "' outside of struct; currently unsupported"
				  << std::endl;
			continue;
		}

		auto tagName = getTagName (key, specParent.getName (), getParameter (Params::TagPrefix));
		object keyObject = { { "name", name },
				     { "tag_name", snakeCaseToMacroCase (tagName) },
				     { "type_name", snakeCaseToCamelCase (type) } };

		if (type == "enum")
		{
			auto enumData = enumProcessor.process (key, tagName);

			keyObject["type_name"] = enumData["type_name"].string_value ();
			enums.emplace_back (enumData);
		}

		if (experimentalStructs && type == "struct")
		{
			kdb::KeySet subkeys;
			for (auto cur = it + 1; cur != ks.end (); ++cur)
			{
				if (cur->isDirectBelow (key))
				{
					subkeys.append (*cur);
				}
				else
				{
					break;
				}
			}

			auto structData = structProcessor.process (key, subkeys, tagName);

			keyObject["type_name"] = structData["type_name"].string_value ();
			keyObject["is_struct?"] = true;

			structs.emplace_back (structData);
		}

		keys.emplace_back (keyObject);
	}

	data["keys_count"] = std::to_string (keys.size ());
	data["keys"] = keys;
	data["enums"] = enums;
	data["structs"] = structs;
	data["defaults"] = keySetToCCode (spec);

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
