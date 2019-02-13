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

const char * ElektraGenTemplate::Params::InitFunctionName = "initFn";
const char * ElektraGenTemplate::Params::TagPrefix = "tagPrefix";

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

static kainjow::mustache::list getEnumValues (const std::string & prefix, const kdb::Key & key)
{
	using namespace kainjow::mustache;

	if (!key.hasMeta ("check/enum"))
	{
		return {};
	}

	list values;

	const auto end = key.getMeta<std::string> ("check/enum");
	kdb::long_long_t i = 0;
	auto cur = "#" + std::to_string (i);
	while (cur <= end)
	{
		if (key.hasMeta ("check/enum/" + cur))
		{
			auto name = prefix + "_";
			name += camelCaseToMacroCase (key.getMeta<std::string> ("check/enum/" + cur));
			const auto value = std::to_string (i);
			// TODO: custom values
			values.emplace_back (object{ { "name", name }, { "value", value } });
		}
		++i;
		cur = "#" + std::to_string (i);
	}

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
	if (parentKey[0] != '/')
	{
		throw CommandAbortException ("parentKey has to be cascading");
	}

	using namespace kainjow::mustache;

	auto headerFile = outputName + ".h";
	auto includeGuard = createIncludeGuard (headerFile);
	auto initFunctionName = getParameter (Params::InitFunctionName, "loadConfiguration");

	auto data = object{ { "header_file", headerFile },
			    { "include_guard", includeGuard },
			    { "parent_key", parentKey },
			    { "init_function_name", initFunctionName } };

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
			auto values = getEnumValues (camelCaseToMacroCase (nativeType), key);

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
						    { "values", values } });
		}

		keys.emplace_back (keyObject);
	}

	data["keys_count"] = std::to_string (keys.size ());
	data["keys"] = keys;
	data["enums"] = enums;

	return data;
}