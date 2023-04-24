/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./enums.hpp"
#include "./common.hpp"
#include <command.hpp>
#include <elektra/ease/array.h>
#include <internal/utility/old_helper.h>

kainjow::mustache::list EnumProcessor::getValues (const std::string & prefix, const kdb::Key & key, std::string & fromStringSwitch,
						  std::string & valuesString, size_t & trieDepth)
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
	if (ckdb::elektraArrayValidateBaseNameString (end.c_str ()) < 0)
	{
		throw CommandAbortException ("The key '" + key.getName () + "' has invalid check/enum metadata: " + end);
	}

	kdb::long_long_t i = 0;
	std::string cur = "#0";
	while (cur <= end)
	{
		if (key.hasMeta ("check/enum/" + cur))
		{
			auto name = prefix + "_";
			const std::string & stringValue = key.getMeta<std::string> ("check/enum/" + cur);
			name += camelCaseToMacroCase (stringValue);
			escapeNonAlphaNum (name);
			auto value = std::to_string (i);
			if (key.hasMeta ("gen/enum/" + cur + "/value"))
			{
				value = key.getMeta<std::string> ("gen/enum/" + cur + "/value");
			}
			values.emplace_back (object{ { "name", name }, { "value", value }, { "string_value", stringValue } });
			stringValues.insert ({ stringValue, name });
			ss << name << "=" << value << "\n";
		}
		++i;
		auto indexString = std::to_string (i);
		cur = "#" + std::string (indexString.length () - 1, '_') + std::to_string (i);
	}

	EnumTrie trie (stringValues);
	trieDepth = trie.getDepth ();
	fromStringSwitch = trie.createSwitch ();

	valuesString = ss.str ();

	return values;
}

std::string EnumProcessor::getType (const kdb::Key & key, const std::string & tagName, bool & genType)
{
	genType = key.hasMeta ("gen/enum/type");
	return genType ? key.getMeta<std::string> ("gen/enum/type") : snakeCaseToPascalCase (tagName);
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
	size_t trieDepth;
	auto values = getValues (camelCaseToMacroCase (nativeType), key, fromStringSwitch, valuesString, trieDepth);

	auto isNew = true;
	auto generateTypeDef = shouldGenerateTypeDef (key);

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

		isNew = false;
	}

	enumTypes[typeName] = std::make_pair (name, valuesString);

	auto useTrie = conversion == EnumConversion::Trie || (conversion == EnumConversion::Auto && trieDepth < 3);

	return object{ { "new", isNew },
		       { "name", name },
		       { "tag_name", snakeCaseToMacroCase (tagName) },
		       { "type_name", typeName },
		       { "native_type", nativeType },
		       { "generate_typedef?", generateTypeDef },
		       { "values", values },
		       { "switch_from_string?", useTrie },
		       { "from_string_code", fromStringSwitch } };
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

		ss << "*variable = " << name << ";" << std::endl;
		ss << "return 1;" << std::endl;
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
		ss << "*variable = " << name << ";" << std::endl;
		ss << "return 1;" << std::endl;
		return false;
	}
	return true;
}

size_t EnumTrie::getDepth ()
{
	if (children.empty ())
	{
		return 1;
	}

	std::vector<size_t> childDepths;
	std::transform (children.begin (), children.end (), std::back_inserter (childDepths),
			[] (const std::pair<const char, std::unique_ptr<EnumTrie>> & p) { return p.second->getDepth (); });

	return *std::max_element (childDepths.begin (), childDepths.end ()) + 1;
}
