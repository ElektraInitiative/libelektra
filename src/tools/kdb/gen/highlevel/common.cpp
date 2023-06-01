/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./common.hpp"

void escapeNonAlphaNum (std::string & str)
{
	std::replace_if (str.begin (), str.end (), std::not1 (std::ptr_fun (isalnum)), '_');
}

std::vector<std::string> getKeyParts (const kdb::Key & key)
{
	auto rawName = static_cast<const char *> (ckdb::keyUnescapedName (key.getKey ()));
	size_t size = ckdb::keyGetUnescapedNameSize (key.getKey ());
	std::vector<char> name (rawName, rawName + size);
	auto cur = name.begin ();
	std::vector<std::string> parts;
	while (cur != name.end ())
	{
		auto next = std::find (cur, name.end (), '\0');
		parts.emplace_back (cur, next);
		cur = next + 1;
	}
	return parts;
}

bool hasType (const kdb::Key & key)
{
	return key.hasMeta ("type") && !key.getMeta<std::string> ("type").empty ();
}

std::string getType (const kdb::Key & key)
{
	return key.getMeta<std::string> ("type");
}

std::string getTagName (std::string name)
{
	name = std::regex_replace (name, std::regex ("/[#_]/"), "/");
	name = std::regex_replace (name, std::regex ("[#_]/"), "/");
	name = std::regex_replace (name, std::regex ("/[#_]"), "/");
	name = std::regex_replace (name, std::regex (R"(/%(([^/]|(\\\\)*\\/|(\\\\)+)+)%/)"), "/$1/");

	if (name[name.length () - 1] == '/')
	{
		name.erase (name.length () - 1);
	}

	escapeNonAlphaNum (name);

	return name;
}

std::string getTagName (const kdb::Key & key, const std::string & parentKey)
{
	auto name = key.getName ();
	name.erase (0, parentKey.length () + 1);

	return getTagName (name);
}

std::string snakeCaseToCamelCase (const std::string & s, bool upper)
{
	std::string result;
	result.resize (s.size ());
	auto upcase = upper;
	std::transform (s.begin (), s.end (), result.begin (), [&upcase] (char c) {
		int x = upcase ? toupper (c) : c;
		upcase = c == '_';
		return x;
	});
	result.erase (std::remove (result.begin (), result.end (), '_'), result.end ());
	return result;
}

std::string snakeCaseToPascalCase (const std::string & s)
{
	return snakeCaseToCamelCase (s, true);
}

std::string snakeCaseToMacroCase (const std::string & s)
{
	std::string result;
	result.resize (s.size ());
	std::transform (s.begin (), s.end (), result.begin (), ::toupper);
	return result;
}

std::string camelCaseToMacroCase (const std::string & s)
{
	std::stringstream ss;
	std::for_each (s.begin (), s.end (), [&ss] (char c) {
		if (ss.tellp () != std::stringstream::beg && isupper (c))
		{
			ss << '_';
		}
		ss << static_cast<char> (toupper (c));
	});
	return ss.str ();
}

std::string upCaseFirst (const std::string & str)
{
	std::string result = str;
	result[0] = toupper (result[0]);
	return result;
}
