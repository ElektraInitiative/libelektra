/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_GEN_HIGHLEVEL_ENUMS_HPP
#define ELEKTRA_KDB_GEN_HIGHLEVEL_ENUMS_HPP

#include <gen/template.hpp>
#include <map>
#include <set>

enum class EnumConversion
{
	Auto,
	Trie,
	Strcmp
};

class EnumTrie
{
public:
	explicit EnumTrie (const std::set<std::pair<std::string, std::string>> & values);

	EnumTrie () : children (), stringValue (), name ()
	{
	}

	std::string createSwitch ();

	size_t getDepth ();

private:
	std::map<char, std::unique_ptr<EnumTrie>> children;
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
						  std::string & valuesString, size_t & trieDepth);

	static inline bool shouldGenerateTypeDef (const kdb::Key & key);

	EnumConversion conversion;

public:
	explicit EnumProcessor (EnumConversion conversion_) : conversion (conversion_)
	{
	}

	kainjow::mustache::object process (const kdb::Key & key, const std::string & tagName);
	static std::string getType (const kdb::Key & key, const std::string & tagName, bool & genType);
};


#endif // ELEKTRA_KDB_GEN_HIGHLEVEL_ENUMS_HPP
