/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_ELEKTRAGEN_STRUCTS_HPP
#define ELEKTRA_KDB_ELEKTRAGEN_STRUCTS_HPP

#include "../template.hpp"

class StructProcessor
{
private:
	std::unordered_map<std::string, std::pair<std::string, std::string>> structTypes;
	const kdb::Key & parentKey;
	const kdb::KeySet & allKeys;

	static inline bool shouldGenerateTypeDef (const kdb::Key & key);

public:
	StructProcessor (const kdb::Key & parentKey_, const kdb::KeySet & allKeys_) : parentKey (parentKey_), allKeys (allKeys_)
	{
	}

	static inline bool shouldAllocate (const kdb::Key & key);
	static inline std::string getType (const kdb::Key & key, const std::string & tagName, bool & genType);

	kainjow::mustache::object process (const kdb::Key & key, const kdb::KeySet & subkeys, const std::string & tagName,
					   const std::string & specParentName);
	static bool isFieldIgnored (const kdb::Key & key);
};

class StructFieldsProcessor
{
private:
	const kdb::Key & parentKey;
	const kdb::KeySet & allKeys;

	kainjow::mustache::list fields;
	size_t maxFieldNameLen = 0;
	std::string fieldsString;

	const kdb::Key & structKey;
	const kdb::KeySet & structKeys;
	bool allocating;
	const std::string & specParentName;

	bool processed = false;

	static inline std::string getName (const kdb::Key & key, const std::string & fieldKeyName);
	static inline std::string arraySizeName (const kdb::Key & key, const std::string & arrayFieldName);

	kainjow::mustache::object processArrayStructRef (const kdb::Key & key, const std::string & tagName, const std::string & keyName,
							 const std::string & fieldKeyName);

	kainjow::mustache::object processStructRef (const kdb::Key & key, const std::string & tagName, const std::string & keyName,
						    const std::string & fieldKeyName);

	void processAll ();

public:
	StructFieldsProcessor (const kdb::Key & parentKey_, const kdb::KeySet & allKeys_, const kdb::Key & structKey_,
			       const kdb::KeySet & structKeys_, bool allocating_, const std::string & specParentName_)
	: parentKey (parentKey_), allKeys (allKeys_), structKey (structKey_), structKeys (structKeys_), allocating (allocating_),
	  specParentName (specParentName_)
	{
	}

	kainjow::mustache::list getFields ()
	{
		if (!processed)
		{
			processAll ();
		}

		return fields;
	}

	size_t getMaxFieldNameLen ()
	{
		if (!processed)
		{
			processAll ();
		}
		return maxFieldNameLen;
	}

	std::string getFieldsString ()
	{
		if (!processed)
		{
			processAll ();
		}
		return fieldsString;
	}

	static void processStructRef (const kdb::Key & key, const std::string & tagName, const kdb::Key & parentKey,
				      const kdb::KeySet & allKeys, std::string & typeName, std::string & nativeType, bool & alloc);
};


#endif // ELEKTRA_KDB_ELEKTRAGEN_STRUCTS_HPP
