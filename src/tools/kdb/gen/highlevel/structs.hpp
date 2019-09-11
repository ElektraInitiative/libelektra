/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_HIGHLEVEL_STRUCTS_HPP
#define ELEKTRA_KDB_HIGHLEVEL_STRUCTS_HPP

#include <gen/template.hpp>

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
					   const std::string & specParentName, kainjow::mustache::list & unions);
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

	kainjow::mustache::list unions;

	bool processed = false;
	bool containsStructRef = false;

	static inline std::string getName (const kdb::Key & key, const std::string & fieldKeyName);
	static inline std::string arraySizeName (const kdb::Key & key, const std::string & arrayFieldName);
	static inline std::string discriminatorField (const kdb::Key & key, const std::string & refFieldName);
	static inline bool shouldGenerateUnion (const kdb::Key & key);

	kainjow::mustache::object processArrayStructRef (const kdb::Key & key, const std::string & keyName,
							 const std::string & fieldKeyName);

	kainjow::mustache::object processStructRef (const kdb::Key & key, const std::string & keyName, const std::string & fieldKeyName);

	kainjow::mustache::object processStructRefUnion (const kdb::Key & checkKey, const kdb::Key & genKey, const std::string & keyName,
							 bool isArray, const std::string & end, const std::string & fieldKeyName);

	static std::string discriminatorKey (const kdb::Key & key);
	static std::string discriminatorUnionType (const kdb::Key & key);
	static std::string discriminatorEnumType (const kdb::Key & key);


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

	kainjow::mustache::list getUnions ()
	{
		if (!processed)
		{
			processAll ();
		}

		return unions;
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

	bool getContainsStructRef ()
	{
		if (!processed)
		{
			processAll ();
		}
		return containsStructRef;
	}

	static bool processStructRef (const kdb::Key & key, const kdb::Key & parentKey, const kdb::KeySet & allKeys, std::string & typeName,
				      std::string & nativeType, bool & alloc, std::string & restrict);
	static bool processArrayStructRef (const kdb::Key & arrayParent, const kdb::Key & parentKey, const kdb::KeySet & allKeys,
					   std::string & typeName, std::string & nativeType, bool & alloc, std::string & restrict);
};


#endif // ELEKTRA_KDB_HIGHLEVEL_STRUCTS_HPP
