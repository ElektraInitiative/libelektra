/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "structs.hpp"
#include "common.hpp"
#include "enums.hpp"
#include <command.hpp>
#include <kdbease.h>
#include <kdbhelper.h>

std::string StructFieldsProcessor::arraySizeName (const kdb::Key & key, const std::string & arrayFieldName)
{
	return key.hasMeta ("gen/struct/array/sizefield") ? key.getMeta<std::string> ("gen/struct/array/sizefield") :
							    arrayFieldName + "Size";
}

std::string StructFieldsProcessor::getName (const kdb::Key & key, const std::string & fieldKeyName)
{
	std::string result = key.hasMeta ("gen/struct/field") ? key.getMeta<std::string> ("gen/struct/field") : fieldKeyName;
	escapeNonAlphaNum (result);
	return result;
}

kainjow::mustache::object StructFieldsProcessor::processArrayStructRef (const kdb::Key & key, const std::string & tagName,
									const std::string & keyName, const std::string & fieldKeyName)
{
	kdb::Key arrayParent = key.dup ();
	arrayParent.delBaseName ();

	arrayParent = allKeys.lookup (arrayParent);

	if (!arrayParent.hasMeta ("check/reference/restrict"))
	{
		throw CommandAbortException (
			"For array keys with type struct_ref, the array parent (without /# at end) must define 'check/reference/restrict'. "
			"Key: '" +
			key.getName () + "'.");
	}

	auto restrict = arrayParent.getMeta<std::string> ("check/reference/restrict");

	if (ckdb::elektraArrayValidateBaseNameString (restrict.c_str ()))
	{
		throw CommandAbortException ("Alternative references currently not supported. Array Parent: " + arrayParent.getName ());
	}


	char * rawResolved = ckdb::elektraResolveReference (restrict.c_str (), arrayParent.getKey (), parentKey.getKey ());

	auto restrictKey = allKeys.lookup (rawResolved);
	ckdb::elektraFree (rawResolved);

	if (!restrictKey)
	{
		throw CommandAbortException ("'check/reference/restrict' of array parent '" + arrayParent.getName () +
					     "' resolves to an unspecified key.");
	}

	if (restrictKey.getMeta<std::string> ("type") != "struct")
	{
		throw CommandAbortException ("'check/reference/restrict' of array parent '" + arrayParent.getName () +
					     "' resolves to a non-struct key.");
	}

	bool genType;
	auto structType = StructProcessor::getType (restrictKey, tagName, genType);
	auto typeName = "Struct" + structType;
	auto nativeType = genType ? structType : "Elektra" + typeName;
	auto alloc = StructProcessor::shouldAllocate (restrictKey);
	auto name = getName (key, fieldKeyName);

	auto sizeName = arraySizeName (key, fieldKeyName);

	fields.emplace_back (kainjow::mustache::object{
		{ "name", sizeName }, { "is_array_size?", true }, { "array_key", fieldKeyName }, { "native_type", "kdb_long_long_t" } });

	return kainjow::mustache::object{ { "name", name },	  { "key_name", keyName },   { "native_type", nativeType },
					  { "type_name", typeName }, { "alloc?", alloc },       { "is_array?", false },
					  { "is_struct?", true },    { "size_field", sizeName } };
}

void StructFieldsProcessor::processStructRef (const kdb::Key & key, const std::string & tagName, const kdb::Key & parentKey,
					      const kdb::KeySet & allKeys, std::string & typeName, std::string & nativeType, bool & alloc)
{
	if (!key.hasMeta ("check/reference/restrict"))
	{
		throw CommandAbortException ("Keys with type struct_ref must also define 'check/reference/restrict'. Key: '" +
					     key.getName () + "'.");
	}

	auto restrict = key.getMeta<std::string> ("check/reference/restrict");

	if (ckdb::elektraArrayValidateBaseNameString (restrict.c_str ()))
	{
		throw CommandAbortException ("Alternative references currently not supported. Key: " + key.getName ());
	}


	char * rawResolved = ckdb::elektraResolveReference (restrict.c_str (), key.getKey (), parentKey.getKey ());

	auto restrictKey = allKeys.lookup (rawResolved);
	ckdb::elektraFree (rawResolved);

	if (!restrictKey)
	{
		throw CommandAbortException ("'check/reference/restrict' of key '" + key.getName () + "' resolves to an unspecified key.");
	}

	if (restrictKey.getMeta<std::string> ("type") != "struct")
	{
		throw CommandAbortException ("'check/reference/restrict' of key '" + key.getName () + "' resolves to a non-struct key.");
	}

	bool genType;
	auto structType = StructProcessor::getType (restrictKey, tagName, genType);
	typeName = "Struct" + structType;
	nativeType = genType ? structType : "Elektra" + typeName;
	alloc = StructProcessor::shouldAllocate (restrictKey);
}


kainjow::mustache::object StructFieldsProcessor::processStructRef (const kdb::Key & key, const std::string & tagName,
								   const std::string & keyName, const std::string & fieldKeyName)
{
	std::string typeName;
	std::string nativeType;
	bool alloc = false;

	processStructRef (key, tagName, parentKey, allKeys, typeName, nativeType, alloc);

	auto name = getName (key, fieldKeyName);

	return kainjow::mustache::object{ { "name", name },	  { "key_name", keyName }, { "native_type", nativeType },
					  { "type_name", typeName }, { "alloc?", alloc },     { "is_array?", false },
					  { "is_struct?", true } };
}

void StructFieldsProcessor::processAll ()
{
	using namespace kainjow::mustache;

	std::stringstream ss;

	size_t baseParts = getKeyParts (structKey).size ();

	maxFieldNameLen = 0;
	for (const kdb::Key & key : structKeys)
	{
		if (!hasType (key))
		{
			continue;
		}

		auto parts = getKeyParts (key);
		auto isArray = parts.back () == "#";

		auto end = isArray ? parts.end () - 1 : parts.end ();
		std::string fieldKeyName = parts[baseParts];
		for (auto it = parts.begin () + baseParts + 1; it != end; ++it)
		{
			fieldKeyName += "_" + *it;
		}
		fieldKeyName = snakeCaseToCamelCase (fieldKeyName);

		const std::string & type = ::getType (key);

		std::unordered_set<std::string> allowedTypes = { "struct_ref", "enum",		"string",     "boolean",
								 "char",       "octet",		"short",      "unsigned_short",
								 "long",       "unsigned_long", "long_long",  "unsigned_long_long",
								 "float",      "double",	"long_double" };

		if (type == "struct")
		{
			auto msg = "The key '" + key.getName ();
			msg += "' has an unsupported type ('" + type + "')! Cannot have structs inside structs, please use struct_ref.";
			throw CommandAbortException (msg);
		}

		if (allowedTypes.find (type) == allowedTypes.end ())
		{
			auto msg = "The key '" + key.getName ();
			msg += "' has an unsupported type ('" + type + "')!";
			throw CommandAbortException (msg);
		}

		auto isStruct = type == "struct_ref";

		if (!allocating && isStruct)
		{
			auto msg = "Cannot have struct_refs inside non-allocating structs. The key '" + key.getName ();
			msg += "' is a struct_ref appearing inside '" + structKey.getName () + ", which is a non-allocating struct.";
			throw CommandAbortException (msg);
		}

		if (!allocating && isArray)
		{
			auto msg = "Cannot have arrays inside non-allocating structs. The key '" + key.getName ();
			msg += "' is an array appearing inside '" + structKey.getName () + ", which is a non-allocating struct.";
			throw CommandAbortException (msg);
		}

		auto typeName = snakeCaseToPascalCase (type);
		auto nativeType = type == "string" ? "const char *" : "kdb_" + type + "_t";

		auto tagName = getTagName (key, specParentName);

		if (type == "enum")
		{
			bool genType;
			auto enumType = EnumProcessor::getType (key, tagName, genType);

			typeName = "Enum" + enumType;
			nativeType = genType ? enumType : "Elektra" + typeName;
		}

		auto keyName = key.getName ().substr (structKey.getName ().length () + 1);

		if (isStruct)
		{
			if (isArray)
			{
				fields.push_back (
					processArrayStructRef (key, tagName, keyName.substr (0, keyName.length () - 2), fieldKeyName));
			}
			else
			{
				fields.push_back (processStructRef (key, tagName, keyName, fieldKeyName));
			}
			continue;
		}

		auto name = getName (key, fieldKeyName);

		maxFieldNameLen = std::max (maxFieldNameLen, keyName.size ());

		if (isArray)
		{
			keyName = keyName.substr (0, keyName.length () - 2);
		}

		auto field = object{ { "name", name },   { "key_name", keyName },  { "native_type", nativeType }, { "type_name", typeName },
				     { "alloc?", true }, { "is_array?", isArray }, { "is_struct?", false } };

		if (isArray)
		{
			auto sizeName = arraySizeName (key, fieldKeyName);

			fields.emplace_back (object{ { "name", sizeName },
						     { "is_array_size?", true },
						     { "array_key", fieldKeyName },
						     { "native_type", "kdb_long_long_t" } });

			field["size_field"] = sizeName;
		}

		fields.emplace_back (field);

		ss << nativeType << " " << name << "\n";
	}

	fieldsString = ss.str ();
	processed = true;
}

std::string StructProcessor::getType (const kdb::Key & key, const std::string & tagName, bool & genType)
{
	genType = key.hasMeta ("gen/struct/type");
	return genType ? key.getMeta<std::string> ("gen/struct/type") : snakeCaseToPascalCase (tagName);
}

bool StructProcessor::shouldGenerateTypeDef (const kdb::Key & key)
{
	return !key.hasMeta ("gen/struct/create") || key.getMeta<std::string> ("gen/struct/create") == "1";
}

bool StructProcessor::shouldAllocate (const kdb::Key & key)
{
	return key.hasMeta ("gen/struct/alloc") && key.getMeta<std::string> ("gen/struct/alloc") == "1";
}

bool StructProcessor::isFieldIgnored (const kdb::Key & key)
{
	return key.hasMeta ("gen/struct/field/ignore") && key.getMeta<std::string> ("gen/struct/field/ignore") == "1";
}

kainjow::mustache::object StructProcessor::process (const kdb::Key & key, const kdb::KeySet & subkeys, const std::string & tagName,
						    const std::string & specParentName)
{
	using namespace kainjow::mustache;

	auto name = key.getName ();
	name.erase (0, sizeof ("spec") - 1);

	bool genType;
	auto structType = getType (key, tagName, genType);
	auto typeName = "Struct" + structType;

	auto nativeType = genType ? structType : "Elektra" + typeName;


	auto allocate = shouldAllocate (key);

	StructFieldsProcessor fieldsProcessor (parentKey, allKeys, key, subkeys, allocate, specParentName);
	auto fields = fieldsProcessor.getFields ();
	std::string fieldsString = fieldsProcessor.getFieldsString ();
	size_t maxFieldNameLen = fieldsProcessor.getMaxFieldNameLen ();

	auto isNew = true;
	auto generateTypeDef = shouldGenerateTypeDef (key);

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

		isNew = false;
	}

	structTypes[typeName] = std::make_pair (name, fieldsString);

	return object{ { "new", isNew },
		       { "type_name", typeName },
		       { "native_type", nativeType },
		       { "generate_typedef?", generateTypeDef },
		       { "fields", fields },
		       { "max_field_len", std::to_string (maxFieldNameLen + 1) },
		       { "alloc?", allocate } };
}