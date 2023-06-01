/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./structs.hpp"
#include "./common.hpp"
#include "./enums.hpp"
#include <command.hpp>
#include <elektra/ease/array.h>
#include <elektra/ease/reference.h>
#include <internal/utility/old_helper.h>

bool StructFieldsProcessor::shouldGenerateUnion (const kdb::Key & key)
{
	return !key.hasMeta ("gen/union/create") || key.getMeta<std::string> ("gen/union/create") == "1";
}

std::string StructFieldsProcessor::discriminatorField (const kdb::Key & key, const std::string & refFieldName)
{
	auto result = key.hasMeta ("gen/reference/discriminator") ? key.getMeta<std::string> ("gen/reference/discriminator") :
								    refFieldName + "Discriminator";
	escapeNonAlphaNum (result);
	return result;
}

std::string StructFieldsProcessor::arraySizeName (const kdb::Key & key, const std::string & arrayFieldName)
{
	auto result = key.hasMeta ("gen/struct/array/sizefield") ? key.getMeta<std::string> ("gen/struct/array/sizefield") :
								   arrayFieldName + "Size";
	escapeNonAlphaNum (result);
	return result;
}

std::string StructFieldsProcessor::getName (const kdb::Key & key, const std::string & fieldKeyName)
{
	auto result = key.hasMeta ("gen/struct/field") ? key.getMeta<std::string> ("gen/struct/field") : fieldKeyName;
	escapeNonAlphaNum (result);
	return result;
}

std::string StructFieldsProcessor::discriminatorKey (const kdb::Key & key)
{
	auto result = key.hasMeta ("gen/reference/discriminator/key") ? key.getMeta<std::string> ("gen/reference/discriminator/key") :
									"discriminator";
	escapeNonAlphaNum (result);
	return result;
}

std::string StructFieldsProcessor::discriminatorEnumType (const kdb::Key & key)
{
	if (!key.hasMeta ("gen/reference/discriminator/enum"))
	{
		throw CommandAbortException ("To use alternative references, the key '" + key.getName () +
					     "' must have gen/reference/enum set.");
	}

	auto result = key.getMeta<std::string> ("gen/reference/discriminator/enum");
	escapeNonAlphaNum (result);
	return result;
}

std::string StructFieldsProcessor::discriminatorUnionType (const kdb::Key & key)
{
	if (!key.hasMeta ("gen/reference/discriminator/union"))
	{
		throw CommandAbortException ("To use alternative references, the key '" + key.getName () +
					     "' must have gen/reference/union set.");
	}

	auto result = key.getMeta<std::string> ("gen/reference/discriminator/union");
	escapeNonAlphaNum (result);
	return result;
}

bool StructFieldsProcessor::processArrayStructRef (const kdb::Key & key, const kdb::Key & parentKey, const kdb::KeySet & allKeys,
						   std::string & typeName, std::string & nativeType, bool & alloc, std::string & restrict)
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

	if (arrayParent.hasMeta ("type"))
	{
		throw CommandAbortException (
			"For array keys with type struct_ref, the array parent (without /# at end) must NOT define 'type'. "
			"Key: '" +
			key.getName () + "'.");
	}

	restrict = arrayParent.getMeta<std::string> ("check/reference/restrict");

	if (ckdb::elektraArrayValidateBaseNameString (restrict.c_str ()) > 0)
	{
		return false;
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
	auto structType = StructProcessor::getType (restrictKey, getTagName (restrictKey, parentKey.getName ()), genType);
	typeName = "Struct" + structType;
	nativeType = genType ? structType : "Elektra" + typeName;
	alloc = StructProcessor::shouldAllocate (restrictKey);

	return true;
}

kainjow::mustache::object StructFieldsProcessor::processArrayStructRef (const kdb::Key & key, const std::string & keyName,
									const std::string & fieldKeyName)
{
	std::string typeName;
	std::string nativeType;
	std::string restrict;
	bool alloc = false;

	if (!processArrayStructRef (key, parentKey, allKeys, typeName, nativeType, alloc, restrict))
	{
		kdb::Key arrayParent = key.dup ();
		arrayParent.delBaseName ();

		arrayParent = allKeys.lookup (arrayParent);
		return processStructRefUnion (arrayParent, key, keyName, true, restrict, fieldKeyName);
	}

	auto name = getName (key, fieldKeyName);
	auto sizeName = arraySizeName (key, fieldKeyName);

	fields.emplace_back (kainjow::mustache::object{
		{ "name", sizeName }, { "is_array_size?", true }, { "array_key", fieldKeyName }, { "native_type", "kdb_long_long_t" } });

	return kainjow::mustache::object{ { "name", name },	     { "key_name", keyName },	{ "native_type", nativeType },
					  { "type_name", typeName }, { "alloc?", alloc },	{ "is_array?", true },
					  { "is_struct?", true },    { "size_field", sizeName } };
}

bool StructFieldsProcessor::processStructRef (const kdb::Key & key, const kdb::Key & parentKey, const kdb::KeySet & allKeys,
					      std::string & typeName, std::string & nativeType, bool & alloc, std::string & restrict)
{
	if (!key.hasMeta ("check/reference/restrict"))
	{
		throw CommandAbortException ("Keys with type struct_ref must also define 'check/reference/restrict'. Key: '" +
					     key.getName () + "'.");
	}

	restrict = key.getMeta<std::string> ("check/reference/restrict");

	if (ckdb::elektraArrayValidateBaseNameString (restrict.c_str ()) > 0)
	{
		return false;
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
	auto structType = StructProcessor::getType (restrictKey, getTagName (restrictKey, parentKey.getName ()), genType);
	typeName = "Struct" + structType;
	nativeType = genType ? structType : "Elektra" + typeName;
	alloc = StructProcessor::shouldAllocate (restrictKey);

	return true;
}

kainjow::mustache::object StructFieldsProcessor::processStructRefUnion (const kdb::Key & checkKey, const kdb::Key & genKey,
									const std::string & keyName, bool isArray, const std::string & end,
									const std::string & fieldKeyName)
{
	auto discriminatorField = StructFieldsProcessor::discriminatorField (genKey, fieldKeyName);
	auto discriminatorKeyName = StructFieldsProcessor::discriminatorKey (genKey);

	std::string enumType = StructFieldsProcessor::discriminatorEnumType (genKey);
	auto enumPrefix = camelCaseToMacroCase (enumType);

	std::string unionType = StructFieldsProcessor::discriminatorUnionType (genKey);
	kainjow::mustache::list unionFields;
	bool generateUnion = shouldGenerateUnion (genKey);

	kdb::long_long_t i = 0;
	std::string cur = "#0";
	while (cur <= end)
	{
		if (checkKey.hasMeta ("check/reference/restrict/" + cur))
		{
			auto restrict = checkKey.getMeta<std::string> ("check/reference/restrict/" + cur);
			char * rawResolved = ckdb::elektraResolveReference (restrict.c_str (), checkKey.getKey (), parentKey.getKey ());

			auto restrictKey = allKeys.lookup (rawResolved);
			ckdb::elektraFree (rawResolved);

			if (!restrictKey)
			{
				throw CommandAbortException ("'check/reference/restrict' of key '" + checkKey.getName () +
							     "' resolves to an unspecified key.");
			}

			if (restrictKey.getMeta<std::string> ("type") != "struct")
			{
				throw CommandAbortException ("'check/reference/restrict' of key '" + checkKey.getName () +
							     "' resolves to a non-struct key.");
			}

			auto discriminatorRef = restrict + "/";
			discriminatorRef += discriminatorKeyName;
			rawResolved = ckdb::elektraResolveReference (discriminatorRef.c_str (), checkKey.getKey (), parentKey.getKey ());

			auto discriminatorKey = allKeys.lookup (rawResolved);
			ckdb::elektraFree (rawResolved);

			if (!discriminatorKey)
			{
				throw CommandAbortException ("'check/reference/restrict' of key '" + checkKey.getName () +
							     "' resolves to an unspecified key.");
			}

			if (discriminatorKey.getMeta<std::string> ("type") != "discriminator" ||
			    discriminatorKey.getMeta<std::string> ("check/type") != "enum")
			{
				throw CommandAbortException ("The discriminator key '" + discriminatorKey.getName () + "' for key '" +
							     genKey.getName () + "' must have type 'discriminator' and check/type 'enum'.");
			}

			if (discriminatorKey.getMeta<std::string> ("gen/enum/type") != enumType)
			{
				throw CommandAbortException ("The discriminator key '" + discriminatorKey.getName () + "' for key '" +
							     genKey.getName () + "' must have gen/enum/type set to '" + enumType + "'.");
			}

			if (!genKey.hasMeta ("gen/reference/restrict/" + cur + "/discriminator"))
			{
				throw CommandAbortException (
					"To use alternative references in key '" + genKey.getName () +
					"', you must set the gen/reference/restrict/#/value for each check/reference/restrict/# on key " +
					checkKey.getName ());
			}

			std::string enumValueString = genKey.getMeta<std::string> ("gen/reference/restrict/" + cur + "/discriminator");

			if (discriminatorKey.getMeta<std::string> ("check/enum/" + cur) != enumValueString)
			{
				std::string msg = "The discriminator key '" + discriminatorKey.getName ();
				msg += "' for key '" + genKey.getName () + "' must have check/enum/";
				msg += cur + " set to '";
				msg += enumValueString + "'.";
				throw CommandAbortException (msg);
			}

			bool genType;
			auto structType = StructProcessor::getType (restrictKey, getTagName (restrictKey, parentKey.getName ()), genType);
			auto typeName = "Struct" + structType;
			auto nativeType = genType ? structType : "Elektra" + typeName;
			auto alloc = StructProcessor::shouldAllocate (restrictKey);

			if (!alloc)
			{
				throw CommandAbortException (
					"All structs referenced by alternative references must be allocating (gen/struct/alloc = 1). "
					"Key: " +
					restrictKey.getName () + " referenced by:" + genKey.getName ());
			}

			std::string name = enumValueString;
			escapeNonAlphaNum (name);

			std::string enumValue = enumPrefix + "_";
			enumValue += camelCaseToMacroCase (enumValueString);
			escapeNonAlphaNum (enumValue);

			unionFields.emplace_back (kainjow::mustache::object{ { "name", name },
									     { "native_type", nativeType },
									     { "enum_value", enumValue },
									     { "type_name", typeName },
									     { "union_type", unionType } });
		}
		++i;
		auto indexString = std::to_string (i);
		cur = "#" + std::string (indexString.length () - 1, '_') + std::to_string (i);
	}

	if (unionFields.empty ())
	{
		throw CommandAbortException ("Key with alternative references must have at least 1 alternative. Key: " + genKey.getName ());
	}

	auto name = getName (genKey, fieldKeyName);

	auto field = kainjow::mustache::object{ { "name", name },
						{ "key_name", keyName },
						{ "native_type", unionType },
						{ "type_name", "Union" + unionType },
						{ "is_array?", isArray },
						{ "is_struct?", false },
						{ "is_union?", true },
						{ "discr_native_type", enumType },
						{ "discr_type_name", "Enum" + enumType },
						{ "discr_suffix", discriminatorKeyName },
						{ "discr_field", discriminatorField } };

	if (isArray)
	{
		auto sizeName = arraySizeName (genKey, fieldKeyName);

		fields.emplace_back (kainjow::mustache::object{ { "name", sizeName },
								{ "is_array_size?", true },
								{ "array_key", fieldKeyName },
								{ "native_type", "kdb_long_long_t" } });

		field["size_field"] = sizeName;
	}

	fields.emplace_back (kainjow::mustache::object{ { "name", discriminatorField },
							{ "is_discriminator?", true },
							{ "is_array?", isArray },
							{ "union_key", fieldKeyName },
							{ "native_type", enumType } });

	unions.emplace_back (kainjow::mustache::object{ { "native_type", unionType },
							{ "type_name", "Union" + unionType },
							{ "fields", unionFields },
							{ "generate_typedef?", generateUnion },
							{ "discr_native_type", enumType },
							{ "default_type", unionFields[0]["native_type"] },
							{ "generate_setters?", false } });

	return field;
}


kainjow::mustache::object StructFieldsProcessor::processStructRef (const kdb::Key & key, const std::string & keyName,
								   const std::string & fieldKeyName)
{
	std::string typeName;
	std::string nativeType;
	std::string restrict;
	bool alloc = false;

	if (!processStructRef (key, parentKey, allKeys, typeName, nativeType, alloc, restrict))
	{
		return processStructRefUnion (key, key, keyName, false, restrict, fieldKeyName);
	}

	auto name = getName (key, fieldKeyName);

	return kainjow::mustache::object{ { "name", name },	     { "key_name", keyName }, { "native_type", nativeType },
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

		std::unordered_set<std::string> allowedTypes = { "struct_ref", "enum",		"string",      "boolean",
								 "char",       "octet",		"short",       "unsigned_short",
								 "long",       "unsigned_long", "long_long",   "unsigned_long_long",
								 "float",      "double",	"long_double", "discriminator" };

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

		if (type == "discriminator")
		{
			continue;
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

		auto name = getName (key, fieldKeyName);

		if (isArray)
		{
			keyName = keyName.substr (0, keyName.length () - 2);
		}

		maxFieldNameLen = std::max (maxFieldNameLen, keyName.size ());

		ss << nativeType << " " << name << "\n";

		if (isStruct)
		{
			containsStructRef = true;

			if (isArray)
			{
				fields.push_back (processArrayStructRef (key, keyName, fieldKeyName));
			}
			else
			{
				fields.push_back (processStructRef (key, keyName, fieldKeyName));
			}
		}
		else
		{

			auto field = object{ { "name", name },		{ "key_name", keyName }, { "native_type", nativeType },
					     { "type_name", typeName }, { "alloc?", true },	 { "is_array?", isArray },
					     { "is_struct?", false } };

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
		}
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
						    const std::string & specParentName, kainjow::mustache::list & unions)
{
	using namespace kainjow::mustache;

	auto name = key.getName ();
	name.erase (0, sizeof ("spec:") - 1);

	bool genType;
	auto structType = getType (key, tagName, genType);
	auto typeName = "Struct" + structType;

	auto nativeType = genType ? structType : "Elektra" + typeName;


	auto allocate = shouldAllocate (key);

	StructFieldsProcessor fieldsProcessor (parentKey, allKeys, key, subkeys, allocate, specParentName);
	auto fields = fieldsProcessor.getFields ();
	std::string fieldsString = fieldsProcessor.getFieldsString ();
	size_t maxFieldNameLen = fieldsProcessor.getMaxFieldNameLen ();
	unions = fieldsProcessor.getUnions ();
	auto containsStructRef = fieldsProcessor.getContainsStructRef ();

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
		       { "alloc?", allocate },
		       { "generate_setters?", !containsStructRef } };
}
