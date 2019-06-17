/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "elektragen.hpp"

#include "common.hpp"
#include "enums.hpp"
#include "structs.hpp"

#include <command.hpp>
#include <kdbhelper.h>
#include <kdbtypes.h>
#include <modules.hpp>

#include <fstream>
#include <kdb.h>
#include <kdbease.h>
#include <memory>
#include <regex>
#include <set>
#include <streambuf>
#include <string>

const char * ElektraGenTemplate::Params::InitFunctionName = "initFn";
const char * ElektraGenTemplate::Params::HelpFunctionName = "helpFn";
const char * ElektraGenTemplate::Params::SpecloadFunctionName = "specloadFn";
const char * ElektraGenTemplate::Params::EnumConversion = "enumConv";
const char * ElektraGenTemplate::Params::AdditionalHeaders = "headers";

static std::string createIncludeGuard (const std::string & fileName)
{
	std::string result;
	result.resize (fileName.length ());
	std::transform (fileName.begin (), fileName.end (), result.begin (), ::toupper);
	escapeNonAlphaNum (result);
	return result;
}

static inline std::string getArgName (const kdb::Key & key, kdb_long_long_t index, const std::string & defaultPrefix)
{
	auto indexStr = std::to_string (index);
	auto metaName = "gen/arg/name/#" + std::string (indexStr.length () - 1, '_') + indexStr;
	return key.hasMeta (metaName) ? key.getMeta<std::string> (metaName) : defaultPrefix + indexStr;
}

static inline std::string getArgDescription (const kdb::Key & key, kdb_long_long_t index, const std::string & kind)
{
	auto indexStr = std::to_string (index);
	auto metaName = "gen/arg/description/#" + std::string (indexStr.length () - 1, '_') + indexStr;
	return key.hasMeta (metaName) ? key.getMeta<std::string> (metaName) :
					"Replaces occurence no. " + indexStr + " of " + kind + " in the keyname.";
}

static void getKeyArgs (const kdb::Key & key, const size_t parentKeyParts, kainjow::mustache::list & args, std::string & fmtString)
{
	using namespace kainjow::mustache;
	auto parts = getKeyParts (key);
	parts.erase (parts.begin (), parts.begin () + parentKeyParts);

	std::stringstream fmt;

	size_t pos = 1;
	size_t names = 1;
	size_t indices = 1;
	for (const auto & part : parts)
	{
		if (part == "_")
		{
			const std::string & argName = getArgName (key, names, "name");
			auto arg = object{ { "native_type", "const char *" },
					   { "name", argName },
					   { "code", argName },
					   { "index?", false },
					   { "description", getArgDescription (key, names, "_") } };
			args.push_back (arg);
			fmt << "%s/";
			++pos;
			++names;
		}
		else if (part == "#")
		{
			const std::string & argName = getArgName (key, indices, "index");

			std::string argCode = "elektra_len (" + argName + "), elektra_len (";
			argCode += argName + "), \"#___________________\", (long long) ";
			argCode += argName;

			auto arg = object{ { "native_type", "kdb_long_long_t" },
					   { "name", argName },
					   { "code", argCode },
					   { "index?", true },
					   { "description", getArgDescription (key, indices, "#") } };
			args.push_back (arg);
			fmt << "%*.*s%lld/";
			pos += 4;
			++indices;
		}
		else
		{
			// escape backslashes first too avoid collision
			fmt << std::regex_replace (part, std::regex ("[\\\\/]"), "\\\\$0") << "/";
		}
	}

	if (!args.empty ())
	{
		args.back ()["last?"] = true;
	}

	if (args.size () > 1)
	{
		args[args.size () - 2]["last_but_one?"] = true;
	}

	fmtString = fmt.str ();
	fmtString.pop_back ();
}

static std::string keySetToCCode (kdb::KeySet set)
{
	using namespace kdb;
	using namespace kdb::tools;

	Modules modules;
	PluginPtr plugin = modules.load ("c", KeySet ());

	auto file = "/tmp/elektra.elektragen." + std::to_string (std::time (nullptr));
	Key errorKey ("", KEY_VALUE, file.c_str (), KEY_END);
	plugin->set (set, errorKey);

	std::ifstream is (file);
	std::string line;

	std::stringstream ss;
	while (std::getline (is, line))
	{
		ss << line << std::endl;
	}

	return ss.str ();
}

kainjow::mustache::data ElektraGenTemplate::getTemplateData (const std::string & outputName, const kdb::KeySet & ks,
							     const std::string & parentKey) const
{
	if (parentKey.substr (0, 5) != "spec/")
	{
		throw CommandAbortException ("parentKey has to be in spec namespace");
	}

	using namespace kainjow::mustache;


	auto headerFile = outputName + ".h";
	auto includeGuard = createIncludeGuard (headerFile);
	auto initFunctionName = getParameter (Params::InitFunctionName, "loadConfiguration");
	auto helpFunctionName = getParameter (Params::HelpFunctionName, "printHelpMessage");
	auto specloadFunctionName = getParameter (Params::SpecloadFunctionName, "specloadCheck");
	auto additionalHeaders = split (getParameter (Params::AdditionalHeaders), ',');
	auto enumConversionString = getParameter (Params::EnumConversion, "default");

	auto enumConversion = EnumConversion::Default;
	if (enumConversionString == "trie")
	{
		enumConversion = EnumConversion::Trie;
	}
	else if (enumConversionString == "strcmp")
	{
		enumConversion = EnumConversion::Strcmp;
	}
	else if (enumConversionString != "default")
	{
		throw CommandAbortException ("enumConv must be one of: default, trie, strcmp");
	}


	auto cascadingParent = parentKey.substr (4);

	auto data = object{ { "header_file", headerFile },
			    { "include_guard", includeGuard },
			    { "spec_parent_key", parentKey },
			    { "parent_key", cascadingParent },
			    { "init_function_name", initFunctionName },
			    { "help_function_name", helpFunctionName },
			    { "specload_function_name", specloadFunctionName },
			    { "more_headers", list (additionalHeaders.begin (), additionalHeaders.end ()) } };

	list enums;
	list structs;
	list keys;
	list unions;

	auto specParent = kdb::Key (parentKey, KEY_END);

	EnumProcessor enumProcessor (enumConversion);
	StructProcessor structProcessor (specParent, ks);

	const std::string & specParentName = specParent.getName ();
	auto parentLength = specParentName.length ();

	kdb::KeySet spec;
	kdb::KeySet defaults;

	kdb::Key parent = ks.lookup (specParent).dup ();
	spec.append (parent);

	parent = ks.lookup (specParent).dup ();
	parent.setName ("");
	defaults.append (parent);

	auto parentKeyParts = getKeyParts (specParent);

	for (auto it = ks.begin (); it != ks.end (); ++it)
	{
		kdb::Key key = *it;

		if (!key.isSpec () || !key.isBelow (specParent))
		{
			continue;
		}

		kdb::Key specKey = key.dup ();
		spec.append (specKey);

		if (!hasType (key))
		{
			continue;
		}

		auto type = getType (key);
		auto name = key.getName ();
		name.erase (0, sizeof ("spec") - 1);

		std::string fmtString;
		list args;
		getKeyArgs (key, parentKeyParts.size (), args, fmtString);

		if (!key.hasMeta ("default"))
		{
			throw CommandAbortException ("The key '" + name + "' doesn't have a default value!");
		}

		kdb::Key defaultsKey = key.dup ();
		defaultsKey.setName (defaultsKey.getName ().substr (parentLength));
		defaultsKey.setString (key.getMeta<std::string> ("default"));
		defaults.append (defaultsKey);

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
								 "long_double",
								 "struct",
								 "struct_ref",
								 "discriminator" };

		if (allowedTypes.find (type) == allowedTypes.end ())
		{
			auto msg = "The key '" + name;
			msg += "' has an unsupported type ('" + type + "')!";
			throw CommandAbortException (msg);
		}

		if (type == "discriminator")
		{
			type = "enum";
			defaultsKey.setMeta ("type", "enum");
			specKey.setMeta ("type", "enum");
		}

		auto nativeType = type == "string" ? "const char *" : "kdb_" + type + "_t";
		auto typeName = snakeCaseToPascalCase (type);

		auto tagName = getTagName (key, specParentName);

		auto isArray = key.getBaseName () == "#";

		object keyObject = { { "name", name.substr (cascadingParent.size () + 1) }, // + 2 to remove slash
				     { "native_type", nativeType },
				     { "macro_name", snakeCaseToMacroCase (tagName) },
				     { "tag_name", snakeCaseToPascalCase (tagName) },
				     { "type_name", typeName },
				     { "is_array?", isArray } };

		if (!args.empty ())
		{
			keyObject["args?"] = object{ { "args", args } };
			keyObject["args"] = args;
			keyObject["fmt_string"] = fmtString;
		}

		if (isArray)
		{
			if (args.size () > 1)
			{
				// remove last argument and last part of format string
				auto arrayArgs = list{ args.begin (), args.end () - 1 };
				arrayArgs.back ()["last?"] = true;

				keyObject["array_args?"] =
					object ({ { "args", arrayArgs }, { "fmt_string", fmtString.substr (0, fmtString.rfind ('/')) } });
			}
			// remove last part ('/#') from name
			keyObject["array_name"] = name.substr (cascadingParent.size () + 1, name.size () - cascadingParent.size () - 3);
		}

		if (type == "enum")
		{
			auto enumData = enumProcessor.process (key, tagName);

			keyObject["type_name"] = enumData["type_name"].string_value ();
			keyObject["native_type"] = enumData["native_type"].string_value ();

			if (enumData["new"].is_true ())
			{
				enums.emplace_back (enumData);
			}
		}
		else if (type == "struct_ref")
		{
			bool allocate;
			std::string dummyString;

			bool processed;
			if (isArray)
			{
				processed = StructFieldsProcessor::processArrayStructRef (key, specParent, ks, typeName, nativeType,
											  allocate, dummyString);
			}
			else
			{
				processed = StructFieldsProcessor::processStructRef (key, specParent, ks, typeName, nativeType, allocate,
										     dummyString);
			}

			if (processed)
			{
				keyObject["type_name"] = typeName;
				keyObject["native_type"] = nativeType;
				keyObject["is_struct_ref?"] = true;
				keyObject["alloc?"] = allocate;
			}
			else
			{
				continue;
			}
		}
		else if (type == "struct")
		{
			auto maxDepth = key.hasMeta ("gen/struct/depth") ? key.getMeta<kdb::short_t> ("gen/struct/depth") : 1;
			auto baseDepth = getKeyParts (key).size ();

			kdb::KeySet subkeys;
			for (auto cur = it + 1; cur != ks.end (); ++cur)
			{
				if (cur->isBelow (key))
				{
					if (StructProcessor::isFieldIgnored (*cur))
					{
						continue;
					}

					auto parts = getKeyParts (*cur);
					if (parts.size () <= baseDepth + maxDepth)
					{
						if (std::any_of (parts.begin () + baseDepth, parts.end () - 1,
								 [](const std::string & s) { return s == "_" || s == "#"; }) ||
						    parts.back () == "_")
						{
							throw CommandAbortException ("struct cannot contain globbed keys (_, #).");
						}

						subkeys.append (*cur);
					}
					else if (parts.size () <= baseDepth + maxDepth + 1 && parts.back () == "#")
					{
						subkeys.append (*cur);
					}
				}
				else
				{
					break;
				}
			}

			kainjow::mustache::list structUnions;
			auto structData = structProcessor.process (key, subkeys, tagName, specParentName, structUnions);

			for (const auto & u : structUnions)
			{
				unions.push_back (u);
			}

			keyObject["type_name"] = structData["type_name"].string_value ();
			keyObject["native_type"] = structData["native_type"].string_value ();
			keyObject["is_struct?"] = true;
			keyObject["alloc?"] = structData["alloc?"].is_true ();

			if (structData["new"].is_true ())
			{
				structs.emplace_back (structData);
			}
		}

		keys.emplace_back (keyObject);
	}

	// TODO: make configurable?
	auto specloadArg = "--elektra-spec";

	kdb::KeySet contract;
	contract.append (kdb::Key ("system/elektra/ensure/plugins/global/gopts", KEY_VALUE, "mounted", KEY_END));

	data["keys_count"] = std::to_string (keys.size ());
	data["keys"] = keys;
	data["enums"] = enums;
	data["unions"] = unions;
	data["structs"] = structs;
	data["defaults"] = keySetToCCode (defaults);
	data["spec"] = keySetToCCode (spec);
	data["contract"] = keySetToCCode (contract);
	data["specload_arg"] = specloadArg;

	return data;
}
