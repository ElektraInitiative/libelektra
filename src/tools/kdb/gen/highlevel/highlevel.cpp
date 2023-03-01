/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "highlevel.hpp"

#include "common.hpp"
#include "enums.hpp"
#include "structs.hpp"

#include <command.hpp>
#include <modules.hpp>

#include <coloredkdbio.hpp>
#include <kdb.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbopts.h>
#include <kdbplugin.h>
#include <kdbtypes.h>

#include <fstream>
#include <key.hpp>
#include <memory>
#include <regex>
#include <set>
#include <streambuf>
#include <string>

const char * HighlevelGenTemplate::Params::InitFunctionName = "initFn";
const char * HighlevelGenTemplate::Params::HelpFunctionName = "helpFn";
const char * HighlevelGenTemplate::Params::SpecloadFunctionName = "specloadFn";
const char * HighlevelGenTemplate::Params::RunCommandsFunctionName = "runCmdFn";
const char * HighlevelGenTemplate::Params::TagPrefix = "tagPrefix";
const char * HighlevelGenTemplate::Params::EnumConversion = "enumConv";
const char * HighlevelGenTemplate::Params::AdditionalHeaders = "headers";
const char * HighlevelGenTemplate::Params::GenerateSetters = "genSetters";
const char * HighlevelGenTemplate::Params::EmbeddedSpec = "embeddedSpec";
const char * HighlevelGenTemplate::Params::InstallPrefix = "installPrefix";
const char * HighlevelGenTemplate::Params::EmbedHelpFallback = "embedHelpFallback";
const char * HighlevelGenTemplate::Params::UseCommands = "useCommands";
const char * HighlevelGenTemplate::Params::InitWithPointers = "initWithPointers";

enum class EmbeddedSpec
{
	Full,
	Defaults,
	None
};

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
					"Replaces occurrence no. " + indexStr + " of " + kind + " in the keyname.";
}

static void getKeyArgs (const kdb::Key & key, const size_t parentKeyParts, kainjow::mustache::list & args, std::string & fmtString)
{
	using namespace kainjow::mustache;
	auto parts = getKeyParts (key);
	parts.erase (parts.begin (), parts.begin () + parentKeyParts);

	std::stringstream fmt;

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

static std::string keySetToCCode (kdb::KeySet & set)
{
	using namespace kdb;
	using namespace kdb::tools;

	Modules modules;
	PluginPtr plugin = modules.load ("c", KeySet ());

	auto file = "/tmp/elektra.highlevelgen." + std::to_string (std::time (nullptr));
	Key errorKey ("/", KEY_VALUE, file.c_str (), KEY_END);
	if (plugin->set (set, errorKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		throw CommandAbortException ("c (plugin) failed");
	}

	std::ifstream is (file);
	std::string line;

	std::stringstream ss;
	while (std::getline (is, line))
	{
		ss << line << std::endl;
	}

	return ss.str ();
}

static void keySetToQuickdump (kdb::KeySet & set, const std::string & path, const std::string & parent)
{
	using namespace kdb;
	using namespace kdb::tools;

	Modules modules;
	KeySet config;
	config.append (Key ("system:/noparent", KEY_END));
	PluginPtr plugin = modules.load ("quickdump", config);

	Key parentKey (parent.c_str (), KEY_VALUE, path.c_str (), KEY_END);
	if (plugin->set (set, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
	{
		throw CommandAbortException ("quickdump failed");
	}
}

static kdb::KeySet cascadingToSpec (const kdb::KeySet & ks)
{
	auto result = kdb::KeySet (ks.size (), KS_END);
	for (auto it = ks.begin (); it != ks.end (); ++it)
	{
		if (it->isCascading ())
		{
			auto specKey = kdb::Key (it->dup ());
			specKey.setName ("spec:" + specKey.getName ());
			result.append (specKey);
		}
		if (it->isSpec ())
		{
			result.append (*it);
		}
	}
	return result;
}

static std::string generateHelpMessage (const std::string & appName, const std::string & specParent, kdb::KeySet spec)
{
	std::vector<const char *> argv = { appName.c_str (), "--help" };

	kdb::Key parentKey (specParent.c_str (), KEY_END);
	int ret = ckdb::elektraGetOpts (spec.getKeySet (), argv.size (), argv.data (), NULL, parentKey.getKey ());

	if (ret != 1)
	{
		throw CommandAbortException ("could not generate fallback help message");
	}

	char * help = ckdb::elektraGetOptsHelpMessage (parentKey.getKey (), NULL, NULL);
	std::string result (help);
	ckdb::elektraFree (help);

	return result;
}

kainjow::mustache::data HighlevelGenTemplate::getTemplateData (const std::string & outputName, const std::string & part,
							       const kdb::KeySet & keySet, const std::string & parentKey) const
{
	using namespace kainjow::mustache;

	auto headerFile = outputName + ".h";
	auto commandHeader = outputName + ".commands.h";
	auto includeGuard = createIncludeGuard (headerFile);
	auto commandIncludeGuard = createIncludeGuard (commandHeader);
	auto initFunctionName = getParameter (Params::InitFunctionName, "loadConfiguration");
	auto helpFunctionName = getParameter (Params::HelpFunctionName, "printHelpMessage");
	auto specloadFunctionName = getParameter (Params::SpecloadFunctionName, "exitForSpecload");
	auto runCommandsFunctionName = getParameter (Params::RunCommandsFunctionName, "runCommands");
	auto tagPrefix = getParameter (Params::TagPrefix, "ELEKTRA_TAG_");
	auto installPrefix = getParameter (Params::InstallPrefix, "/usr/local");
	auto additionalHeaders = split (getParameter (Params::AdditionalHeaders), ',');
	auto enumConversionString = getParameter (Params::EnumConversion, "auto");
	auto generateSetters = getBoolParameter (Params::GenerateSetters, true);
	auto embedHelpFallback = getBoolParameter (Params::EmbedHelpFallback, true);
	auto specHandling = getParameter<EmbeddedSpec> (Params::EmbeddedSpec, { { "", EmbeddedSpec::Full },
										{ "full", EmbeddedSpec::Full },
										{ "defaults", EmbeddedSpec::Defaults },
										{ "none", EmbeddedSpec::None } });
	auto enumConversion = getParameter<EnumConversion> (Params::EnumConversion, { { "", EnumConversion::Auto },
										      { "auto", EnumConversion::Auto },
										      { "switch", EnumConversion::Trie },
										      { "strcmp", EnumConversion::Strcmp } });
	auto useCommands = getBoolParameter (Params::UseCommands, false);
	auto initWithPointers = getBoolParameter (Params::InitWithPointers, true);


	std::string cascadingParent;
	std::string specParentName;
	kdb::KeySet ks;

	if (parentKey[0] == '/')
	{
		cascadingParent = parentKey;
		specParentName = "spec:" + parentKey;
		ks = cascadingToSpec (keySet);
	}
	else if (parentKey.substr (0, 6) == "spec:/")
	{
		cascadingParent = parentKey.substr (5);
		specParentName = parentKey;
		ks = keySet;
	}
	else
	{
		throw CommandAbortException ("parentKey has to start with spec:/ or /");
	}

	auto data = object{ { "header_file", headerFile },
			    { "commands_header", commandHeader },
			    { "include_guard", includeGuard },
			    { "commands_include_guard", commandIncludeGuard },
			    { "spec_parent_key", specParentName },
			    { "parent_key", cascadingParent },
			    { "init_function_name", initFunctionName },
			    { "help_function_name", helpFunctionName },
			    { "specload_function_name", specloadFunctionName },
			    { "run_commands_function_name", runCommandsFunctionName },
			    { "generate_setters?", generateSetters },
			    { "use_commands?", useCommands },
			    { "embed_help_fallback?", embedHelpFallback },
			    { "embed_spec?", specHandling == EmbeddedSpec::Full },
			    { "embed_defaults?", specHandling == EmbeddedSpec::Defaults },
			    { "spec_as_defaults?", specHandling == EmbeddedSpec::Full },
			    { "more_headers", list (additionalHeaders.begin (), additionalHeaders.end ()) },
			    { "init_with_pointers?", initWithPointers } };

	list enums;
	list structs;
	list keys;
	list unions;
	list commands;

	auto specParent = kdb::Key (specParentName, KEY_END);

	EnumProcessor enumProcessor (enumConversion);
	StructProcessor structProcessor (specParent, ks);

	auto parentLength = specParentName.length ();

	kdb::KeySet spec;
	kdb::KeySet specWithParent;
	kdb::KeySet defaults;

	specWithParent.append (ks.lookup (specParent));

	kdb::Key parent = ks.lookup (specParent).dup ();
	parent.setName ("/");
	spec.append (parent);

	if (useCommands)
	{
		if (!parent.hasMeta ("command") || parent.getMeta<std::string> ("command") != "")
		{
			throw CommandAbortException (
				"With 'useCommands' enabled, the parent key of the specification must "
				"have the metakey 'command' set to an empty string.");
		}

		if (getType (parent) != "string")
		{
			throw CommandAbortException (
				"With 'useCommands' enabled, the parent key of the specification must have 'type=string'.");
		}

		if (!parent.hasMeta ("gen/command/function"))
		{
			throw CommandAbortException ("The key " + parent.getName () +
						     " must have the 'gen/command/function' metakey, "
						     "because it has the 'command' metakey.");
		}

		object command = { { "name", "" }, // + 2 to remove slash
				   { "function_name", parent.getMeta<std::string> ("gen/command/function") } };

		commands.emplace_back (command);
	}

	auto parentKeyParts = getKeyParts (specParent);

	auto mountpoint = parent.getMeta<std::string> ("mountpoint");

	std::regex appNameRegex ("/sw/[^/]+/[^/]+/#0/current");
	std::string appName;
	std::string appNameWithOrg;
	if (std::regex_match (cascadingParent, appNameRegex))
	{
		appName = parentKeyParts[3];
		escapeNonAlphaNum (appName);
		std::string orgName = parentKeyParts[2];
		escapeNonAlphaNum (orgName);
		appNameWithOrg = orgName + "/" + appName;
	}
	else
	{
		appName = cascadingParent.substr (1);
		escapeNonAlphaNum (appName);
		appNameWithOrg = appName;
	}

	if (part == ".mount.sh")
	{
		return object{ { "parent_key", cascadingParent },
			       { "spec_parent_key", specParentName },
			       { "mount_file", appName + ".overlay.spec.eqd" },
			       { "spec_mount_file", mountpoint },
			       { "direct_file?", specHandling != EmbeddedSpec::Full },
			       { "org_and_app", appNameWithOrg },
			       { "app", appName } };
	}

	for (auto it = ks.begin (); it != ks.end (); ++it)
	{
		kdb::Key key = *it;

		if (!key.isSpec () || !key.isBelow (specParent))
		{
			continue;
		}

		kdb::Key specKey = key.dup ();
		specKey.setName (specKey.getName ().substr (parentLength));
		spec.append (specKey);

		specWithParent.append (key);

		if (!useCommands && !hasType (key))
		{
			continue;
		}

		auto type = getType (key);
		auto name = key.getName ();
		name.erase (0, sizeof ("spec:") - 1);

		if (useCommands && key.hasMeta ("command"))
		{
			if (type != "string")
			{
				throw CommandAbortException ("The key " + name +
							     " must have 'type=string', because it has the 'command' metakey.");
			}

			if (!key.hasMeta ("gen/command/function"))
			{
				throw CommandAbortException ("The key " + name +
							     " must have the 'gen/command/function' metakey, "
							     "because it has the 'command' metakey.");
			}

			auto functionName = key.getMeta<std::string> ("gen/command/function");

			object command = { { "name", name.substr (cascadingParent.size () + 1) }, // + 2 to remove slash
					   { "function_name", functionName } };

			commands.emplace_back (command);
		}

		if (!hasType (key))
		{
			continue;
		}

		std::string fmtString;
		list args;
		getKeyArgs (key, parentKeyParts.size (), args, fmtString);

		if (!key.hasMeta ("default") && !key.hasMeta ("require"))
		{
			throw CommandAbortException ("The key '" + name +
						     "' doesn't have a default value and is not marked with 'require'!");
		}

		kdb::Key defaultsKey (key.getName ().substr (parentLength), KEY_END);
		defaultsKey.setMeta ("default", key.getMeta<std::string> ("default"));
		defaultsKey.setMeta ("type", key.getMeta<std::string> ("type"));
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
			specKey.setMeta ("type", "enum");
		}

		bool isString = type == "string";
		auto nativeType = isString ? "const char *" : "kdb_" + type + "_t";
		auto typeName = snakeCaseToPascalCase (type);

		auto tagName = getTagName (key, specParentName);

		auto isArray = key.getBaseName () == "#";

		object keyObject = { { "name", name.substr (cascadingParent.size () + 1) }, // + 2 to remove slash
				     { "native_type", nativeType },
				     { "macro_name", tagPrefix + snakeCaseToMacroCase (tagName) },
				     { "tag_name", snakeCaseToPascalCase (tagName) },
				     { "type_name", typeName },
				     { "is_string?", isString },
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
				keyObject["generate_setters?"] = false;
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
								 [] (const std::string & s) { return s == "_" || s == "#"; }) ||
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
			keyObject["generate_setters?"] = structData["generate_setters?"].is_true ();

			if (structData["new"].is_true ())
			{
				structs.emplace_back (structData);
			}
		}

		keys.emplace_back (keyObject);
	}

	kdb::KeySet contract;
	contract.append (kdb::Key ("system:/elektra/contract/mountglobal/gopts", KEY_END));

	// make elektraOpen() succeed, if there are missing required keys, but we are in helpMode
	contract.append (kdb::Key ("system:/elektra/contract/highlevel/helpmode/ignore/require", KEY_VALUE, "1", KEY_END));

	// enable check for properly mounted specification
	contract.append (kdb::Key ("system:/elektra/contract/highlevel/check/spec/mounted", KEY_VALUE, "1", KEY_END));

	// calculate specification token
	char token[65];
	kdb::Key parentKeyMaybeWithErrors = kdb::Key (specParentName, KEY_END);
	kdb_boolean_t success = ckdb::calculateSpecificationToken (token, ks.getKeySet (), parentKeyMaybeWithErrors.getKey ());
	if (!success)
	{
		kdb::printWarnings (std::cerr, parentKeyMaybeWithErrors, false, false);
		kdb::printError (std::cerr, parentKeyMaybeWithErrors, false, false);
		throw CommandAbortException ("Error during calculation of specification token.");
	}
	// FIXME [new_backend]: token calculation broken
	// contract.append (kdb::Key ("system:/elektra/contract/highlevel/check/spec/token", KEY_VALUE, token, KEY_END));


	data["keys_count"] = std::to_string (keys.size ());
	data["keys"] = keys;
	data["enums"] = enums;
	data["unions"] = unions;
	data["structs"] = structs;
	data["commands"] = commands;
	data["commands_count"] = std::to_string (commands.size ());
	data["spec"] = keySetToCCode (spec);
	data["defaults"] = keySetToCCode (defaults);
	data["contract"] = keySetToCCode (contract);
	data["specload_arg"] = "--elektra-spec";
	data["help_fallback"] = generateHelpMessage (appName, specParentName, specWithParent);

	if (part == ".spec.eqd")
	{
		keySetToQuickdump (spec, outputName + part, specParentName);
		return kainjow::mustache::data (false);
	}

	return data;
}

std::string HighlevelGenTemplate::escapeFunction (const std::string & str) const
{
	std::stringstream ss;
	for (const auto & c : str)
	{
		switch (c)
		{
		case '\a':
			ss << "\\a";
			break;
		case '\b':
			ss << "\\b";
			break;
		case '\f':
			ss << "\\f";
			break;
		case '\n':
			ss << "\\n";
			break;
		case '\r':
			ss << "\\r";
			break;
		case '\t':
			ss << "\\t";
			break;
		case '\v':
			ss << "\\v";
			break;
		case '\\':
			ss << "\\\\";
			break;
		case '\'':
			ss << "\\'";
			break;
		case '"':
			ss << "\\\"";
			break;
		default:
			if (isprint (c))
			{
				ss << c;
			}
			else
			{
				ss << "\\x" << std::hex << std::setw (2) << static_cast<unsigned char> (c);
			}
		}
	}

	return ss.str ();
}

std::vector<std::string> HighlevelGenTemplate::getActualParts () const
{
	std::vector<std::string> parts (GenTemplate::getActualParts ());
	if (getParameter (Params::EmbeddedSpec, "full") == "full")
	{
		parts.erase (std::remove (parts.begin (), parts.end (), ".spec.eqd"), parts.end ());
	}
	if (!getBoolParameter (Params::UseCommands, false))
	{
		parts.erase (std::remove (parts.begin (), parts.end (), ".commands.h"), parts.end ());
	}
	return parts;
}
