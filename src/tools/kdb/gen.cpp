/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <gen.hpp>

#include "./gen/template.hpp"
#include <cmdline.hpp>
#include <kdb.hpp>

#include <elektra/plugin/plugin.h>
#include <fstream>
#include <modules.hpp>

using namespace kdb;
using namespace kdb::tools;
using namespace std;

int GenCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () < 3)
	{
		throw invalid_argument ("need at least 3 arguments");
	}

	auto templateName = cl.arguments[0];
	auto parentKeyName = cl.arguments[1];
	auto outputName = cl.arguments[2];

	std::unordered_map<std::string, std::string> parameters;
	if (cl.arguments.size () > 3)
	{
		std::transform (cl.arguments.begin () + 3, cl.arguments.end (), std::inserter (parameters, parameters.end ()),
				[] (const std::string & param) {
					auto search = param.find ('=');
					if (search == std::string::npos)
					{
						throw invalid_argument ("parameters have to be of format name=value");
					}

					return std::make_pair (std::string (param.begin (), param.begin () + search),
							       std::string (param.begin () + search + 1, param.end ()));
				});
	}

	const auto & templates = GenTemplateList::getInstance ();
	const auto tmpl = templates.getTemplate (templateName, parameters);

	if (tmpl->isEmpty ())
	{
		// empty template -> correct one not found
		throw invalid_argument ("couldn't find template '" + templateName + "'");
	}

	if (parentKeyName == "-")
	{
		for (const auto & part : tmpl->getParts ())
		{
			std::cout << outputName + part << std::endl;
		}
		return 0;
	}

	/**
	 * The parentKeyForTokenCalculation will be used to find the spec keys within the ks.
	 * Because the token does not take into account metadata from other namespaces, it is only concerned with the spec: namespace.
	 * Changes to the specification that was done in other namespaces is not the goal of the token mechanism.
	 *
	 * To really only take into account spec keys, the namespace of parentKeyForTokenCalculation is important!
	 * There are 2 cases:
	 *
	 * 1. Keys loaded via plugin from file: Here, all keys within the ks will be in the cascading namespace.
	 * Also, the ks will only contain keys from that file.
	 * Thus, the parentKeyForTokenCalculation must be in cascading namespace as well.
	 *
	 * 2. Keys loaded from KDB: Here, the ks will contain keys from all namespaces (user:, spec: , ...)
	 * For token calculation, only keys from the spec namespace are relevant.
	 * Thus, the parentKeyForTokenCalculation must be in spec namespace.
	 */
	Key parentKeyForTokenCalculation (parentKeyName, KEY_END);

	KeySet ks;

	// When no inputFile was specified, load specification keys from the KDB.
	if (cl.inputFile.empty ())
	{
		Key getKey (parentKeyName, KEY_END);

		KDB kdb;
		kdb.get (ks, getKey);

		printWarnings (cerr, getKey, cl.verbose, cl.debug);
		printError (cerr, getKey, cl.verbose, cl.debug);

		if (getKey.hasMeta ("error"))
		{
			throw CommandAbortException ("Error loading from KDB");
		}

		// Set namespace to spec (see comments above for details)
		parentKeyForTokenCalculation.setNamespace (kdb::ElektraNamespace::SPEC);
	}
	// Otherwise, don't use any keys from KDB. Instead load the specification from the specified file via the specified plugin.
	else
	{
		auto pos = cl.inputFile.find ('=');
		if (pos == std::string::npos)
		{
			throw invalid_argument ("-F/--input-file argument must be given as <plugin>=<file>");
		}

		std::string pluginName (cl.inputFile.begin (), cl.inputFile.begin () + pos);
		std::string file (cl.inputFile.begin () + pos + 1, cl.inputFile.end ());

		Modules modules;
		PluginPtr plugin = modules.load (pluginName, cl.getPluginsConfig ());

		if (plugin == nullptr)
		{
			throw invalid_argument ("plugin '" + pluginName + "' given to -F/--input-file could not be loaded");
		}

		Key getKey (parentKeyName, KEY_VALUE, file.c_str (), KEY_END);
		if (plugin->get (ks, getKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			printWarnings (cerr, getKey, cl.verbose, cl.debug);
			printError (cerr, getKey, cl.verbose, cl.debug);
			throw invalid_argument ("file '" + file + "' given to -F/--input-file could not be loaded with plugin '" +
						pluginName + "'");
		}
	}

	auto inputKs = ks.cut (Key (parentKeyForTokenCalculation.getName (), KEY_END));

	for (const auto & part : tmpl->getParts ())
	{
		std::ofstream output (outputName + part);
		tmpl->render (output, outputName, part, inputKs, parentKeyForTokenCalculation.getName ());
	}

	return 0;
}
