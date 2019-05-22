/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <gen.hpp>

#include "gen/template.hpp"
#include <cmdline.hpp>
#include <kdb.hpp>

#include <fstream>
#include <kdbplugin.h>
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
	auto parentKey = cl.arguments[1];
	auto outputName = cl.arguments[2];

	std::unordered_map<std::string, std::string> parameters;
	if (cl.arguments.size () > 3)
	{
		std::transform (cl.arguments.begin () + 3, cl.arguments.end (), std::inserter (parameters, parameters.end ()),
				[](const std::string & param) {
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

	KeySet ks;

	if (cl.inputFile.empty ())
	{
		Key getKey (parentKey, KEY_END);

		KDB kdb;
		kdb.get (ks, getKey);

		printWarnings (cerr, getKey);
		printError (cerr, getKey);

		if (getKey.hasMeta ("error"))
		{
			throw CommandAbortException ("Error loading from KDB");
		}
	}
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

		Key getKey (parentKey, KEY_VALUE, file.c_str (), KEY_END);
		if (plugin->get (ks, getKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			printWarnings (cerr, getKey);
			printError (cerr, getKey);
			throw invalid_argument ("file '" + file + "' given to -F/--input-file could not be loaded with plugin '" +
						pluginName + "'");
		}
	}

	for (const auto & part : tmpl->getParts ())
	{
		std::ofstream output (outputName + part);
		tmpl->render (output, outputName, part, ks, parentKey);
	}

	return 0;
}
