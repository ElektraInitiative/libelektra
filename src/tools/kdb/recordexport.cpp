/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "recordexport.hpp"
#include "coloredkdbio.hpp"
#include <cmdline.hpp>
#include <iostream>
#include <kdb.hpp>
#include <kdbrecord.h>
#include <modules.hpp>
#include <plugindatabase.hpp>
#include <toolexcept.hpp>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

RecordExportCommand::RecordExportCommand () = default;
RecordExportCommand::~RecordExportCommand () = default;

int RecordExportCommand::execute (const Cmdline & cmdline)
{
	KDB kdb;
	Key errorKey;

#ifdef _WIN32
	string file = "CON";
#else
	string file = "/dev/stdout";
#endif
	std::string format = "ansible";
	Key parentKey;

	size_t argc = cmdline.arguments.size ();
	if (argc > 2)
	{
		throw invalid_argument ("need 0 to 2 arguments");
	}

	if (argc == 1)
	{
		try
		{
			parentKey = cmdline.createKey (0);
		}
		catch (exception & ex)
		{
			format = cmdline.arguments[0];
		}
	}
	else if (argc == 2)
	{
		parentKey = cmdline.createKey (0);
		format = cmdline.arguments[1];
	}

	ModulesPluginDatabase pluginDatabase;
	PluginSpec provides = pluginDatabase.lookupProvides (format);

	if (cmdline.verbose) std::cout << "found provider: " << provides.getName () << endl;

	Modules modules;
	PluginPtr plugin = modules.load (provides.getName (), cmdline.getPluginsConfig ());

	parentKey.setString (file);

	if (cmdline.withoutElektra)
	{
		parentKey.setMeta ("meta:/export/withoutElektra", "true");
	}

	if (cmdline.includeSessionStorage)
	{
		parentKey.setMeta ("meta:/export/includeRecordingSession", "true");
	}

	if (!ckdb::elektraRecordExportSession (*kdb, plugin->operator->(), *parentKey, *errorKey))
	{
		printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
		return 11;
	}

	printWarnings (cerr, errorKey, cmdline.verbose, cmdline.debug);

	return 0;
}
