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
       Key errorKey ("/");

#ifdef _WIN32
       string file = "CON";
#else
       string file = "/dev/stdout";
#endif
       std::string format = "ansible";

       ModulesPluginDatabase pluginDatabase;
       PluginSpec provides = pluginDatabase.lookupProvides (format);

       if (cmdline.verbose) std::cout << "found provider: " << provides.getName () << endl;

       Modules modules;
       PluginPtr plugin = modules.load (provides.getName (), cmdline.getPluginsConfig ());

       Key parentKey;
       parentKey.setString (file);


       if (!ckdb::elektraRecordExportSession (*kdb, plugin->operator->(), *parentKey, *errorKey))
       {
	       printError (cerr, errorKey, cmdline.verbose, cmdline.debug);
	       return 1;
       }

       printWarnings (cerr, errorKey, cmdline.verbose, cmdline.debug);

       return 0;
}
