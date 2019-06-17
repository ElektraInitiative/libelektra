/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <export.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <modules.hpp>
#include <toolexcept.hpp>

#include <iostream>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

ExportCommand::ExportCommand ()
{
}

int ExportCommand::execute (Cmdline const & cl)
{
	size_t argc = cl.arguments.size ();
	if (argc != 1 && argc != 2 && argc != 3)
	{
		throw invalid_argument ("need 1 to 3 arguments");
	}

	Key root = cl.createKey (0);

	kdb.get (ks, root);
	printWarnings (cerr, root, cl.verbose, cl.debug);

	KeySet part (ks.cut (root));

	if (cl.withoutElektra)
	{
		Key systemElektra ("system/elektra", KEY_END);
		part.cut (systemElektra);
	}

	string format = cl.format;
	if (argc > 1) format = cl.arguments[1];

#ifdef _WIN32
	string file = "CON";
#else
	string file = "/dev/stdout";
#endif
	if (argc > 2 && cl.arguments[2] != "-") file = cl.arguments[2];

	Modules modules;
	PluginPtr plugin = modules.load (format, cl.getPluginsConfig ());

	Key errorKey (root);
	errorKey.setString (file);

	plugin->set (part, errorKey);

	printWarnings (cerr, errorKey, cl.verbose, cl.debug);
	printError (cerr, errorKey, cl.verbose, cl.debug);

	return 0;
}

ExportCommand::~ExportCommand ()
{
}
