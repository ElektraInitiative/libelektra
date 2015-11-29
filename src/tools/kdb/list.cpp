/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <list.hpp>

#include <iostream>

#include <cmdline.hpp>
#include <plugins.hpp>

using namespace kdb;
using namespace std;

ListCommand::ListCommand()
{}

int ListCommand::execute(Cmdline const& cl)
{
	std::vector<std::string> plugins = kdb::tools::listAllAvailablePlugins();

	if (cl.verbose) cout << "number of all plugins: " << plugins.size() << endl;

	for (auto & plugin : plugins)
	{
		std::cout << plugin;
		if (cl.null)
		{
			cout << '\0';
		}
		else
		{
			cout << endl;
		}
	}

	//TODO: check for ELEKTRA_STATIC and do ls in folder

	return 0;
}

ListCommand::~ListCommand()
{}
