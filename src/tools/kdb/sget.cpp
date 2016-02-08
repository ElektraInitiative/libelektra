/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <sget.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <iostream>

using namespace std;
using namespace kdb;

ShellGetCommand::ShellGetCommand()
{}

int ShellGetCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 2) throw invalid_argument ("Need two arguments");

	std::string default_value = cl.arguments[1];

	try
	{
		kdb::KDB kdb;
		KeySet conf;
		Key x = cl.createKey(0);

		kdb.get(conf, x);
		Key k = conf.lookup(x);

		if (!k)
		{
			throw invalid_argument("Did not find key");
		}

		cout << k.getString();

	}
	catch (...)
	{
		std::cout << default_value;
	}

	return 0;
}

ShellGetCommand::~ShellGetCommand()
{}
