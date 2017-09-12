/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <factory.hpp>
#include <listcommands.hpp>

#include <iostream>

#include <cmdline.hpp>

using namespace kdb;
using namespace std;

ListCommandsCommand::ListCommandsCommand ()
{
}

int ListCommandsCommand::execute (Cmdline const & cl)
{
	Factory f;

	std::vector<std::string> commands;
	try
	{
		if (cl.verbose)
		{
			commands = f.getPrettyCommands ();
		}
		else
		{
			commands = f.getCommands ();
		}
	}
	catch (kdb::KDBException const & ce)
	{
		std::cerr << "Sorry, I have a severe problem, it seems like I am not installed correctly!\n"
			  << "kdbOpen() failed with the info:" << std::endl
			  << ce.what () << std::endl
			  << "Please report the issue at https://issues.libelektra.org/";
		return 8;
	}

	if (cl.verbose) cout << "number of all commands: " << commands.size () << endl;

	for (auto & command : commands)
	{
		std::cout << command;

		if (cl.null)
		{
			cout << '\0';
		}
		else
		{
			cout << endl;
		}
	}

	return 0;
}

ListCommandsCommand::~ListCommandsCommand ()
{
}
