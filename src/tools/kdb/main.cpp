/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <cmdline.hpp>
#include <command.hpp>
#include <external.hpp>
#include <factory.hpp>
#include <toolexcept.hpp>

#include <kdb.hpp>
#include <key.hpp>

using namespace kdb;
using namespace std;

int displayHelp (std::string app, Factory const & f)
{
	std::cout << "Usage: " << app << " <command> [args]\n" << std::endl;
	std::cout << app << " is a program to manage elektra's key database.\n"
		  << "Run a command with -H or --help as args to show a help text for\n"
		  << "a specific command.\n"
		  << std::endl;
	std::cout << "Known commands are:" << std::endl;
	std::vector<std::string> commands;
	try
	{
		commands = f.getCommands ();
	}
	catch (kdb::KDBException const & ce)
	{
		std::cerr << "There is a severe problem with your installation!\n"
			  << "kdbOpen() failed with the info:" << std::endl
			  << ce.what () << std::endl;
		std::cout << "Listing of internal commands failed" << std::endl;
		return 8;
	}
	for (auto & command : commands)
	{
		std::cout << command << std::endl;
	}
	return 0;
}

void displayVersion ()
{
	kdb::KDB kdb;
	kdb::KeySet versions;
	kdb::Key k ("system/elektra/version", KEY_END);
	kdb.get (versions, k);
	kdb::Key kdb_version = versions.lookup ("system/elektra/version/constants/KDB_VERSION");
	if (!kdb_version)
	{
		cerr << "Could not lookup KDB_VERSION key" << endl;
	}
	else
	{
		cout << "KDB_VERSION: " << kdb_version.getString () << endl;
	}
	kdb::Key so_version = versions.lookup ("system/elektra/version/constants/SO_VERSION");
	if (!so_version)
	{
		cerr << "Could not lookup SO_VERSION key" << endl;
	}
	else
	{
		cout << "SO_VERSION: " << so_version.getString () << endl;
	}
}

int main (int argc, char ** argv)
{
	Factory f;

	if (argc < 2)
	{
		return displayHelp (argv[0], f);
	}

	string command = argv[1];
	if (command == "help" || command == "-H" || command == "--help")
	{
		if (argc >= 3)
		{
			runManPage (argv[2]);
		}
		else
		{
			runManPage ();
		}

		return displayHelp (argv[0], f);
	}

	if (command == "-V" || command == "--version")
	{
		displayVersion ();
		return 0;
	}

	try
	{
		std::vector<char *> origArguments (argv + 1, argv + argc);
		origArguments.push_back (0);
		CommandPtr cmd = f.get (command);
		Cmdline cl (argc, argv, cmd.get ());

		if (cl.help)
		{
			runManPage (command, cl.profile);
			// does not return, but may throw
		}

		// version and invalidOpt might be implemented
		// differently for external command
		if (dynamic_cast<ExternalCommand *> (cmd.get ()))
		{
			tryExternalCommand (&origArguments[0]);
			// does not return, but may throw
		}

		if (cl.version)
		{
			displayVersion ();
			return 0;
		}

		if (cl.invalidOpt)
		{
			cerr << cl << endl;
			return 1;
		}

		try
		{
			return cmd->execute (cl);
		}
		catch (std::invalid_argument const & ia)
		{
			cerr << "Invalid arguments passed: " << ia.what () << endl << endl;
			cerr << cl << endl;
			return 2;
		}
	}
	catch (CommandException const & ce)
	{
		std::cerr << "The command " << command << " terminated unsuccessfully with the info: " << ce.what () << std::endl;
		return 3;
	}
	catch (UnknownCommand const & uc)
	{
		std::cerr << "The command " << command << " is not known" << std::endl;
		displayHelp (argv[0], f);
		return 4;
	}
	catch (kdb::KDBException const & ce)
	{
		std::cerr << "The command " << command << " failed while accessing the key database with the info:" << std::endl
			  << ce.what () << std::endl;
		return 5;
	}
	catch (std::exception const & ce)
	{
		std::cerr << "The command " << command << " terminated unsuccessfully with the info:" << std::endl
			  << ce.what () << std::endl;
		return 6;
	}
	catch (...)
	{
		std::cerr << "Unknown error" << std::endl;
		displayHelp (argv[0], f);
		return 7;
	}
}
