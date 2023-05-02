/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include <coloredkdbio.hpp>

#include <cmdline.hpp>
#include <command.hpp>
#include <csignal>
#include <external.hpp>
#include <factory.hpp>

#include <kdb.hpp>
#include <key.hpp>

#include <cpp-main.h>

using namespace kdb;
using namespace std;

int displayHelp (std::string app, Factory const & f)
{
	std::cout << "Usage: " << app << " <command> [args]\n" << std::endl;
	std::cout << app << " is a program to manage Elektra's key database.\n"
		  << "Please run a command with -H or --help as args to show a help text for\n"
		  << "a specific command.\n"
		  << std::endl;
	std::cout << "Known commands are:" << std::endl;
	std::vector<std::string> commands;
	try
	{
		commands = f.getPrettyCommands ();
	}
	catch (kdb::KDBException const & ce)
	{
		std::cerr << "Sorry, I have a severe problem, it seems like I am not installed correctly!\n"
			  << "kdbOpen() failed with the info:" << std::endl
			  << ce.what () << std::endl
			  << "Please report the issue at https://issues.libelektra.org/";
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
	kdb::Key k ("system:/elektra/version", KEY_END);
	kdb.get (versions, k);
	kdb::Key kdb_version = versions.lookup ("system:/elektra/version/constants/KDB_VERSION");
	if (!kdb_version)
	{
		cerr << "Could not lookup KDB_VERSION key" << endl;
	}
	else
	{
		cout << "KDB_VERSION: " << kdb_version.getString () << endl;
	}
	kdb::Key so_version = versions.lookup ("system:/elektra/version/constants/SO_VERSION");
	if (!so_version)
	{
		cerr << "Could not lookup SO_VERSION key" << endl;
	}
	else
	{
		cout << "SO_VERSION: " << so_version.getString () << endl;
	}
}

void printSignal (int signum)
{
	switch (signum)
	{
	case SIGILL:
		cerr << "SIGILL";
		break;
	case SIGABRT:
		cerr << "SIGABRT";
		break;
	case SIGFPE:
		cerr << "SIGFPE";
		break;
	case SIGSEGV:
		cerr << "SIGSEGV";
		break;
	}
}


void catchSignal (int signum)
{
	cerr << endl << "Sorry, I crashed by the signal ";
	printSignal (signum);
	cerr << endl << "This should not have happened!" << endl;
	cerr << endl << "Please report the issue at https://issues.libelektra.org/" << std::endl;
	signal (SIGABRT, SIG_DFL);
	abort ();
}

void setupSignal (int signum)
{
	if (signal (signum, catchSignal) == SIG_ERR)
	{
		cerr << "Sorry, I could not setup signal ";
		printSignal (signum);
		cerr << " because: " << strerror (errno) << std::endl;
		cerr << "Please report the issue at https://issues.libelektra.org/" << std::endl;
	}
}

void setupSignals ()
{
	setupSignal (SIGILL);
	setupSignal (SIGABRT);
	setupSignal (SIGFPE);
	setupSignal (SIGSEGV);
}

int cpp_main (int argc, char ** argv)
{
	setupSignals ();
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

	bool printVerbose = false;
	bool printDebug = false;
	try
	{
		std::vector<char *> origArguments (argv + 1, argv + argc);
		origArguments.push_back (0);
		CommandPtr cmd = f.get (command);
		Cmdline cl (argc, argv, cmd.get ());
		printVerbose = cl.verbose;
		printDebug = cl.debug;

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
			cerr << "Sorry, I could not process the arguments: " << ia.what () << endl << endl;
			cerr << cl << endl;
			return 2;
		}
	}
	catch (CommandException const & ce)
	{
		std::cerr << "The command " << getErrorColor (ANSI_COLOR::BOLD) << argv[0] << " " << command
			  << getErrorColor (ANSI_COLOR::RESET) << " terminated " << getErrorColor (ANSI_COLOR::RED) << "unsuccessfully"
			  << getErrorColor (ANSI_COLOR::RESET) << " with the info:\n"
			  << ce.what () << std::endl;
		if (ce.errorCode () != 3 && (ce.errorCode () < 11 || ce.errorCode () > 20))
		{
			std::cerr << "Command used invalid return value (" << ce.errorCode ()
				  << "), please report the issue at https://issues.libelektra.org/" << std::endl;
			return 3;
		}
		return ce.errorCode ();
	}
	catch (UnknownCommand const & uc)
	{
		std::cerr << "The command " << getErrorColor (ANSI_COLOR::BOLD) << argv[0] << " " << command
			  << getErrorColor (ANSI_COLOR::RESET) << " is " << getErrorColor (ANSI_COLOR::RED) << "not known"
			  << getErrorColor (ANSI_COLOR::RESET) << std::endl;
		displayHelp (argv[0], f);
		return 4;
	}
	catch (kdb::KDBMountException const & ce)
	{
		std::cerr << ce.what () << std::endl;
		return 5;
	}
	catch (kdb::KDBException const & ce)
	{
		std::cerr << ce.whatWithArguments (printVerbose, printDebug) << std::endl;
		return 5;
	}
	catch (std::exception const & ce)
	{
		std::cerr << "The command " << getErrorColor (ANSI_COLOR::BOLD) << argv[0] << " " << command
			  << getErrorColor (ANSI_COLOR::RESET) << " terminated " << getErrorColor (ANSI_COLOR::RED) << "unsuccessfully"
			  << getErrorColor (ANSI_COLOR::RESET) << " with the info:" << endl
			  << ce.what () << endl
			  << "Please report the issue at https://issues.libelektra.org/" << std::endl;
		return 7;
	}
	catch (...)
	{
		std::cerr << "The command " << getErrorColor (ANSI_COLOR::BOLD) << argv[0] << " " << command
			  << getErrorColor (ANSI_COLOR::RESET) << " terminated with an " << getErrorColor (ANSI_COLOR::RED)
			  << "unknown error" << getErrorColor (ANSI_COLOR::RESET) << endl
			  << "Please report the issue at https://issues.libelektra.org/" << std::endl;
		displayHelp (argv[0], f);
		return 7;
	}
}
