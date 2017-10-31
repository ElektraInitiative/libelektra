/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef SHELL_HPP
#define SHELL_HPP

#include <command.hpp>

#include <kdb.hpp>

class ShellCommand : public Command
{
	kdb::KDB kdb;
	std::string supportedCommands;

public:
	ShellCommand ();
	~ShellCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "";
	}

	virtual std::string getShortHelpText () override
	{
		return "Start a kdb shell.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Use an interactive mode to view or edit\n"
		       "the key database.\n"
		       "\n"
		       "Supported commands are:\n" +
		       supportedCommands +
		       "\n"
		       "Read the API docu\n"
		       "to see what these commands are doing.\n"
		       "\n"
		       "An internal current key and keyset assist\n"
		       "to build up data structures which can be\n"
		       "applied to the key database.";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
