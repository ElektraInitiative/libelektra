/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef SHELL_GET_HPP
#define SHELL_GET_HPP

#include <command.hpp>

#include <kdb.hpp>

class ShellGetCommand : public Command
{

public:
	ShellGetCommand ();
	~ShellGetCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "<name> <default value>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Get the value of an individual key within a shell script.";
	}

	virtual std::string getLongHelpText () override
	{
		return "The get command does not work properly within shell scripts\n"
		       "because it may issue an error instead of printing a value.\n"
		       "ShellGet guarantees that no error is printed, in any event\n"
		       "(except for wrong parameters and options)\n"
		       "a value will be printed, either the one retrieved from the\n"
		       "system or a fallback default value given as second\n"
		       "parameter";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
