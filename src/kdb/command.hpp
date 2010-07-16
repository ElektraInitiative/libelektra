#ifndef COMMAND_HPP
#define COMMAND_HPP

#include "print.hpp"

#include <exception>

/**
 * Base class for any exceptions thrown in a command.
 */
class CommandException : public std::exception
{
	virtual const char* what() const throw()
	{
		return "A situation had a appeared where the command had to abort";
	}
};

class CommandAbortException : public CommandException
{
	virtual const char* what() const throw()
	{
		return "Aborted by user request";
	}
};

class UnknownCommandException : public CommandException
{
	virtual const char* what() const throw()
	{
		return  "This command is not yet known to kdb.\n"
			"Common commands are get, set"
			"ls and mount.";
	}
};

class Command
{
public:
	virtual ~Command();
	virtual int execute (int argc, char**argv) = 0;
};

#endif
