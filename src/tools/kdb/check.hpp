#ifndef CHECK_HPP
#define CHECK_HPP

#include <command.hpp>

#include <kdb.hpp>

class CheckCommand : public Command
{
public:
	CheckCommand();
	~CheckCommand();

	virtual std::string getShortOptions()
	{
		return "v";
	}

	virtual std::string getSynopsis()
	{
		return "[<name>]";
	}

	virtual std::string getShortHelpText()
	{
		return "Do some basic checks on a plugin.\n"
			"If no arguments are given checks on key database\n"
			"are done instead.\n"
			"\n"
			"Return values on kdb checking:\n"
			" 0 .. everything ok (no output)\n"
			" 1 .. warning on open\n"
			" 2 .. warning on close\n"
			" 4 .. error on open (a plugin is broken!)\n"
			" 8 .. error on close (a plugin is broken!)\n"
			"\n"
			"Or a sum of above\n"
			"\n"
			"Return values on plugin checking:\n"
			" 0 .. everything ok (no output)\n"
			" 1 .. no plugin found\n"
			" 2 .. plugin did not pass checks\n"
			" 3 .. plugin has warnings\n"
			"\n"
			"Please report any issues you found on http://www.libelektra.org\n"
			"\n";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
