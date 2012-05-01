#ifndef VALIDATION_HPP
#define VALIDATION_HPP

#include <command.hpp>

#include <kdb.hpp>

class ValidationCommand : public Command
{
	kdb::KDB kdb;

public:
	ValidationCommand();
	~ValidationCommand();

	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual std::string getShortHelpText()
	{
		return "Set an value together with regex to check it.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"<key-name> <value> <regular expression> [<message>]\n"
			"\n"
			"This is a convenience function to set validation meta data for a key.\n"
			"It supports regular expressions as defined in extended regular expressions.\n"
			"\n"
			"The message is shown whenever someone tries to set a value which does\n"
			"not match the regular expression.\n";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
