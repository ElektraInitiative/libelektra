/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

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

	virtual std::string getShortOptions() override
	{
		return "";
	}

	virtual std::string getSynopsis() override
	{
		return "<key-name> <value> <regular expression> [<message>]";
	}

	virtual std::string getShortHelpText() override
	{
		return "Set a value together with a validation regex.";
	}

	virtual std::string getLongHelpText() override
	{
		return
			"This is a convenience function to set validation meta data for a key.\n"
			"It supports regular expressions as defined in extended regular expressions.\n"
			"\n"
			"The message is shown whenever someone tries to set a value which does\n"
			"not match the regular expression."
			"\n"
			"The command will only work if the validation plugin is mounted\n"
			"where the key resides."
			;
	}

	virtual int execute (Cmdline const& cmdline) override;
};

#endif
