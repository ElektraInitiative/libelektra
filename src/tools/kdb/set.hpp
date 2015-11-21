/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef SET_HPP
#define SET_HPP

#include <command.hpp>

#include <kdb.hpp>

class SetCommand : public Command
{
	kdb::KDB kdb;
public:
	SetCommand();
	~SetCommand();

	virtual std::string getShortOptions()
	{
		return "vN";
	}

	virtual std::string getSynopsis()
	{
		return "<name> [<value>]";
	}

	virtual std::string getShortHelpText()
	{
		return "Set the value of an individual key.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"If no value is given, it will be set to a null-value\n"
			"To get an empty value you need to quote like \"\" (depending on shell)"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
