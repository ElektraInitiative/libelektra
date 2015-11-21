/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef REMOVE_HPP
#define REMOVE_HPP

#include <command.hpp>

#include <kdb.hpp>

class RemoveCommand : public Command
{
	kdb::KDB kdb;

public:
	RemoveCommand();
	~RemoveCommand();

	virtual std::string getShortOptions()
	{
		return "r";
	}

	virtual std::string getSynopsis()
	{
		return "<name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Remove key(s) from key database.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
