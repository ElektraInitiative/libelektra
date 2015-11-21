/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef MV_HPP
#define MV_HPP

#include <command.hpp>

#include <kdb.hpp>

class MvCommand : public Command
{
	kdb::KDB kdb;

public:
	MvCommand();
	~MvCommand();

	virtual std::string getShortOptions()
	{
		return "rv";
	}

	virtual std::string getSynopsis()
	{
		return "<source> <dest>";
	}

	virtual std::string getShortHelpText()
	{
		return "Move configuration within the key database.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
