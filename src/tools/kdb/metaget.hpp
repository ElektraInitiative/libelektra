/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef METAGET_HPP
#define METAGET_HPP

#include <command.hpp>

#include <kdb.hpp>

class MetaGetCommand : public Command
{
	kdb::KDB kdb;

public:
	MetaGetCommand();
	~MetaGetCommand();

	virtual std::string getShortOptions()
	{
		return "n";
	}

	virtual std::string getSynopsis()
	{
		return "<key-name> <meta-name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Get a meta value.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"Meta key are information about keys.\n"
			"\n"
			"Return values:\n"
			"1 .. key not found\n"
			"2 .. metakey not found\n"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
