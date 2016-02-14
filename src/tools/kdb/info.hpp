/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef INFO_HPP
#define INFO_HPP

#include <command.hpp>

#include <kdb.hpp>

class InfoCommand : public Command
{
	kdb::KDB kdb;

public:
	InfoCommand();
	~InfoCommand();

	virtual std::string getShortOptions() override
	{
		return "lc";
	}

	virtual std::string getSynopsis() override
	{
		return "<plugin name> [<clause name>]";
	}

	virtual std::string getShortHelpText() override
	{
		return "Print information about a plugin.";
	}

	virtual std::string getLongHelpText() override
	{
		return
			"Print out the information of a specific plugin's contract.\n"
			"\n"
			"E.g.\n"
			"Print out all information about dump plugin:\n"
			" kdb info dump\n"
			"\n"
			"Print out the licence of the resolver plugin:\n"
			" kdb info resolver licence\n"
			;
	}

	virtual int execute (Cmdline const& cmdline) override;
};

#endif
