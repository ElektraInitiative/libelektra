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

	virtual std::string getShortOptions()
	{
		return "lc";
	}

	virtual std::string getSynopsis()
	{
		return "<plugin name> [<clause name>]";
	}

	virtual std::string getShortHelpText()
	{
		return "Print information about a plugin.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"Print out the information except configuration of a specific plugin.\n"
			"This means all exported functions and all info will be printed.\n"
			"\n"
			"The tool will use the configuration below system/elektra/modules/.\n"
			"\n"
			"If this information could not be found,\n"
			"(e.g. plugin not mounted anywhere)\n"
			"the module will be loaded dynamically\n"
			"and the information is requested directly.\n"
			"\n"
			" -l forces loading, even if it is mounted\n"
			"\n"
			"Clause name lets you to restrict to a specific\n"
			"clause to print out.\n"
			"\n"
			"Returns 0 on success.\n"
			"Returns 1 if clause was not found.\n"
			"\n"
			"E.g.\n"
			"Print out all information about dump plugin:\n"
			" kdb info dump\n"
			"\n"
			"Print out the licence of the resolver plugin:\n"
			" kdb info resolver licence\n"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
