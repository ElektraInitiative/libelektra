/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef IMPORT_HPP
#define IMPORT_HPP

#include <command.hpp>
#include <kdb.hpp>

class ImportCommand : public Command
{
	kdb::KDB kdb;

public:
	ImportCommand();
	~ImportCommand();

	virtual std::string getShortOptions()
	{
		return "sv";
	}

	virtual std::string getSynopsis()
	{
		return "<destination> [<format>]";
	}

	virtual std::string getShortHelpText()
	{
		return "Import configuration to the key database.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"The import utility allows you to import\n"
			"all or parts of the configuration from stdin.\n"
			"\n"
			"The default format can be changed by\n"
			" /sw/kdb/current/format\n"
			"\n"
			"Example:\n"
			"cat sw.ecf | kdb import system/sw\n"
			"To restore a backup of your whole configuration\n"
			"below system/sw\n"
			;
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
