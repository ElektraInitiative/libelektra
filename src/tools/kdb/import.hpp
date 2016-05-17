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
#include "coloredkdbio.hpp"
#include <kdb.hpp>

class ImportCommand : public Command
{
	kdb::KDB kdb;

public:
	ImportCommand ();
	~ImportCommand ();

	virtual std::string getShortOptions () override
	{
		return "svcC";
	}

	virtual std::string getSynopsis () override
	{
		return "<destination> [<format>]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Import configuration to the key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "The import utility allows you to import\n"
		       "all or parts of the configuration from stdin.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
