/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef IMPORT_HPP
#define IMPORT_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class ImportCommand : public Command
{
	kdb::KDB kdb;

public:
	ImportCommand ();
	~ImportCommand ();

	virtual std::string getShortOptions () override
	{
		return "scE";
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
