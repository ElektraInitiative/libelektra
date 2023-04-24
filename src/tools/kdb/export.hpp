/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef EXPORT_H
#define EXPORT_H

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class ExportCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	ExportCommand ();
	~ExportCommand ();

	virtual std::string getShortOptions () override
	{
		return "Ec";
	}

	virtual std::string getSynopsis () override
	{
		return "<source> [<format>]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Export configuration from the key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "The export utility allows you to export\n"
		       "all or parts of the configuration to stdout.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
