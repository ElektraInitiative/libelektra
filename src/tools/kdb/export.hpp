/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef EXPORT_H
#define EXPORT_H

#include <command.hpp>
#include <kdb.hpp>

class ExportCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	ExportCommand();
	~ExportCommand();

	virtual std::string getShortOptions() override
	{
		return "Ec";
	}

	virtual std::string getSynopsis() override
	{
		return "<source> [<format>]";
	}

	virtual std::string getShortHelpText() override
	{
		return "Export configuration from the key database.";
	}

	virtual std::string getLongHelpText() override
	{
		return
			"The export utility allows you to export\n"
			"all or parts of the configuration to stdout.\n"
			"\n"
			"The default format can be changed by\n"
			" /sw/kdb/current/format\n"
			"\n"
			"Example:\n"
			"kdb export system/sw > sw.ecf\n"
			"To make a backup of your whole configuration\n"
			"below system/sw"
			;
	}

	virtual int execute (Cmdline const& cmdline) override;
};

#endif
