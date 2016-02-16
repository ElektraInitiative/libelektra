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

	virtual std::string getShortOptions() override
	{
		return "n";
	}

	virtual std::string getSynopsis() override
	{
		return "<key-name> <meta-name>";
	}

	virtual std::string getShortHelpText() override
	{
		return "Get a meta value.";
	}

	virtual std::string getLongHelpText() override
	{
		return
			"Meta key are information about keys.\n"
			;
	}

	virtual int execute (Cmdline const& cmdline) override;
};

#endif
