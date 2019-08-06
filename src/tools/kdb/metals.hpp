/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef METALS_H
#define METALS_H

#include <command.hpp>
#include <kdb.hpp>

class MetaLsCommand : public Command
{
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	MetaLsCommand ();
	~MetaLsCommand ();

	virtual std::string getShortOptions () override
	{
		return "0";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Get all meta information of an individual key.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
