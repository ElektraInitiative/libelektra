/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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
	MetaLsCommand();
	~MetaLsCommand();

	virtual std::string getShortOptions()
	{
		return "0";
	}

	virtual std::string getSynopsis()
	{
		return "<name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Get all meta information of an individual key.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};

#endif
