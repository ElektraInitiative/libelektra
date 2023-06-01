/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef DIRNAME_HPP
#define DIRNAME_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class DirnameCommand : public Command
{
	kdb::KDB kdb;

public:
	DirnameCommand ();
	~DirnameCommand ();

	virtual std::string getShortOptions () override
	{
		return "n";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Get the cascading directory name of a key.";
	}

	virtual std::string getLongHelpText () override
	{
		return "For example, \"kdb dirname user:/key/subkey\" will yield \"/key\".\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
