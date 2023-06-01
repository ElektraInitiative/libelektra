/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef BASENAME_HPP
#define BASENAME_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class BasenameCommand : public Command
{
	kdb::KDB kdb;

public:
	BasenameCommand ();
	~BasenameCommand ();

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
		return "Get the basename of a key.";
	}

	virtual std::string getLongHelpText () override
	{
		return "For example, \"kdb basename user:/key/subkey\" will yield \"subkey\".\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
