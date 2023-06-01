/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef METAREMOVE_HPP
#define METAREMOVE_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class MetaRemoveCommand : public Command
{
	kdb::KDB kdb;

public:
	MetaRemoveCommand ();
	~MetaRemoveCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "<key-name> <metaname>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Remove a metakey.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
