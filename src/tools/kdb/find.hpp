/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef KDB_FIND_H
#define KDB_FIND_H

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class FindCommand : public Command
{
public:
	FindCommand ();
	~FindCommand ();

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
		return "Find keys with a given regex.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
