/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef REMOVE_HPP
#define REMOVE_HPP

#include <command.hpp>

#include <kdb.hpp>

class RemoveCommand : public Command
{
	kdb::KDB kdb;

public:
	RemoveCommand ();
	~RemoveCommand ();

	virtual std::string getShortOptions () override
	{
		return "rfE";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Remove key(s) from key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
