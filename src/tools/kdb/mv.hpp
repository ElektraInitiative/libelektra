/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef MV_HPP
#define MV_HPP

#include <command.hpp>

#include <kdb.hpp>

class MvCommand : public Command
{
	kdb::KDB kdb;

public:
	MvCommand ();
	~MvCommand ();

	virtual std::string getShortOptions () override
	{
		return "rv";
	}

	virtual std::string getSynopsis () override
	{
		return "<source> <dest>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Move configuration within the key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
