/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef SET_HPP
#define SET_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class SetCommand : public Command
{
	kdb::KDB kdb;

public:
	SetCommand ();
	~SetCommand ();

	virtual std::string getShortOptions () override
	{
		return "qf";
	}

	virtual std::string getSynopsis () override
	{
		return "<name> <value>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Set the value of an individual key.";
	}

	virtual std::string getLongHelpText () override
	{
		return "To set an empty value you need to quote like \"\" (depending on shell)\n"
		       "To set a negative value you need to use '--' to stop option processing.\n"
		       "(e.g. 'kdb set -- /tests/neg -3')\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
