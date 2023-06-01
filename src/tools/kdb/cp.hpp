/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CP_HPP
#define CP_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class CpCommand : public Command
{
	kdb::KDB kdb;

public:
	CpCommand ();
	~CpCommand ();

	virtual std::string getShortOptions () override
	{
		return "rf";
	}

	virtual std::string getSynopsis () override
	{
		return "<source> <dest>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Copy keys within the key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
