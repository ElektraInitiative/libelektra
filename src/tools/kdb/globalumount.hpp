/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef GLOBALUMOUNT_HPP
#define GLOBALUMOUNT_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class GlobalUmountCommand : public Command
{
	kdb::KDB kdb;

public:
	GlobalUmountCommand ();
	~GlobalUmountCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Unmounts a global plugin from key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
