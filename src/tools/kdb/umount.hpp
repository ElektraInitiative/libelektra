/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef UMOUNT_HPP
#define UMOUNT_HPP

#include <command.hpp>

#include <kdb.hpp>

class UmountCommand : public Command
{
	kdb::KDB kdb;

public:
	UmountCommand ();
	~UmountCommand ();

	virtual std::string getShortOptions () override
	{
		return "v";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Unmount backend from key database.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif
