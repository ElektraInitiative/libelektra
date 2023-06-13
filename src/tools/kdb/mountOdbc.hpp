/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef MOUNTODBC_HPP
#define MOUNTODBC_HPP

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class MountOdbcCommand : public Command
{
	kdb::Key root;
	kdb::KDB kdb;

public:
	MountOdbcCommand ();
	~MountOdbcCommand () override;

	std::string getShortOptions () override
	{
		return "f";
	}

	std::string getSynopsis () override
	{
		return "<data source name> <user name> <password> <table name> <key column name> <value column name> <meta table name> <mt "
		       "key column name> <mt metakey column name> <mt metavalue column name> <timeout (s)> <mountpoint>";
	}

	std::string getShortHelpText () override
	{
		return "Mount a new ODBC data source";
	}

	std::string getLongHelpText () override
	{
		return "You need a working ODBC driver and configuration on your system to use this backend.\n"
		       "If no username and/or password is needed or if they are defined in the ODBC data source config, "
		       "please pass \"\" (empty string) as value for these arguments.\n"
		       "If you need more information about setting up and using the ODBC backend, there is a detailed tutorial about "
		       "this topic available. (see https://www.libelektra.org/tutorials/readme)\n\n"
		       "There is no special command for unmounting ODBC mountpoints, just use 'kdb umount <mountpoint>' for unmounting";
	}

	int execute (Cmdline const & cmdline) override;


private:
	void checkArguments (Cmdline const & cl);
};


#endif // MOUNTODBC_HPP
