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
	kdb::KeySet ks;

public:
	MountOdbcCommand ();
	~MountOdbcCommand();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "";
	}

	virtual std::string getShortHelpText () override
	{
		return "Mount a new ODBC data source";
	}

	virtual std::string getLongHelpText () override
	{
		return "Mount a new ODBC data source.\n"
		       "This is only a first alpha version for testing\n"
		       "the final version will offer more configuration options.";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif // MOUNTODBC_HPP