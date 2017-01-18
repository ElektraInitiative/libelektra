/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef SUPERLS_H
#define SUPERLS_H

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class SuperLsCommand : public Command
{
	kdb::Key root;
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	SuperLsCommand ();
	~SuperLsCommand ();

	virtual std::string getShortOptions () override
	{
		return "r0";
	}

	virtual std::string getSynopsis () override
	{
		return "<name>";
	}

	virtual std::string getShortHelpText () override
	{
		return "List the names of keys below a given name.";
	}

	virtual std::string getLongHelpText () override
	{
		return "List all keys below given name.\n"
		       "To also retrieve the value use the\n"
		       "export command.";
	}

	virtual int execute (Cmdline const & cmdline) override;

private:
	kdb::Key getParentKey(kdb::Key key);
	
};

#endif
