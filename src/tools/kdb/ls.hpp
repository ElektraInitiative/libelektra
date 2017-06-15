/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef LS_H
#define LS_H

#include "coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

class LsCommand : public Command
{
	kdb::Key root;
	kdb::KDB kdb;
	kdb::KeySet ks;

public:
	LsCommand ();
	~LsCommand ();

	virtual std::string getShortOptions () override
	{
		return "dMmv0C";
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

	virtual int execute (const Cmdline & cmdline) override;

private:
	void checkArguments (const Cmdline & cl);
	void printResults (const kdb::KeySet & part, const int rootDepth, const Cmdline & cl);
	int getDepth (const kdb::Key & key);
	bool shallShowNextLevel (const std::string argument);
};

#endif
