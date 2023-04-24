/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef LS_H
#define LS_H

#include "./coloredkdbio.hpp"
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
		return "mM0";
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
	void checkArguments (Cmdline const & cl);
	void printResults (kdb::KeySet const & part, const int rootDepth, Cmdline const & cl);
	int getDepth (kdb::Key const & key);
	bool shallShowNextLevel (const std::string argument);
};

#endif
