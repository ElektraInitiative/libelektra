/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CMERGE_HPP
#define CMERGE_HPP

#include <command.hpp>
#include <kdb.hpp>

using namespace std;

class MergeCommand : public Command
{
	kdb::KDB kdb;

public:
	MergeCommand ();
	~MergeCommand ();

	virtual int execute (Cmdline const & cmdline) override;

	virtual std::string getShortOptions () override
	{
		return "sf";
	}

	virtual std::string getSynopsis () override
	{
		return "[options] ourpath theirpath basepath resultpath";
	}

	virtual std::string getShortHelpText () override
	{
		return "Three-way merge of KeySets.";
	}

	virtual std::string getLongHelpText () override
	{
		return "Performs a three-way merge between keysets.\n"
		       "On success the resulting keyset will be saved to resultpath.\n"
		       "On unresolved conflicts nothing will be changed.\n";
	}
};

#endif
