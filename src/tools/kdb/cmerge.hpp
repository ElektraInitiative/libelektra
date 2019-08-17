/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef NEWMERGE_HPP
#define NEWMERGE_HPP

#include <command.hpp>
#include <kdb.hpp>

using namespace std;

class CMergeCommand : public Command
{
	kdb::KDB kdb;

public:
	CMergeCommand ();
	~CMergeCommand ();

	virtual int execute (Cmdline const & cmdline) override;

	virtual std::string getShortOptions () override
	{
		return "s";
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
		return "Does a three-way merge between keysets.\n"
		       "On success the resulting keyset will be saved to mergepath.\n"
		       "On unresolved conflicts nothing will be changed.\n";
	}
};

#endif
