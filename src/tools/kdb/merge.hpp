#ifndef MERGE_HPP
#define MERGE_HPP

#include <command.hpp>
#include <kdb.hpp>

using namespace std;

class MergeCommand : public Command
{
	kdb::KDB kdb;

public:
	MergeCommand();
	~MergeCommand();

	virtual int execute (Cmdline const& cmdline);

	virtual std::string getShortOptions()
	{
		return "iHsvf";
	}

	virtual std::string getSynopsis()
	{
		return "[options] ourpath theirpath basepath resultpath";
	}

	virtual std::string getShortHelpText()
	{
		return "Three-way merge of KeySets.";
	}

	virtual std::string getLongHelpText()
	{
		return
			"Does a three-way merge between keysets.\n"
			"On success the resulting keyset will be saved to mergepath.\n"
			"On unresolved conflicts nothing will be changed.\n"
			"\n"
			"Conflicts in a merge can be resolved using a strategy with -s.\n"
			"\n"
			"ourpath ..    path to the keyset to serve as ours\n"
			"theirpath ..  path to the keyset to serve as theirs\n"
			"basepath ..   path to the base keyset\n"
			"resultpath .. path without keys where the merged keyset will be saved\n"
			"              (use -b to override results)\n"
			;
	}

};

#endif
