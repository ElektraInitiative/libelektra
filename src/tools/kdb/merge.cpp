#include <merge.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>

#include <iostream>
#include <string>

using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

MergeCommand::MergeCommand()
{
}

int MergeCommand::execute(Cmdline const& cl)
{
	int ret = 0;

	if (cl.arguments.size () != 4)
	{
		throw invalid_argument ("wrong number of arguments, 4 needed");
	}

	Key mergeRoot (cl.arguments[0], KEY_END);
	if (!mergeRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[0] + " is not a valid keyname");
	}

	Key ourRoot (cl.arguments[1], KEY_END);
	if (!ourRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[1] + " is not a valid keyname");
	}

	Key theirRoot (cl.arguments[2], KEY_END);
	if (!theirRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[2] + " is not a valid keyname");
	}

	Key baseRoot (cl.arguments[3], KEY_END);
	if (!baseRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[3] + " is not a valid keyname");
	}

	KeySet original;
	KeySet base;
	KeySet ours;
	KeySet theirs;

	kdb.get (original, baseRoot);
	base = original.cut (mergeRoot);
	ours = original.cut (ourRoot);
	theirs = original.cut (theirRoot);
	original.append (base);
	original.append (ours);
	original.append (theirs);
	ThreeWayMerge merger;
	MergeResult result = merger.mergeKeySet (
			MergeTask (
					BaseMergeKeys (base, mergeRoot),
					OurMergeKeys (ours, ourRoot),
					TheirMergeKeys (theirs, theirRoot), baseRoot));

	KeySet empty;
	if (result.hasConflicts ())
	{
		cerr << "Conflicts where detected that could not be resolved automatically:" << endl;

		KeySet conflicts = result.getConflictSet();
		conflicts.rewind();
		Key current;
		while ((current = conflicts.next()))
		{
			string ourConflict = current.getMeta<string> ("conflict/operation/our");
			string theirConflict = current.getMeta<string> ("conflict/operation/their");

			cerr << current << endl;
			cerr << "ours: " << ourConflict << ", theirs: " << theirConflict << endl;
			cerr << endl;
		}

		cerr << "Merge unsuccessful." << endl;
		ret = -1;
	}

	original.append (result.getMergedKeys ());
	kdb.set (original, baseRoot);

	return ret;
}

MergeCommand::~MergeCommand()
{
}
