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

	if (cl.arguments.size () < 3)
	{
		throw invalid_argument ("wrong number of arguments, 3 needed");
	}

	Key ourRoot (cl.arguments[0], KEY_END);
	if (!ourRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[0] + " is not a valid keyname");
	}

	Key theirRoot (cl.arguments[1], KEY_END);
	if (!theirRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[1] + " is not a valid keyname");
	}

	Key baseRoot (cl.arguments[2], KEY_END);
	if (!baseRoot.isValid ())
	{
		throw invalid_argument (cl.arguments[2] + " is not a valid keyname");
	}

	Key mergeRoot;

	if (cl.arguments.size() >= 4)
	{
		mergeRoot = Key (cl.arguments[3], KEY_END);

		if (!mergeRoot.isValid ())
		{
			throw invalid_argument (cl.arguments[3] + "is not a valid keyname");
		}
 	}
	else
 	{
		if (cl.overrideBase)
		{
			mergeRoot = baseRoot;
		}
		else
		{
			cerr << "if you really want to override the base keys, specifiy the -b option" << endl;
			return -1;
		}
 	}

	KeySet base;
	KeySet ours;
	KeySet theirs;

	kdb.get (base, baseRoot);
	kdb.get (ours, ourRoot);
	kdb.get (theirs, theirRoot);

	base = base.cut (baseRoot);
	ours = ours.cut (ourRoot);
	theirs = theirs.cut (theirRoot);

	// TODO: check for last modification time (otherwise the result flaps)
	ThreeWayMerge merger;
	merger.addConflictStrategy(new AutoMergeStrategy());
	MergeResult result = merger.mergeKeySet (
			MergeTask (
					BaseMergeKeys (base, baseRoot),
					OurMergeKeys (ours, ourRoot),
					TheirMergeKeys (theirs, theirRoot), mergeRoot));

	KeySet empty;
	if (!result.hasConflicts ())
	{
		KeySet resultKeys = result.getMergedKeys();
		kdb.set (resultKeys, mergeRoot);

		cout << resultKeys.size() << " keys in the result" << endl;
		cout << result.getNumberOfEqualKeys() << " keys were equal" << endl;
		cout << result.getNumberOfResolvedKeys() << " keys were resolved" << endl;
	}
	else
	{
		KeySet conflicts = result.getConflictSet();

		cerr << conflicts.size() + " conflicts were detected that could not be resolved automatically:" << endl;
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


	return ret;
}

MergeCommand::~MergeCommand()
{
}
