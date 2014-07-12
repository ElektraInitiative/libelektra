#include <merge.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>

#include <iostream>
#include <string>
#include <vector>

#include <merging/metamergestrategy.hpp>
#include <merging/automergestrategy.hpp>
#include <merging/onesidestrategy.hpp>
#include <merging/interactivemergestrategy.hpp>

using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

MergeCommand::MergeCommand()
{
	// TODO: this is just a quickfix, find a better solution
	// without eager instantiating all the strategies. Maybe even automatically
	// discover all available strategies
	strategyMap.insert (make_pair ("auto", new AutoMergeStrategy()));

	strategyMap.insert (make_pair ("ours", new OneSideStrategy(OURS)));
	strategyMap.insert (make_pair ("theirs", new OneSideStrategy(THEIRS)));
	strategyMap.insert (make_pair ("base", new OneSideStrategy(BASE)));

	merger = ThreeWayMerge();
	// TODO: for now we have to position this strategy manually
	// to avoid meta information loss
	metaStrategy = new MetaMergeStrategy(merger);
	merger.addConflictStrategy(metaStrategy);
}

MergeCommand::~MergeCommand()
{
	vector<MergeConflictStrategy*> strategies = getAllStrategies();
	for (vector<MergeConflictStrategy*>::iterator it = strategies.begin(); it != strategies.end (); ++it)
	{
		delete (*it);
	}

	delete (metaStrategy);
}

vector<MergeConflictStrategy*> MergeCommand::getAllStrategies()
{
	vector<MergeConflictStrategy*> result;
	for (map<string, MergeConflictStrategy*>::iterator it = strategyMap.begin (); it != strategyMap.end (); ++it)
	{
		result.push_back ((*it).second);
	}

	return result;
}

string MergeCommand::getStrategyList()
{
	ostringstream oss;
	for (map<string, MergeConflictStrategy*>::iterator it = strategyMap.begin (); it != strategyMap.end (); ++it)
	{
		oss << (*it).first << ",";
	}

	return oss.str ();
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

	if (cl.interactive)
	{
		merger.addConflictStrategy (new InteractiveMergeStrategy (cin, cout));
		cout << "Choose interactive merge" << endl;
	}
	else
	{
		if (cl.strategy.size () > 0)
		{
			istringstream sstream (cl.strategy);
			string current;
			while (getline (sstream, current, ','))
			{
				if (strategyMap.find (current) == strategyMap.end ())
				{
					throw invalid_argument (
							"'" + current + "' is not a valid strategy. Valid strategies are: " + getStrategyList ());
				}

				MergeConflictStrategy *strategy = strategyMap[current];
				merger.addConflictStrategy (strategy);
			}


		}
	}


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

