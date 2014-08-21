
#include <iostream>
#include <vector>

#include <cmdline.hpp>
#include <keysetio.hpp>

#include <mergehelper.hpp>
#include <merging/automergestrategy.hpp>
#include <merging/onesidestrategy.hpp>
#include <merging/newkeystrategy.hpp>
#include <merging/interactivemergestrategy.hpp>

using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

MergeHelper::MergeHelper()
{
	// TODO: this is just a quickfix, find a better solution
	// without eager instantiating all the strategies. Maybe even automatically
	// discover all available strategies
	// comment markus: the factory could be part of libtools
	strategyMap.insert (make_pair ("preserve", new AutoMergeStrategy()));

	strategyMap.insert (make_pair ("ours", new OneSideStrategy(OURS)));
	strategyMap.insert (make_pair ("theirs", new OneSideStrategy(THEIRS)));
	strategyMap.insert (make_pair ("base", new OneSideStrategy(BASE)));
	strategyMap.insert (make_pair ("newkey", new NewKeyStrategy()));
	strategyMap.insert (make_pair ("ourvalue", new OneSideStrategy(OURS)));
	strategyMap.insert (make_pair ("theirvalue", new OneSideStrategy(THEIRS)));
}

MergeHelper::~MergeHelper()
{
	vector<MergeConflictStrategy*> strategies = getAllStrategies();
	for (vector<MergeConflictStrategy*>::iterator it = strategies.begin(); it != strategies.end (); ++it)
	{
		delete (*it);
	}
}

vector<MergeConflictStrategy*> MergeHelper::getAllStrategies()
{
	vector<MergeConflictStrategy*> result;
	for (map<string, MergeConflictStrategy*>::iterator it = strategyMap.begin (); it != strategyMap.end (); ++it)
	{
		result.push_back ((*it).second);
	}

	return result;
}

string MergeHelper::getStrategyList()
{
	ostringstream oss;
	for (map<string, MergeConflictStrategy*>::iterator it = strategyMap.begin (); it != strategyMap.end (); ++it)
	{
		oss << (*it).first << ",";
	}

	return oss.str ();
}

void MergeHelper::parseStrategies(Cmdline const& cl, ThreeWayMerge& merger)
{


	if (cl.interactive)
	{
		merger.addConflictStrategy (new InteractiveMergeStrategy (cin, cout));
		cout << "Chose interactive merge" << endl;
	}
	else
	{
		if (cl.strategy.size () > 0)
		{
			// strategies are comma separated, split them
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
}

void MergeHelper::reportResult(Cmdline const& cl, MergeResult& result, ostream& out, ostream& err)
{

	if (!result.hasConflicts ())
	{
		if (cl.verbose)
		{
			out << result.getMergedKeys().size() << " keys in the result" << endl;
			out << result.getNumberOfEqualKeys() << " keys were equal" << endl;
			out << result.getNumberOfResolvedKeys() << " keys were resolved" << endl;
		}
	}
	else
	{
		KeySet conflicts = result.getConflictSet();

		err << conflicts.size() << " conflicts were detected that could not be resolved automatically:" << endl;
		conflicts.rewind();
		Key current;
		while ((current = conflicts.next()))
		{
			string ourConflict = current.getMeta<string> ("conflict/operation/our");
			string theirConflict = current.getMeta<string> ("conflict/operation/their");

			err << current << endl;
			err << "ours: " << ourConflict << ", theirs: " << theirConflict << endl;
			err << endl;
		}

		err << "Merge unsuccessful." << endl;
	}
}
