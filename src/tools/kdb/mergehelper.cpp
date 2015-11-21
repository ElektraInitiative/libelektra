/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <iostream>
#include <vector>

#include <cmdline.hpp>
#include <keysetio.hpp>

#include <mergehelper.hpp>
#include <merging/interactivemergestrategy.hpp>
#include <merging/automergeconfiguration.hpp>
#include <merging/onesidemergeconfiguration.hpp>
#include <merging/overwritemergeconfiguration.hpp>
#include <merging/importmergeconfiguration.hpp>

using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

MergeHelper::MergeHelper()
{
	// TODO: this is just a quickfix, find a better solution
	// without eager instantiating all the strategies. Maybe even automatically
	// discover all available strategies
	// comment markus: the factory could be part of libtools
	configurationMap.insert (make_pair ("preserve", new AutoMergeConfiguration()));

	configurationMap.insert (make_pair ("ours", new OneSideMergeConfiguration(OURS)));
	configurationMap.insert (make_pair ("theirs", new OneSideMergeConfiguration(THEIRS)));

	// primarily used for import
	configurationMap.insert (make_pair ("cut", new OverwriteMergeConfiguration(THEIRS)));
	configurationMap.insert (make_pair ("import", new ImportMergeConfiguration()));

}

MergeHelper::~MergeHelper()
{
	vector<MergeConfiguration *> configurations = getAllConfigurations();
	for (vector<MergeConfiguration *>::iterator it = configurations.begin(); it != configurations.end (); ++it)
	{
		delete (*it);
	}
}

vector<MergeConfiguration *> MergeHelper::getAllConfigurations()
{
	vector<MergeConfiguration *> result;
	for (map<string, MergeConfiguration *>::iterator it = configurationMap.begin (); it != configurationMap.end (); ++it)
	{
		result.push_back ((*it).second);
	}

	return result;
}

string MergeHelper::getConfigurationList()
{
	ostringstream oss;
	for (map<string, MergeConfiguration *>::iterator it = configurationMap.begin (); it != configurationMap.end (); ++it)
	{
		oss << (*it).first << ",";
	}

	return oss.str ();
}

void MergeHelper::configureMerger(Cmdline const& cl, ThreeWayMerge& merger)
{


	if (cl.interactive)
	{
		merger.addConflictStrategy (new InteractiveMergeStrategy (cin, cout));
		cout << "Chose interactive merge" << endl;
	}
	else
	{
		if (configurationMap.find (cl.strategy) == configurationMap.end ())
		{
			throw invalid_argument (
					"'" + cl.strategy + "' is not a valid strategy. Valid strategies are: " + getConfigurationList ());
		}

		MergeConfiguration *configuration = configurationMap[cl.strategy];
		configuration->configureMerger(merger);
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
