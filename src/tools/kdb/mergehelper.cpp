/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <iostream>
#include <vector>

#include <cmdline.hpp>
#include <keysetio.hpp>

#include <mergehelper.hpp>
#include <merging/automergeconfiguration.hpp>
#include <merging/importmergeconfiguration.hpp>
#include <merging/interactivemergestrategy.hpp>
#include <merging/onesidemergeconfiguration.hpp>
#include <merging/overwritemergeconfiguration.hpp>

using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

MergeHelper::MergeHelper ()
{
	// TODO: this is just a quickfix, find a better solution
	// without eager instantiating all the strategies. Maybe even automatically
	// discover all available strategies
	// comment markus: the factory could be part of libtools
	configurationMap.insert (make_pair ("preserve", new AutoMergeConfiguration ()));

	configurationMap.insert (make_pair ("ours", new OneSideMergeConfiguration (OURS)));
	configurationMap.insert (make_pair ("theirs", new OneSideMergeConfiguration (THEIRS)));

	// primarily used for import
	configurationMap.insert (make_pair ("cut", new OverwriteMergeConfiguration (THEIRS)));
	configurationMap.insert (make_pair ("import", new ImportMergeConfiguration ()));
}

MergeHelper::~MergeHelper ()
{
	vector<MergeConfiguration *> configurations = getAllConfigurations ();
	for (auto & configuration : configurations)
	{
		delete (configuration);
	}
}

vector<MergeConfiguration *> MergeHelper::getAllConfigurations ()
{
	vector<MergeConfiguration *> result;
	for (auto & elem : configurationMap)
	{
		result.push_back ((elem).second);
	}

	return result;
}

string MergeHelper::getConfigurationList ()
{
	ostringstream oss;
	for (auto & elem : configurationMap)
	{
		oss << (elem).first << ",";
	}

	return oss.str ();
}

void MergeHelper::configureMerger (Cmdline const & cl, ThreeWayMerge & merger)
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
			throw invalid_argument ("'" + cl.strategy +
						"' is not a valid strategy. Valid strategies are: " + getConfigurationList ());
		}

		MergeConfiguration * configuration = configurationMap[cl.strategy];
		configuration->configureMerger (merger);
	}
}

void MergeHelper::reportResult (Cmdline const & cl, MergeResult & result, ostream & out, ostream & err)
{
	if (!result.hasConflicts ())
	{
		if (cl.verbose)
		{
			out << result.getMergedKeys ().size () << " keys in the result" << endl;
			out << result.getNumberOfEqualKeys () << " keys were equal" << endl;
			out << result.getNumberOfResolvedKeys () << " keys were resolved" << endl;
		}
	}
	else
	{
		KeySet conflicts = result.getConflictSet ();

		err << conflicts.size () << " conflicts were detected that could not be resolved automatically:" << endl;

		for (Key current : conflicts)
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


KeySet prependNamespace (KeySet const & resultKeys, std::string const & ns)
{
	KeySet ret;
	for (auto const & k : resultKeys)
	{
		ret.append (prependNamespace (k, ns));
	}
	return ret;
}

Key prependNamespace (Key const & root, std::string const & ns)
{
	Key ret = root.dup ();
	if (ret.isCascading ())
	{
		ret.setName (ns + ":" + root.getName ());
	}
	return ret;
}

Key removeNamespace (Key const & root)
{
	Key ret = root.dup ();
	if (!ret.isCascading ())
	{
		string keyName = ret.getName ();
		string cascadingName = keyName.substr (keyName.find (":") + 1);
		ret.setName (cascadingName);
	}
	return ret;
}

void applyMeta (KeySet & imported, KeySet const & base)
{
	for (auto k : imported)
	{
		Key b = base.lookup (k, 0);
		if (b)
		{
			k.copyAllMeta (b);
		}
	}
}
