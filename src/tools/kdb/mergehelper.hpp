/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef MERGEHELPER_HPP_
#define MERGEHELPER_HPP_

#include <map>

#include <command.hpp>
#include <merging/mergeconfiguration.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb::tools::merging;

class MergeHelper
{
public:
	MergeHelper ();
	virtual ~MergeHelper ();
	vector<MergeConfiguration *> getAllConfigurations ();
	string getConfigurationList ();
	void configureMerger (Cmdline const & cl, ThreeWayMerge & merger);
	void reportResult (Cmdline const & cl, MergeResult & result, ostream & out, ostream & err);


private:
	map<string, MergeConfiguration *> configurationMap;
};

#endif /* MERGEHELPER_HPP_ */
