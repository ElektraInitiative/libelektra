/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef MERGEHELPER_HPP_
#define MERGEHELPER_HPP_

#include <map>

#include <command.hpp>
#include <merging/mergeconfiguration.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb::tools::merging;

kdb::KeySet prependNamespace (kdb::KeySet const & resultKeys, std::string ns);
kdb::Key prependNamespace (kdb::Key const & root, std::string ns);
void applyMeta (kdb::KeySet & imported, kdb::KeySet const & base);

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
