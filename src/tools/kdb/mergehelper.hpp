/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef MERGEHELPER_HPP_
#define MERGEHELPER_HPP_

#include <map>

#include <command.hpp>
#include <merging/mergeconfiguration.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb::tools::merging;

kdb::KeySet prependNamespace (kdb::KeySet const & resultKeys, std::string const & ns);
kdb::Key prependNamespace (kdb::Key const & root, std::string const & ns);
kdb::Key removeNamespace (kdb::Key const & root);
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
