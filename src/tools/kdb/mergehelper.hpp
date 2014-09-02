
#ifndef MERGEHELPER_HPP_
#define MERGEHELPER_HPP_

#include <map>

#include <command.hpp>
#include <merging/threewaymerge.hpp>

using namespace std;
using namespace kdb::tools::merging;

class MergeHelper
{
public:
	MergeHelper();
	virtual ~MergeHelper();
	vector<MergeConflictStrategy*> getAllStrategies();
	string getStrategyList();
	void parseStrategies(Cmdline const& cl, ThreeWayMerge& merger);
	void reportResult(Cmdline const& cl, MergeResult& result, ostream& out, ostream& err);


private:
	map<string, MergeConflictStrategy*> strategyMap;

};

#endif /* MERGEHELPER_HPP_ */
