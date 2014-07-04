#include <merge.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <keysetio.hpp>
#include <mergetools.hpp>

#include <iostream>
#include <string>


using namespace kdb;
using namespace kdb::tools;
using namespace std;


MergeCommand::MergeCommand()
{}

int MergeCommand::execute (Cmdline const& cl)
{
	int ret = 0;

	if (cl.arguments.size() != 4)
	{
		throw invalid_argument("wrong number of arguments, 4 needed");
	}

	Key root1 (cl.arguments[0], KEY_END);
	if (!root1.isValid())
	{
		throw invalid_argument(cl.arguments[0] +
				" is not a valid keyname");
	}

	Key root2 (cl.arguments[1], KEY_END);
	if (!root2.isValid())
	{
		throw invalid_argument(cl.arguments[1] +
				" is not a valid keyname");
	}
	Key root3 (cl.arguments[2], KEY_END);
	if (!root3.isValid())
	{
		throw invalid_argument(cl.arguments[2] +
				" is not a valid keyname");
	}
	Key root4 (cl.arguments[3], KEY_END);
	if (!root4.isValid())
	{
		throw invalid_argument(cl.arguments[3] +
				" is not a valid keyname");
	}

	KeySet original;
	KeySet base;
	KeySet ours;
	KeySet theirs;

	kdb.get(original, root4);
	base = original.cut(root1);
	ours = original.cut(root2);
	theirs = original.cut(root3);
	original.append(base);
	original.append(ours);
	original.append(theirs);
	MergeResult result = ThreeWayMerge::mergeKeySet(base, root1, ours, root2, theirs, root3, root4);
	
	KeySet empty;
	if( result.hasConflicts()){
		cerr << "Merge unsuccessful." << endl;
		ret = -1;
	}

	original.append(result.getMergedKeys());
	kdb.set(original, root4);

	return ret;
}


MergeCommand::~MergeCommand()
{}
