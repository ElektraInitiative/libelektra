/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <merge.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <modules.hpp>

#include <iostream>
#include <string>

#include <mergehelper.hpp>
#include <merging/metamergestrategy.hpp>
#include <merging/threewaymerge.hpp>

using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

MergeCommand::MergeCommand ()
{
}

MergeCommand::~MergeCommand ()
{
}

int MergeCommand::execute (Cmdline const & cl)
{

	if (cl.arguments.size () < 4)
	{
		throw invalid_argument ("wrong number of arguments, 4 needed");
	}

	Key oursRoot = cl.createKey (0);
	Key theirsRoot = cl.createKey (1);
	Key baseRoot = cl.createKey (2);
	Key resultRoot = cl.createKey (3);

	KeySet ours;
	KeySet theirs;
	KeySet base;

	{
		KDB lkdb;
		lkdb.get (ours, oursRoot);
		ours = ours.cut (oursRoot);
		ours.lookup (oursRoot, KDB_O_POP);
		if (cl.verbose) std::cout << "we got ours: " << oursRoot << " with keys " << ours << std::endl;
	}
	{
		KDB lkdb;
		lkdb.get (theirs, theirsRoot);
		theirs = theirs.cut (theirsRoot);
		ours.lookup (oursRoot, KDB_O_POP);
		if (cl.verbose) std::cout << "we got theirs: " << theirsRoot << " with keys " << theirs << std::endl;
	}
	{
		KDB lkdb;
		lkdb.get (base, baseRoot);
		base = base.cut (baseRoot);
		ours.lookup (oursRoot, KDB_O_POP);
		if (cl.verbose) std::cout << "we got base: " << baseRoot << " with keys " << base << std::endl;
	}

	KeySet resultKeys;
	kdb.get (resultKeys, resultRoot);

	KeySet discard = resultKeys.cut (resultRoot);
	if (discard.size () != 0)
	{
		if (cl.force)
		{
			if (cl.verbose)
			{
				std::cout << "will remove " << discard.size () << " keys, because -f was given" << std::endl;
			}
		}
		else
		{
			std::cerr << discard.size () << " keys exist in merge resultpath, will quit. Use -f to override the keys there."
				  << std::endl;
		}
	}

	MergeHelper helper;
	ThreeWayMerge merger;

	helper.configureMerger (cl, merger);

	MergeResult result = merger.mergeKeySet (
		MergeTask (BaseMergeKeys (base, baseRoot), OurMergeKeys (ours, oursRoot), TheirMergeKeys (theirs, theirsRoot), resultRoot));

	helper.reportResult (cl, result, cout, cerr);

	int ret = 0;
	if (!result.hasConflicts ())
	{
		resultKeys.append (result.getMergedKeys ());
		kdb.set (resultKeys, resultRoot);
	}
	else
	{
		ret = 11;
	}

	return ret;
}
