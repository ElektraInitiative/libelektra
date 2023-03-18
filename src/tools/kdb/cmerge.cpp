/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "cmerge.hpp"
#include <internal/macros/utils.h>
#include <elektra/merge.h>
#include "keyset.hpp"
#include <cmdline.hpp>
#include <iostream>
#include <keysetio.hpp>
#include <string>

using ckdb::keyNew;

CMergeCommand::CMergeCommand ()
{
}

CMergeCommand::~CMergeCommand ()
{
}

int CMergeCommand::execute (Cmdline const & cl ELEKTRA_UNUSED)
{
	if (cl.arguments.size () < 4)
	{
		throw invalid_argument ("Wrong number of arguments! At least 4 arguments needed");
	}
	kdb::Key oursRoot = cl.createKey (0);
	kdb::Key theirsRoot = cl.createKey (1);
	kdb::Key baseRoot = cl.createKey (2);
	kdb::Key resultRoot = cl.createKey (3);
	ckdb::MergeStrategy strategy = ckdb::MERGE_STRATEGY_ABORT;
	if (cl.strategy == "preserve")
	{
		/** This is here for compatibility. The old merge has preserve as default as defined in cmdline.cpp.
		 *  As cmerge uses the existing functionality it is still default, even though it does not exist in cmerge.
		 *  Default in new merge is abort.
		 *
		 *  This will be obsolete as soon as cmerge supersedes the old merge.
		 */
		strategy = ckdb::MERGE_STRATEGY_ABORT;
	}
	else if (cl.strategy == "abort")
	{
		strategy = ckdb::MERGE_STRATEGY_ABORT;
	}
	else if (cl.strategy == "our")
	{
		strategy = ckdb::MERGE_STRATEGY_OUR;
	}
	else if (cl.strategy == "their")
	{
		strategy = ckdb::MERGE_STRATEGY_THEIR;
	}
	else
	{
		throw invalid_argument ("'" + cl.strategy + "' is not a valid strategy. Valid strategies are: abort, our, their");
	}

	kdb::KeySet ours;
	kdb::KeySet theirs;
	kdb::KeySet base;

	{
		kdb::KDB lkdb;
		lkdb.get (ours, oursRoot);
		ours = ours.cut (oursRoot);
		ours.lookup (oursRoot, 0);
		if (cl.verbose) std::cout << "we got ours: " << oursRoot << " with keys\n" << ours << std::endl;
	}
	{
		kdb::KDB lkdb;
		lkdb.get (theirs, theirsRoot);
		theirs = theirs.cut (theirsRoot);
		ours.lookup (oursRoot, 0);
		if (cl.verbose) std::cout << "we got theirs: " << theirsRoot << " with keys\n" << theirs << std::endl;
	}
	{
		kdb::KDB lkdb;
		lkdb.get (base, baseRoot);
		base = base.cut (baseRoot);
		ours.lookup (oursRoot, 0);
		if (cl.verbose) std::cout << "we got base: " << baseRoot << " with keys\n" << base << std::endl;
	}
	kdb::KeySet keysAtResultRoot;
	kdb.get (keysAtResultRoot, resultRoot);
	kdb::KeySet discard = keysAtResultRoot.cut (resultRoot);
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
			throw CommandAbortException ("There are keys in the result path. Use -f to override them.");
		}
	}
	ckdb::KeySet * c_ours = ours.getKeySet ();
	ckdb::KeySet * c_theirs = theirs.getKeySet ();
	ckdb::KeySet * c_base = base.getKeySet ();
	ckdb::Key * informationKey = keyNew ("/", KEY_END);
	ckdb::KeySet * c_merge_result = elektraMerge (c_ours, oursRoot.getKey (), c_theirs, theirsRoot.getKey (), c_base,
						      baseRoot.getKey (), resultRoot.getKey (), strategy, informationKey);
	int numberOfConflicts = elektraMergeGetConflicts (informationKey);
	ckdb::keyDel (informationKey);
	if (c_merge_result != NULL)
	{
		kdb::KeySet merge_result = c_merge_result;
		if (keysAtResultRoot.append (merge_result) < 0)
		{
			return 1;
		}
		if (kdb.set (keysAtResultRoot, resultRoot) < 0)
		{
			return 1;
		}
		return 0;
	}
	else
	{
		if (numberOfConflicts > 0 && strategy == ckdb::MERGE_STRATEGY_ABORT)
		{
			return 2;
		}
		else
		{
			return 1;
		}
	}
}
