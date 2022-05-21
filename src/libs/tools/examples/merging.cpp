/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.hpp>
#include <keysetio.hpp>

#include <iostream>
#include <memory>
#include <string>

#include <merging/onesidemergeconfiguration.hpp>
#include <merging/threewaymerge.hpp>


using namespace kdb;
using namespace kdb::tools::merging;
using namespace std;

int main ()
{
	KeySet ours;
	KeySet theirs;
	KeySet base;

	// the root of the subtree containing our keys (i.e. our side of the merge)
	Key oursRoot ("user:/ours", KEY_END);

	// the root of the subtree containing their keys (i.e. their side of the merge)
	Key theirsRoot ("user:/theirs", KEY_END);

	// the root of the subtree containing the base keys (i.e. the common ancestor of the merge)
	Key baseRoot ("user:/base", KEY_END);

	// the root of the subtree that will contain the merge result
	Key resultRoot ("user:/result", KEY_END);

	// Step 1: retrieve clean KeySets containing only those
	// keys that should be merged. This is a bit trickier than
	// it seems at first. Have a look at the documentation of kdbGet
	// for detailed information
	// things to note:
	//   * use blocks with local KDB instances so we don't have to worry about
	//     writing the keys back
	//   * remove the root key itself from the result KeySet because it usually
	//     contains the mounted filename and cannot be merged anyway
	// Also have a look at the documentation of kdbSet()
	// (https://doc.libelektra.org/api/master/html/group__kdb.html#ga11436b058408f83d303ca5e996832bcf).
	// The merging framework can also be used to resolve conflicts resulting from
	// concurrent calls to kdbSet() as described in the example of kdbSet().
	{
		KDB lkdb;
		lkdb.get (ours, oursRoot);
		ours = ours.cut (oursRoot);
		ours.lookup (oursRoot, KDB_O_POP);
		lkdb.get (theirs, theirsRoot);
		theirs = theirs.cut (theirsRoot);
		theirs.lookup (theirsRoot, KDB_O_POP);
		lkdb.get (base, baseRoot);
		base = base.cut (baseRoot);
		base.lookup (baseRoot, KDB_O_POP);
	}


	// Step 2: Make sure that no keys reside below the intended merge result root
	// Usually the merge can be either aborted if this is the case or the existing
	// keys can be overwritten.
	KeySet resultKeys;
	kdb::KDB kdb;
	kdb.get (resultKeys, resultRoot);

	KeySet discard = resultKeys.cut (resultRoot);
	if (discard.size () != 0)
	{
		// handle existing keys below the result root
		return -1;
	}

	ThreeWayMerge merger;

	// Step 3: Decide which resolution strategies to use. The strategies are registered
	// with the merge and applied in order as soon as a conflict is detected. If a strategy
	// marks a conflict as resolved, no further strategies are consulted. Therefore the order
	// in which they are registered is absolutely crucial. With this chaining the strategies
	// remain simple, but can be combined to powerful resolution strategies.
	// Have a look at the strategy documentation for further details on what they do and how they work.
	// The unit tests also provide insight into how the strategies work.

	// In order to simplify the initialization, predefined merge configurations exist.
	// in this example we first resolve all the keys that can be automatically
	// resolved (e.g. only one side was modified). This is exactly the use case of the
	// AutoMergeConfiguration.

	AutoMergeConfiguration configuration;
	configuration.configureMerger (merger);

	// Step 4: Perform the actual merge
	MergeResult result = merger.mergeKeySet (
		MergeTask (BaseMergeKeys (base, baseRoot), OurMergeKeys (ours, oursRoot), TheirMergeKeys (theirs, theirsRoot), resultRoot));

	// Step 5: work with the result. The merger will return an object containing information
	// about the merge result.
	if (!result.hasConflicts ())
	{
		// output some statistical information
		cout << result.getMergedKeys ().size () << " keys in the result" << endl;
		cout << result.getNumberOfEqualKeys () << " keys were equal" << endl;
		cout << result.getNumberOfResolvedKeys () << " keys were resolved" << endl;

		// write the result
		resultKeys.append (result.getMergedKeys ());
		kdb.set (resultKeys, resultRoot);

		return 0;
	}
	else
	{
		KeySet conflicts = result.getConflictSet ();

		cerr << conflicts.size () << " conflicts were detected that could not be resolved automatically:" << endl;

		for (Key current : conflicts)
		{
			// For each unresolved conflict there is a conflict key in the merge result.
			// This conflict key contains meta information about the reason of the conflict.
			// In particular the metakeys conflict/operation/our and conflict/operation/their contain
			// the operations done on our version of the key and their version of the key relative to
			// the base version of the key.
			string ourConflict = current.getMeta<string> ("conflict/operation/our");
			string theirConflict = current.getMeta<string> ("conflict/operation/their");

			cerr << current << endl;
			cerr << "ours: " << ourConflict << ", theirs: " << theirConflict << endl;
			cerr << endl;
		}

		cerr << "Merge unsuccessful." << endl;

		return -1;
	}
}
