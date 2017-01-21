/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <superls.hpp>

#include <functional>
#include <iostream>
#include <stack>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace std;

SuperLsCommand::SuperLsCommand ()
{
}

int SuperLsCommand::execute (const Cmdline & cl)
{
	if (cl.arguments.size () != 1)
	{
		throw invalid_argument ("wrong number of arguments, 1 needed");
	}

	cout.setf (ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (ios_base::skipws);
	}

	// Determine the actual root key, as for completion purpose originalRoot may not exist
	KDB kdb;
	const Key originalRoot = cl.createKey (0);
	Key root = originalRoot;
	KeySet ks;
	printWarnings (cerr, root);

	kdb.get (ks, root);
	if (!ks.lookup (root) && !root.getBaseName ().empty ())
	{
		root = getParentKey (root);
	}
	ks = ks.cut (root);

	// Now analyze the completion possibilities and print the results
	addMountpoints (ks);
	KeySet virtualKeys;
	printResult (originalRoot, root, analyze (ks, root, virtualKeys), virtualKeys);
	printWarnings (cerr, root);

	return 0;
}

void SuperLsCommand::addMountpoints (KeySet & ks)
{
	KDB kdb;
	Key mountpointPath ("system/elektra/mountpoints", KEY_END);
	KeySet mountpoints;

	kdb.get (mountpoints, mountpointPath);
	mountpoints = mountpoints.cut (mountpointPath);

	for (const Key mountpoint : mountpoints)
	{
		if (mountpoint.isDirectBelow (mountpointPath))
		{
			const string actualName = mountpoints.lookup (mountpoint.getFullName () + "/mountpoint").getString ();
			ks.append (Key (actualName, KEY_END));
		}
	}

	printWarnings (cerr, mountpointPath);
}

const map<Key, pair<int, int>> SuperLsCommand::analyze (const KeySet & ks, const Key root, KeySet & virtualKeys)
{
	map<Key, pair<int, int>> hierarchy;
	stack<Key> keyStack;
	Key parent;
	Key last;

	ks.rewind ();
	if (!(ks.next ()))
	{
		return hierarchy;
	}

	int curDepth = 0;
	keyStack.push (ks.current ());
	while (!keyStack.empty ())
	{
		Key current = keyStack.top ();
		keyStack.pop ();
		if (current.isDirectBelow (last))
		{ // down in the hierarchy, last element is new parent
			parent = last;
			curDepth++;
		}

		cout << "Processing " << current << " vs last " << last << " at depth " << curDepth << endl;

		if (current.isDirectBelow (parent))
		{ // hierarchy continues at the current level
			increaseCount (hierarchy, parent, [](int p) { return p; });
			increaseCount (hierarchy, current, [=](int) { return curDepth; });
		}
		else
		{ // hierarchy does not fit the current parent, expand the current key to the stack to find the new parent
			cout << "expanding at " << current << " vs root " << root << " which is belowOrSame "
			     << current.isBelowOrSame (root) << endl;
			// Go back up in the hierarchy until we encounter a known key or are back at the namespace level
			while (current.isBelow (root) && !current.getBaseName ().empty () && hierarchy[current].first == 0)
			{
				cout << "expanding at " << current << " vs root " << root << endl;
				keyStack.push (current);
				virtualKeys.append (current);
				current = getParentKey (current);
			}
			parent = getParentKey (current);
			curDepth = hierarchy[current].second;
		}

		// Current hierarchy processed, we can resume with the next
		if (keyStack.empty () && (ks.next ()))
		{
			keyStack.push (ks.current ());
		}
		last = current;
	}

	return hierarchy;
}

const Key SuperLsCommand::getParentKey (const Key key)
{
	return Key (key.getFullName ().erase (key.getFullName ().size () - key.getBaseName ().size ()), KEY_END);
}

void SuperLsCommand::increaseCount (map<Key, pair<int, int>> & hierarchy, const Key key, const function<int(int)> depthIncreaser)
{
	const pair<int, int> prev = hierarchy[key];
	hierarchy[key] = pair<int, int> (prev.first + 1, depthIncreaser (prev.second));
}

void SuperLsCommand::printResult (const Key originalRoot, const Key root, const map<Key, pair<int, int>> & hierarchy,
				  const KeySet & virtualKeys)
{
	const function<bool(string)> filterPredicate = determineFilterPredicate (originalRoot, root);

	// Adjust the output offset, in case the given string exists in the hierarchy but not in the original ks
	const int offset = originalRoot != root && virtualKeys.lookup (originalRoot);
	const int minDepth = 0 + offset;
	const int maxDepth = false ? numeric_limits<int>::max () : (1 + offset);
	cout << "max is " << maxDepth << " and min is " << minDepth << endl;

	cout << "dumping all results" << endl;
	for (const auto & it : hierarchy)
	{
		cout << it.first << " " << it.second.first << " " << it.second.second << endl;
	}

	cout << endl << "dumping filtered results" << endl;
	for (const auto & it : hierarchy)
	{
		if (filterPredicate (it.first.getFullName ()) && it.second.second > minDepth && it.second.second <= maxDepth)
		{
			cout << it.first << (it.second.first > 1 ? " node " + to_string (it.second.first - 1) : " leaf") << endl;
		}
	}
}

const function<bool(string)> SuperLsCommand::determineFilterPredicate (const Key originalRoot, const Key root)
{
	if (root == originalRoot)
	{
		return [](string) { return true; };
	}
	const string fullName = originalRoot.getFullName ();
	return [=](string test) {
		// For cascading keys, we ignore the preceding namespace for filtering
		const int cascadationOffset = (root.isCascading () ? test.find ("/") : 0);
		return fullName.size () <= test.size () && equal (fullName.begin (), fullName.end (), test.begin () + cascadationOffset);
	};
}

SuperLsCommand::~SuperLsCommand ()
{
}
