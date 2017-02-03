/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "complete.hpp"

#include <functional>
#include <iostream>
#include <limits>
#include <stack>

#include "cmdline.hpp"
#include <kdb.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace std;

CompleteCommand::CompleteCommand ()
{
}

/*
 * McCabe complexity of 11, 1 caused by verbose switch so actually its ok
 */
int CompleteCommand::execute (const Cmdline & cl)
{
	if (cl.maxDepth <= cl.minDepth)
	{
		throw invalid_argument ("the maximum depth has to be larger than the minimum depth");
	}
	if (cl.maxDepth < 0)
	{
		throw invalid_argument ("the maximum depth has to be a positive number");
	}
	if (cl.minDepth < 0)
	{
		throw invalid_argument ("the minimum depth has to be a positive number");
	}

	cout.setf (ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (ios_base::skipws);
	}

	// For namespace completion, we get all available keys via / as the actual root and filter via the argument
	const bool hasArgument = cl.arguments.size () > 0;
	// Without specifying an argument, it will show every possible completion, like calling kdb complete ""
	const string originalInput = hasArgument ? cl.arguments[cl.arguments.size () - 1] : "";
	const Key originalUnprocessedKey (originalInput, KEY_END);
	KDB kdb;
	// Determine the actual root key, as for completion purpose originalRoot may not exist
	// If the input is not a valid key, it could still be a bookmark or a namespace
	const bool isArgumentValidKey = originalUnprocessedKey.isValid () || (!originalInput.empty () && originalInput[0] == '+');
	const Key originalRoot = isArgumentValidKey ? cl.createKey (cl.arguments.size () - 1) : originalUnprocessedKey;
	Key root = isArgumentValidKey ? originalRoot : Key ("/", KEY_END);
	KeySet ks;
	printWarnings (cerr, root);

	kdb.get (ks, root);
	if (!ks.lookup (root) && !root.getBaseName ().empty ())
	{
		if (cl.verbose)
		{
			cout << originalRoot << " does not exist or is a cascading key, using " << root << " as the current completion path"
			     << endl;
		}
		root = getParentKey (root);
	}
	ks = ks.cut (root);

	// Now analyze the completion possibilities and print the results
	addMountpoints (ks, root);
	KeySet virtualKeys;
	printResults (originalInput, originalRoot, root, analyze (ks, root, virtualKeys, cl), virtualKeys, cl);
	printWarnings (cerr, root);

	return 0;
}

void CompleteCommand::addMountpoints (KeySet & ks, const Key root)
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
			Key mountpointKey (actualName, KEY_END);
			if (mountpointKey.isBelow (root))
			{
				ks.append (mountpointKey);
			}
		}
	}

	printWarnings (cerr, mountpointPath);
}

void CompleteCommand::addNamespaces (map<Key, pair<int, int>> & hierarchy)
{
	// Always add them on level 0
	const string namespaces[] = {
		"spec/", "proc/", "dir/", "user/", "system/",
	};

	// Check for new namespaces, issue a warning in case
	for (elektraNamespace ens = KEY_NS_FIRST; ens <= KEY_NS_LAST; ++ens)
	{
		// since ens are numbers, there is no way to get a string representation if not found in that case
		bool found = false;
		for (const string ns : namespaces)
		{
			found = found || ckdb::keyGetNamespace (Key (ns, KEY_END).getKey ()) == ens;
		}
		if (!found)
		{
			cerr << "Missing namespace detected:" << ens << ". \nPlease report this issue." << endl;
		}
	}

	for (const string ns : namespaces)
	{
		const Key nsKey (ns, KEY_END);
		// Check for outdated namespaces, issue a warning in case
		if (ckdb::keyGetNamespace (nsKey.getKey ()) == KEY_NS_EMPTY)
		{
			cerr << "Outdated namespace detected:" << ns << ".\nPlease report this issue." << endl;
		}
		hierarchy[nsKey] = pair<int, int> (1, 0);
	}
}

/*
 * McCabe complexity of 13, 3 caused by debug switches so actually its ok
 */
const map<Key, pair<int, int>> CompleteCommand::analyze (const KeySet & ks, const Key root, KeySet & virtualKeys, const Cmdline & cl)
{
	map<Key, pair<int, int>> hierarchy;
	stack<Key> keyStack;
	Key parent;
	Key last;
	addNamespaces (hierarchy);

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

		if (cl.debug)
		{
			cout << "Analyzing " << current << ", the last processed key is " << last << " and the parent is " << parent
			     << " at depth " << curDepth << endl;
		}

		if (current.isDirectBelow (parent))
		{ // hierarchy continues at the current level
			increaseCount (hierarchy, parent, [](int p) { return p; });
			increaseCount (hierarchy, current, [=](int) { return curDepth; });
		}
		else
		{ // hierarchy does not fit the current parent, expand the current key to the stack to find the new parent
			while (current.isBelow (root) && !current.getBaseName ().empty () && hierarchy[current].first == 0)
			{ // Go back up in the hierarchy until we encounter a known key or are back at the namespace level
				if (cl.debug)
				{
					cout << "Expanding " << current << endl;
				}
				keyStack.push (current);
				virtualKeys.append (current);
				current = getParentKey (current);
			}
			parent = getParentKey (current);
			curDepth = hierarchy[current].second;
			if (cl.debug)
			{
				cout << "Finished expanding, resume at " << current << " with parent " << parent << " and depth "
				     << curDepth << endl;
			}
		}

		if (keyStack.empty () && (ks.next ()))
		{ // Current hierarchy processed, we can resume with the next
			keyStack.push (ks.current ());
		}
		last = current;
	}

	return hierarchy;
}

const Key CompleteCommand::getParentKey (const Key key)
{
	return Key (key.getFullName ().erase (key.getFullName ().size () - key.getBaseName ().size ()), KEY_END);
}

void CompleteCommand::increaseCount (map<Key, pair<int, int>> & hierarchy, const Key key, const function<int(int)> depthIncreaser)
{
	const pair<int, int> prev = hierarchy[key];
	hierarchy[key] = pair<int, int> (prev.first + 1, depthIncreaser (prev.second));
}

/*
 * McCabe complexity of 11, 3 caused by verbose switch so actually its ok
 */
void CompleteCommand::printResults (const string originalInput, const Key originalRoot, const Key root,
				    const map<Key, pair<int, int>> & hierarchy, const KeySet & virtualKeys, const Cmdline & cl)
{
	const function<bool(string)> filterPredicate = determineFilterPredicate (originalInput, originalRoot, root);

	// Adjust the output offset, in case the given string exists in the hierarchy but not in the original ks
	// or for namespace completion
	const bool limitMaxDepth = cl.maxDepth != numeric_limits<int>::max ();
	const int offset = originalRoot != root && virtualKeys.lookup (originalRoot) ? 1 : (originalRoot.getFullName ().empty () ? -1 : 0);
	const int minDepth = cl.minDepth + offset;
	const int maxDepth = limitMaxDepth ? cl.maxDepth + offset : cl.maxDepth;
	if (cl.verbose)
	{
		cout << "Showing results for a minimum depth of " << minDepth;
		if (limitMaxDepth)
		{
			cout << " and a maximum depth of " << maxDepth;
		}
		else
		{
			cout << " and no maximum depth";
		}
		cout << endl;
	}

	for (const auto & it : hierarchy)
	{
		if (filterPredicate (it.first.getFullName ()) && it.second.second > minDepth && it.second.second <= maxDepth)
		{
			printResult (it, cl.verbose);
		}
	}
}

void CompleteCommand::printResult (const pair<Key, pair<int, int>> & current, const bool verbose)
{
	cout << current.first;
	if (current.second.first > 1)
	{
		cout << "/";
		if (verbose)
		{
			cout << " node " << to_string (current.second.first - 1);
		}
	}
	else if (verbose)
	{
		cout << " leaf 0";
	}
	if (verbose)
	{
		cout << " " << current.second.second;
	}
	cout << endl;
}

const function<bool(string)> CompleteCommand::determineFilterPredicate (const string originalInput, const Key originalRoot, const Key root)
{
	if (root == originalRoot)
	{
		return [](string) { return true; };
	}

	const bool namespaceCompletion = originalRoot.getFullName ().empty ();
	const string fullName = namespaceCompletion ? originalInput : originalRoot.getFullName ();
	return [=](string test) {
		// For cascading keys, we ignore the preceding namespace for filtering
		const int cascadationOffset = (!namespaceCompletion && root.isCascading () ? test.find ("/") : 0);
		return fullName.size () <= test.size () && equal (fullName.begin (), fullName.end (), test.begin () + cascadationOffset);
	};
}

CompleteCommand::~CompleteCommand ()
{
}
