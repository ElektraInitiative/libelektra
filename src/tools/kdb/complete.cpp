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

	const bool hasArgument = cl.arguments.size () > 0;
	const string argument = hasArgument ? cl.arguments[cl.arguments.size () - 1] : "";
	complete (argument, cl);

	return 0;
}

void CompleteCommand::complete (const string argument, const Cmdline & cl)
{
	using namespace std::placeholders; // for bind

	if (argument.empty ())
	{ // No argument, show all completions by analyzing everything including namespaces, so adjust the offset for that
		const Key root = Key ("/", KEY_END);
		printResults (root, cl.minDepth, cl.maxDepth, cl, analyze (getKeys (root, false), cl),
			      bind (filterDepth, cl.minDepth, cl.maxDepth, _1), printResult);
	}
	else if (!argument.empty () && argument[0] == '+')
	{ // is it a bookmark?
		// Try to resolve the bookmark
		const Key resolvedBookmark = cl.resolveBookmark (argument);
		if (resolvedBookmark.isValid ())
		{
			complete (resolvedBookmark.getFullName (), cl);
		}
		else
		{ // Bookmark not resolvable, so try a bookmark completion
			// since for legacy reasons its probably /sw/kdb, we use /sw as a root
			const Key root = Key ("/sw", KEY_END);
			printResults (root, 0, cl.maxDepth, cl, analyze (getKeys (root, true), cl), bind (filterBookmarks, argument, _1),
				      printBookmarkResult);
		}
	}
	else
	{
		const Key parsedArgument (argument, KEY_END);
		if ((!parsedArgument.isValid () || !shallShowNextLevel (argument)) && parsedArgument.getBaseName ().empty ())
		{ // is it a namespace completion?
			const Key root = Key ("/", KEY_END);
			const auto filter = [&](const pair<Key, pair<int, int>> & c) {
				return filterDepth (cl.minDepth, cl.maxDepth, c) && filterName (argument, c);
			};
			printResults (root, cl.minDepth, cl.maxDepth, cl, analyze (getKeys (root, false), cl), filter, printResult);
		}
		else
		{ // the "normal" completion cases
			completeNormal (argument, parsedArgument, cl);
		}
	}
}

void CompleteCommand::completeNormal (const string argument, const Key parsedArgument, const Cmdline & cl)
{
	Key root = parsedArgument;
	const Key parent = getParentKey (root);
	// Its important to use the parent element here as using non-existent elements may yield no keys
	KeySet ks = getKeys (parent, false);
	// the namespaces count as existent although not found by lookup
	const bool isValidNamespace = parsedArgument.isValid () && parsedArgument.getBaseName ().empty ();
	const bool rootExists = isValidNamespace || ks.lookup (root);
	if (!rootExists)
	{
		root = parent;
	}
	const auto result = analyze (ks, cl);

	// we see depth relative to the completion level, if the root exists, distance will be higher so subtract 1
	// to add up for the offset added my shallShowNextLevel
	const int offset = distance (root.begin (), root.end ()) - rootExists + shallShowNextLevel (argument);

	const auto nameFilter = root.isCascading () ? filterCascading : filterName;
	const auto filter = [&](const pair<Key, pair<int, int>> & c) {
		return filterDepth (cl.minDepth + offset, max (cl.maxDepth, cl.maxDepth + offset), c) && nameFilter (argument, c);
	};
	printResults (root, cl.minDepth, cl.maxDepth, cl, result, filter, printResult);
}

bool CompleteCommand::shallShowNextLevel (const string argument)
{
	auto it = argument.rbegin ();
	// If the argument ends in / its an indicator to complete the next level (like done by shells), but not if its escaped
	return it != argument.rend () && (*it) == '/' && ((++it) == argument.rend () || (*it) != '\\');
}

KeySet CompleteCommand::getKeys (Key root, const bool cutAtRoot)
{
	KeySet ks;
	KDB kdb;
	kdb.get (ks, root);
	addMountpoints (ks, root);
	if (cutAtRoot)
	{
		ks = ks.cut (root);
	}
	return ks;
}

void CompleteCommand::printResults (const Key root, const int minDepth, const int maxDepth, const Cmdline & cl,
				    const map<Key, pair<int, int>> & result,
				    const std::function<bool(const pair<Key, pair<int, int>> & current)> filter,
				    const std::function<void(const pair<Key, pair<int, int>> & current, const bool verbose)> printResult)
{
	if (cl.verbose)
	{
		cout << "Showing results for a minimum depth of " << minDepth;
		if (maxDepth != numeric_limits<int>::max ())
		{
			cout << " and a maximum depth of " << maxDepth;
		}
		else
		{
			cout << " and no maximum depth";
		}
		cout << endl;
	}

	for (const auto & it : result)
	{
		if (cl.debug || filter (it))
		{
			printResult (it, cl.verbose);
		}
	}

	if (cl.debug || cl.verbose)
	{ // Only print this in debug mode to avoid destroying autocompletions because of warnings
		printWarnings (cerr, root);
	}
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

/*
 * McCabe complexity of 11, 4 caused by debug switches so its ok
 */
void CompleteCommand::addNamespaces (map<Key, pair<int, int>> & hierarchy, const Cmdline & cl)
{
	const string namespaces[] = {
		"spec/", "proc/", "dir/", "user/", "system/",
	};

	// Check for new namespaces, issue a warning in case
	if (cl.debug || cl.verbose)
	{
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
	}

	for (const string ns : namespaces)
	{
		const Key nsKey (ns, KEY_END);
		if ((cl.debug || cl.verbose) && ckdb::keyGetNamespace (nsKey.getKey ()) == KEY_NS_EMPTY)
		{ // Check for outdated namespaces, issue a warning in case
			cerr << "Outdated namespace detected:" << ns << ".\nPlease report this issue." << endl;
		}
		hierarchy[nsKey] = pair<int, int> (1, 0);
	}
}

/*
 * McCabe complexity of 12, 3 caused by debug switches so its ok
 */
const map<Key, pair<int, int>> CompleteCommand::analyze (const KeySet & ks, const Cmdline & cl)
{
	map<Key, pair<int, int>> hierarchy;
	stack<Key> keyStack;
	Key parent;
	Key last;
	addNamespaces (hierarchy, cl);

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
			while (!current.getBaseName ().empty () && hierarchy[current].first == 0)
			{ // Go back up in the hierarchy until we encounter a known key or are back at the namespace level
				if (cl.debug)
				{
					cout << "Expanding " << current << endl;
				}
				keyStack.push (current);
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

bool CompleteCommand::filterDepth (const int minDepth, const int maxDepth, const pair<Key, pair<int, int>> & current)
{
	return current.second.second >= minDepth && current.second.second < maxDepth;
}

bool CompleteCommand::filterCascading (const string argument, const pair<Key, pair<int, int>> & current)
{
	// For a cascading key completion, ignore the preceding namespace
	const string test = current.first.getFullName ();
	const int cascadationOffset = test.find ("/");
	return argument.size () <= test.size () && equal (argument.begin (), argument.end (), test.begin () + cascadationOffset);
}

bool CompleteCommand::filterName (const string argument, const pair<Key, pair<int, int>> & current)
{
	// For a namespace completion, compare by substring
	const string test = current.first.getFullName ();
	return argument.size () <= test.size () && equal (argument.begin (), argument.end (), test.begin ());
}

/**
 * McCabe Complexity of 15 due to the boolean conjunctions, easy to understand so its ok
 */
bool CompleteCommand::filterBookmarks (const string bookmarkName, const pair<Key, pair<int, int>> & current)
{
	// For a bookmark completion, ignore everything except the bookmarks by comparing the base name
	const string test = current.first.getBaseName ();
	// as we search in /sw due to legacy reasons, ensure we have an actual bookmark by checking the path
	bool elektraFound = false;
	bool kdbFound = false;
	bool bookmarksFound = false;
	bool bookmarkFound = false;

	for (const string part : current.first)
	{
		// size 1 -> +, so show all bookmarks, order of these checks is important!
		bookmarkFound = bookmarksFound && (bookmarkFound || bookmarkName.size () == 1 ||
						   (bookmarkName.size () - 1 <= part.size () &&
						    equal (bookmarkName.begin () + 1, bookmarkName.end (), part.begin ())));
		bookmarksFound = bookmarksFound || (part == "bookmarks" && !bookmarkFound);
		kdbFound = kdbFound || (!bookmarksFound && part == "kdb");
		elektraFound = elektraFound || (!kdbFound && part == "elektra");
	}

	return (elektraFound || kdbFound) && bookmarksFound && bookmarkFound;
}

void CompleteCommand::printBookmarkResult (const pair<Key, pair<int, int>> & current, const bool verbose)
{ // Ignore the path for a bookmark completion
	cout << "+" << current.first.getBaseName ();
	if (current.second.first > 1)
	{
		cout << "/";
	}
	if (verbose)
	{
		cout << (current.second.first > 1 ? " node " : " leaf ");
		cout << (current.second.first - 1) << " " << current.second.second;
	}
	cout << endl;
}

void CompleteCommand::printResult (const pair<Key, pair<int, int>> & current, const bool verbose)
{
	cout << current.first.getFullName ();
	if (current.second.first > 1)
	{
		cout << "/";
	}
	if (verbose)
	{
		cout << (current.second.first > 1 ? " node " : " leaf ");
		cout << (current.second.first - 1) << " " << current.second.second;
	}
	cout << endl;
}

CompleteCommand::~CompleteCommand ()
{
}
