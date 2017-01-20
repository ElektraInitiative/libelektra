/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <superls.hpp>

#include <iostream>
#include <stack>
#include <functional>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace std;

SuperLsCommand::SuperLsCommand () : kdb (root)
{
}

int SuperLsCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1)
	{
		throw invalid_argument ("1 argument required");
	}

	printWarnings (cerr, root);
	Key originalRoot = cl.createKey (0);
	root = originalRoot;
	kdb.get (ks, root);
	
	cout.setf (std::ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (std::ios_base::skipws);
	}
	
	function<bool(string)> pred = [] (string) { return true; };
	bool isExistent = ks.lookup(root) || root.getBaseName().empty();
	if (!isExistent) {
		string fullName = cl.arguments[0];
		string newKeyName = getParentKey(root).getFullName();
		cout << "new name is " << newKeyName << endl;
		root = Key (newKeyName, KEY_END);
		cout << " building predicate " << root.isCascading() << " " << fullName << " " << fullName.find("/") << endl;
		pred = [=] (string test) { 
			return fullName.size() <= test.size() && std::equal(fullName.begin(), fullName.end(), test.begin() + (root.isCascading() ? test.find("/") : 0));
		};
	}
	
	cout << "cutting at " << root << endl;
	ks = ks.cut(root);
	
	map<Key, pair<int, int>> hierarchy = map<Key, pair<int, int>>();
	
	ks.rewind();
	
	KeySet hierarchialKeys;
	Key parent;
	Key last;
	Key tmp;
	
	stack<Key> keyStack;
	if (!(tmp = ks.next())) {
		// return?
		return 0;
	}
	
	int curDepth = 0;
	keyStack.push(tmp);
	while (!keyStack.empty()) {
		Key current = keyStack.top();
		keyStack.pop();
		if (current.isDirectBelow(last)) { // hierarchy change
			parent = last;
			curDepth++;
		}
		
		cout << "Processing " << current << " vs last " << last << " at depth " << curDepth << endl;
		
		if (current.isDirectBelow(parent)) { // hierarchy continues
			increaseCount(hierarchy, parent, [](int p) {return p;});
			increaseCount(hierarchy, current, [=](int) {return curDepth;});
		} else { // otherwise expand the current key to the stack
			cout << "expanding at " << current << " vs root " << root << " which is belowOrSame " << current.isBelowOrSame(root) << endl;
			while (current.isBelow(root) && !current.getBaseName().empty() && hierarchy[current].first == 0) {
				cout << "expanding at " << current << " vs root " << root << endl;
				keyStack.push(current);
				hierarchialKeys.append(current);
				current = getParentKey(current);
			}
			parent = getParentKey(current);
			curDepth = hierarchy[current].second;
		}
		
		// Current hierarchy processed, we can resume with the next
		if(keyStack.empty() && (tmp = ks.next())){
			keyStack.push(tmp);
		}
		
		last = current;
	}
	
	// Adjust the output offset, in case the given string exists in the hierarchy but not in the original ks
	int offset = !isExistent && hierarchialKeys.lookup(originalRoot);
	int minDepth = 0 + offset;
	int maxDepth = cl.recursive ? std::numeric_limits<int>::max() : (1 + offset);
	cout << "max is " << maxDepth << " and min is " << minDepth << endl;
	
	cout << "dumping all results" << endl;
	for (auto it = hierarchy.begin(); it != hierarchy.end(); it++) {
		cout << it->first << " " << it->second.first << " " << it->second.second << endl;
	}
	
	cout << endl << "dumping filtered results" << endl;
	for (auto it = hierarchy.begin(); it != hierarchy.end(); it++) {
		if (pred(it->first.getFullName()) && it->second.second > minDepth && it->second.second <= maxDepth) {
			cout << it->first << (it->second.first > 1 ? " node " + std::to_string(it->second.first - 1): " leaf") << endl;
		}
	}
	
	printWarnings (cerr, root);

	return 0;
}

Key SuperLsCommand::getParentKey(Key key) {
	return Key (key.getFullName().erase(key.getFullName().size() - key.getBaseName().size()), KEY_END);
}

void SuperLsCommand::increaseCount(map<Key, pair<int, int>> & hierarchy, Key key, function<int(int)> depthIncreaser) {
	cout << "increasing for " << key << endl;
	pair<int, int> prev = hierarchy[key];
	hierarchy[key] = pair<int, int>(prev.first + 1, depthIncreaser(prev.second));
}

SuperLsCommand::~SuperLsCommand ()
{
}
