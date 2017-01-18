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
	root = cl.createKey (0);
	kdb.get (ks, root);
	
	cout.setf (std::ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (std::ios_base::skipws);
	}

	function<bool(string)> pred = [] (string test) { return true; };
	
	bool isExistent = ks.size() != 0 && ks.lookup(root);
	if (!isExistent) {
		string fullName = root.getFullName();
		string newKeyName = getParentKey(root).getFullName();
		cout << "new name is " << newKeyName << endl;
		root = Key (newKeyName, KEY_END);
		pred = [=] (string test) { 
			return fullName.size() <= test.size() && std::equal(fullName.begin(), fullName.end(), test.begin());
		};
	}
	
	ks = ks.cut (root);
	
	map<Key, int> hierarchy = map<Key, int>();
	
	ks.rewind();
	
	Key parent = root;
	Key last;
	Key tmp;
	
	std::stack<Key> keyStack;
	if (!(tmp = ks.next())) {
		// return?
		return 0;
	}
	
	// Simply just show either only the next level or all for now
	int maxDepth = cl.recursive ? std::numeric_limits<int>::max() : 2;
	int curDepth = 0;
	keyStack.push(tmp);
	while (!keyStack.empty()) {
		Key current = keyStack.top();
		cout << "Currently processing " << current << endl;
		keyStack.pop();
		if (current.isDirectBelow(last)) { // hierarchy change
				parent = last;
				curDepth++;
		}
		if (current.isDirectBelow(parent)) { // hierarchy continues
			if (curDepth < maxDepth + 1 && parent != root) {
				hierarchy[parent] = ++hierarchy[parent];
			}
			if (curDepth < maxDepth) {
				hierarchy[current] = ++hierarchy[current];
			}
			// Current hierarchy processed, we can resume with the next
			if(keyStack.empty() && (tmp = ks.next())){
				keyStack.push(tmp);
			}
		} else { // otherwise expand the current key to the stack and begin from root again
			while (current != root) {
				cout << "Currently expanding " << current << endl;
				keyStack.push(current);
				current = getParentKey(current);
			}
			parent = root;
			curDepth = 0;
		}
		last = current;
	}
	
	
	for (auto it = hierarchy.begin(); it != hierarchy.end(); it++) {
		if (pred(it->first.getFullName())) {
			cout << it->first << (it->second > 1 ? " node " + std::to_string(it->second - 1): " leaf") << endl;
		}
	}
	
	printWarnings (cerr, root);

	return 0;
}

Key SuperLsCommand::getParentKey(Key key) {
	return Key (key.getFullName().erase(key.getFullName().size() - key.getBaseName().size()), KEY_END);
}

SuperLsCommand::~SuperLsCommand ()
{
}
