/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <superls.hpp>

#include <iostream>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <functional>

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

	Key key;
	Key next;
		
	bool isExistent = ks.size() != 0 && ks.lookup(root);
	cout << root.getFullName() << (isExistent ? " exists!" : " nope") << " with a keyset sized " << ks.size() << " and properties isValid " << root.isValid() << " isCascading " << root.isCascading() 
	<< " isSpec " << root.isSpec() << " isProc " << root.isProc() << " isDir " << root.isDir() << " isUser " << root.isUser() << " isSystem " << root.isSystem() << endl;
	
	function<bool(string)> pred = [] (string test) { return true; };
	
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
	
	cout << "testing pred" << (pred ("user/test/this/is/ok") ? "true" : "false") << endl;
	cout << "testing pred" << (pred ("user/toest/this/is/not/ok") ? "true" : "false") << endl;
	cout << root.getBaseName() << " " << root.getName() << " " << root.getFullName() << " " << (root.isNull() ? "isNull" : "isNotNull") << (root.isValid() ? "isValid" : "isNotValid") << endl;
	
	for (NameIterator it = root.begin(); it != root.end(); it++)
	{
		cout << *it << endl;
	}
	
	cout << "ks contains " << ks.size() << "elements" << endl;
	
	
	
	// super-ls /
	//system/foo node 1
	//system/foo/bla node 1
	//system/foo/bla/something node 4
	//system/foo/bla/something/a leaf
	//system/foo/bla/something/b leaf
	//system/foo/bla/something/c leaf
	//system/foo/bla/something/d leaf
	//system/bla node 1
	//system/bla/xx node 0
	
	// also consider completion from /system/foo/b 
	// > if the input does not end in /b, complete the parent and filter by /b
	
	// ok initial things:
	
	
	// #1 take /system/foo
	// #2 strip foo and take system 
	// #3 do the analyzation function on key /System
	// #4 
	
	// via map:
	// # parent = null processed = null
	// # system/foo -> nothing processed, no parent take system and check again
	// # nothing, system highest, system = parent, add +1 to map for system, system/foo = processed
	// # check system/foo/bla
	// # case 1: cur isDirectBelow processed -> processed++; parent = processed;
	// # case 2: cur isDirectBelow parent -> parent++;
	// # case 4: cur isBelow parent -> back to beginning
	// # case 3: !below parent != parent -> back to beginning for now, simple
	
	map<string, int> hierarchy = map<string, int>();
	
	ks.rewind();
	while ((key = ks.next())) {
		if (pred(key.getFullName())) {
			int count = hierarchy[key.getFullName()]; // will be 0 if non existent
			hierarchy[key.getFullName()] = ++count;
		}
	}
	
	for (auto it = hierarchy.begin(); it != hierarchy.end(); it++) {
		cout << it->first << " is " << it->second << endl;
	}
	
	// initial key
	// ignore things behind initial key for now
	// maxdepth
	// curDepth
	
	// ks.rewind();
	// while ((key = ks.next())) {
		// cursor_t currentCursor = ks.getCursor();
		// int count = 0;
		// while ((next = ks.next()) && next.isBelow(key)) {
			// if (next.isDirectBelow(key)) {
				// count++;
			// }
		// }
		// if (count == 0) {
			// cout << key << " leaf" << endl;
		// } else {
			// cout << key << " node " << count << endl;
		// }
		// Reset it so we do the same for the next node
		// ks.setCursor(currentCursor);
	// }

	printWarnings (cerr, root);

	return 0;
}

Key SuperLsCommand::getParentKey(Key key) {
	return Key (key.getFullName().erase(key.getFullName().size() - key.getBaseName().size()), KEY_END);
}

SuperLsCommand::~SuperLsCommand ()
{
}
