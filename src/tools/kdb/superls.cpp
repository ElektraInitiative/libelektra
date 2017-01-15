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

	if (cl.verbose) cout << "size of all keys in mountpoint: " << ks.size () << endl;

	KeySet part (ks.cut (root));
	
	if (cl.verbose) cout << "size of requested keys: " << part.size () << endl;
	cout.setf (std::ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (std::ios_base::skipws);
	}

	Key key;
	Key next;
	
	part.rewind();
	while ((key = part.next())) {
		cursor_t currentCursor = part.getCursor();
		int count = 0;
		while ((next = part.next()) && next.isBelow(key)) {
			if (next.isDirectBelow(key)) {
				count++;
			}
		}
		if (count == 0) {
			cout << key << " leaf" << endl;
		} else {
			cout << key << " node " << count << endl;
		}
		// Reset it so we do the same for the next node
		part.setCursor(currentCursor);
	}

	printWarnings (cerr, root);

	return 0;
}

SuperLsCommand::~SuperLsCommand ()
{
}
