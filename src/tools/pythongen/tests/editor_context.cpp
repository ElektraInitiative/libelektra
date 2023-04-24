/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./editor_context.hpp"
#include <kdb.hpp>

#include <iostream>


int main ()
{
	using namespace kdb;

	KDB kdb;
	KeySet ks;
	Context c;
	// some predefined values (for convenience):
	ks.append (Key ("user:/kate/quit", KEY_VALUE, "Ctrl+k", KEY_END));
	KeySet ks2;
	kdb.get (ks2, "/myapp");
	// overwrite them if something is available in config files:
	ks.append (ks2);

	Parameters par (ks, c);

	std::cout << par.myapp.shortcut.quitMyapp << std::endl;

	return 0;
}
