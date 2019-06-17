/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <ls.hpp>

#include <climits>
#include <iostream>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>

using namespace kdb;
using namespace std;

LsCommand::LsCommand () : kdb (root)
{
}

int LsCommand::execute (Cmdline const & cl)
{
	checkArguments (cl);

	printWarnings (cerr, root, cl.verbose, cl.debug);

	root = cl.createKey (0);

	kdb.get (ks, root);

	if (cl.verbose) cout << "size of all keys in mount point: " << ks.size () << endl;

	KeySet part (ks.cut (root));

	if (cl.verbose) cout << "size of requested keys: " << part.size () << endl;
	cout.setf (std::ios_base::unitbuf);
	if (cl.null)
	{
		cout.unsetf (std::ios_base::skipws);
	}

	printResults (part, getDepth (root), cl);

	printWarnings (cerr, root, cl.verbose, cl.debug);

	return 0;
}

void LsCommand::checkArguments (Cmdline const & cl)
{
	if (cl.arguments.size () != 1)
	{
		throw invalid_argument ("1 argument required");
	}
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
}

void LsCommand::printResults (KeySet const & part, const int rootDepth, Cmdline const & cl)
{
	const int offset = root.getBaseName ().empty () || shallShowNextLevel (cl.arguments[0]) ? 1 : 0;
	const int relativeMinDepth = rootDepth + cl.minDepth + offset;
	const int relativeMaxDepth =
		std::max (cl.maxDepth, rootDepth > INT_MAX - cl.maxDepth - offset ? INT_MAX : rootDepth + cl.maxDepth + offset);
	if (cl.debug)
	{
		cout << "The root depth is " << rootDepth << ", the relative minimum depth is " << relativeMinDepth
		     << " and the relative maximum depth is " << relativeMaxDepth << endl;
	}

	for (const auto & it : part)
	{
		const int depth = getDepth (it);
		if ((depth >= relativeMinDepth && depth < relativeMaxDepth) || cl.debug)
		{
			cout << it;
			if (cl.debug)
			{
				cout << " " << depth;
			}

			if (cl.null)
			{
				cout << '\0' << std::flush;
			}
			else
			{
				cout << endl;
			}
		}
	}
}

int LsCommand::getDepth (Key const & key)
{
	return std::distance (key.begin (), key.end ());
}

bool LsCommand::shallShowNextLevel (const string argument)
{
	auto it = argument.rbegin ();
	// If the argument ends in / its an indicator to complete the next level (like done by shells), but not if its escaped
	return it != argument.rend () && (*it) == '/' && ((++it) == argument.rend () || (*it) != '\\');
}

LsCommand::~LsCommand ()
{
}
