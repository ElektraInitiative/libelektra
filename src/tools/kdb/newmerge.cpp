/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <newmerge.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <modules.hpp>

#include <iostream>
#include <string>

/**
 * This file gets found
 * Else the compiler would throw an error
 */
#include <newmerge.h>

using namespace kdb;
// using namespace kdb::tools::merging;
using namespace std;

NewMergeCommand::NewMergeCommand ()
{
}

NewMergeCommand::~NewMergeCommand ()
{
}

int NewMergeCommand::execute (Cmdline const & cl)
{
	if (mymerge () == 3)
	{
		throw invalid_argument ("Correct call");
	}
	throw invalid_argument ("Wrong call");
}
