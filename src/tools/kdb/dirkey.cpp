/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cmdline.hpp>
#include <dirkey.hpp>
#include <nameparthelper.hpp>


using namespace std;

DirkeyCommand::DirkeyCommand ()
{
}

int DirkeyCommand::execute (Cmdline const & cl)
{
	return executeNamepartcommand (cl, keyDirkey);
}

DirkeyCommand::~DirkeyCommand ()
{
}
