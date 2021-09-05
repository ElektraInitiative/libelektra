/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cmdline.hpp>
#include <dirname.hpp>
#include <nameparthelper.hpp>


using namespace std;

DirnameCommand::DirnameCommand ()
{
}

int DirnameCommand::execute (Cmdline const & cl)
{
	return executeNamepartcommand (cl, keyDirname);
}

DirnameCommand::~DirnameCommand ()
{
}
