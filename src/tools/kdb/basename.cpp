/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <basename.hpp>
#include <nameparthelper.hpp>
#include <cmdline.hpp>


using namespace std;

BasenameCommand::BasenameCommand ()
{
}

int BasenameCommand::execute (Cmdline const & cl)
{
	return executeNamepartcommand (cl, keyBasename);
}

BasenameCommand::~BasenameCommand ()
{
}
