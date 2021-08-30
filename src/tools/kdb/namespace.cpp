/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <namespace.hpp>
#include <nameparthelper.hpp>
#include <cmdline.hpp>


using namespace std;

NamespaceCommand::NamespaceCommand ()
{
}

int NamespaceCommand::execute (Cmdline const & cl)
{
	return executeNamepartcommand (cl, keyNamespace);
}

NamespaceCommand::~NamespaceCommand ()
{
}
