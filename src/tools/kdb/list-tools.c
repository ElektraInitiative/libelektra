/**
 * @file
 *
 * @brief Implementation of kdb list-tools command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <list-tools.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "list-tools"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addListToolsSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "List all external tools available to Elektra.", KEY_META, "command", COMMAND_NAME, KEY_END));
}

int execCppListTools (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
