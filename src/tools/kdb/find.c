/**
 * @file
 *
 * @brief Implementation of kdb find command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <find.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "find"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addFindSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Find keys that match a regex pattern.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pattern", KEY_META, "description", "The patter that should be matched",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppFind (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
