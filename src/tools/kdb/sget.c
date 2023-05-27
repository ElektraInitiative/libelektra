/**
* @file
*
* @brief Implementation of kdb sget command
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include <command.h>
#include <sget.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "sget"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addSgetSpec (KeySet * spec)
{
       ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Get the value of a key stored in the key database from a script.", KEY_META,
				  "command", COMMAND_NAME, KEY_END));
       ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The keyname.", KEY_META,
				  "args", "indexed", KEY_META, "args/index", "0", KEY_END));
       ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/default", KEY_META, "description", "The default value that will be should be printed.", KEY_META,
				  "args", "indexed", KEY_META, "args/index", "1", KEY_END));

       ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppSget (int argc, char** argv)
{
       return cpp_main (argc, argv);
}

