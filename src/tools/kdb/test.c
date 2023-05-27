/**
 * @file
 *
 * @brief Implementation of kdb test command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <test.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "test"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addTestSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Run test(s) on the key database.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description", "Where to perform the tests.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/testname/#", KEY_META, "description",
				   "What tests should be run, by default all are run.", KEY_META, "args", "remaining", KEY_END));


	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppTest (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
