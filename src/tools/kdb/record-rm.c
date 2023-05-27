/**
 * @file
 *
 * @brief Implementation of kdb rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <record-rm.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "record-rm"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addRecordRmSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Removes key from the recording session.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/recursive", KEY_META, "description", "Work in recursive mode.",
				   KEY_META, "opt", "r", KEY_META, "opt/long", "recursive", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The key name", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppRecordRm (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
