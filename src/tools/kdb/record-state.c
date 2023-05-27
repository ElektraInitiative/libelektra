/**
 * @file
 *
 * @brief Implementation of kdb rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <record-state.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "record-state"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addRecordStateSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Print out information about the current state of the recording session.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppRecordState (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
