/**
 * @file
 *
 * @brief Implementation of kdb cp command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <cp.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "cp"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addCpSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Copy a configuration within the key database.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/recursive", KEY_META, "description", "Work in recursive mode.",
				   KEY_META, "opt", "r", KEY_META, "opt/long", "recursive", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description",
				   "Overwrite even if target key already has a value.", KEY_META, "opt", "f", KEY_META, "opt/long", "force",
				   KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/source", KEY_META, "description", "The source key", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/destination", KEY_META, "description", "The destination key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "1", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppCp (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
