/**
 * @file
 *
 * @brief Implementation of kdb complete command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <complete.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "complete"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addCompleteSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Show suggestions how to complete a given path.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mindepth", KEY_META, "description", "Specify the minimum depth.",
				   KEY_META, "opt", "m", KEY_META, "opt/long", "min-depth", KEY_META, "opt/arg", "required", KEY_META,
				   "opt/arg/help", "MIN", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/maxdepth", KEY_META, "description", "Specify the maximum depth.",
				   KEY_META, "opt", "M", KEY_META, "opt/long", "max-depth", KEY_META, "opt/arg", "required", KEY_META,
				   "opt/arg/help", "MAX", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description", "The path that should be completed.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppComplete (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
