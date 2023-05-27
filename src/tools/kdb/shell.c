/**
 * @file
 *
 * @brief Implementation of kdb shell command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <shell.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "shell"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addShellSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Start a kdb shell instance.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/color", KEY_META, "description",
				   "Print never/auto(default)/always colored output.", KEY_META, "opt", "C", KEY_META, "opt/arg/help",
				   "WHEN", KEY_META, "opt/long", "color", KEY_META, "opt/arg", "required", KEY_END));
}

int execCppShell (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
