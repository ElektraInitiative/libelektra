/**
 * @file
 *
 * @brief Implementation of kdb gen command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <gen.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "gen"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addGenSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Elektra's code-generator.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/template", KEY_META, "description", "The template to use.", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/parent", KEY_META, "description", "The parent key to use.", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/output", KEY_META, "description", "The base name of the output files.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/parameters/#", KEY_META, "description",
				   "List of parameters that is supported on the template.", KEY_META, "args", "remaining", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/inputfile", KEY_META, "description",
				   "Load the file <file> with plugin <plugin> instead of accessing the KDB.", KEY_META, "opt", "F",
				   KEY_META, "opt/arg/help", "<plugin>=<file>", KEY_META, "opt/long", "input-file", KEY_META, "opt/arg",
				   "required", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppGen (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
