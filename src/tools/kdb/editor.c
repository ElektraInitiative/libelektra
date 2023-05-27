/**
 * @file
 *
 * @brief Implementation of kdb editor command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <editor.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "editor"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addEditorSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Use your editor for editing KDB.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/strategy", KEY_META, "description",
			     "Strategy to use in case of a conflict. Options: ours,theirs,abort(default)", KEY_META, "opt", "s", KEY_META,
			     "opt/arg/help", "STRATEGY", KEY_META, "opt/long", "strategy", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description", "Path to where to edit keys.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/format", KEY_META, "description",
				   "The format in which the keys should be edited.", KEY_META, "opt", "f", KEY_META, "opt/arg/help",
				   "FORMAT", KEY_META, "opt/long", "format", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/editor", KEY_META, "description", "The editor that should eb used.",
				   KEY_META, "opt", "e", KEY_META, "opt/arg/help", "EDITOR", KEY_META, "opt/long", "editor", KEY_META,
				   "opt/arg", "required", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppEditor (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
