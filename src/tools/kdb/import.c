/**
 * @file
 *
 * @brief Implementation of kdb import command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <import.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "import"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addImportSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Import an existing configuration into the key database.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/format", KEY_META, "description", "The format that should be used.",
				   KEY_META, "opt", "f", KEY_META, "opt/arg/help", "FORMAT", KEY_META, "opt/long", "format", KEY_META,
				   "opt/arg", "required", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pluginsconfig", KEY_META, "description",
				   "Add a configuration to the format plugin.", KEY_META, "opt", "c", KEY_META, "opt/long",
				   "plugins-config", KEY_META, "opt/arg", "required", KEY_END));

	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/strategy", KEY_META, "description",
			     "Strategy to use in case of a conflict. Options: ours,theirs,abort(default)", KEY_META, "opt", "s", KEY_META,
			     "opt/arg/help", "STRATEGY", KEY_META, "opt/long", "strategy", KEY_META, "opt/arg", "required", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/destination", KEY_META, "description",
				   "Where the user wants the keys should be imported into.", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppImport (int argc, char** argv)
{

	return cpp_main (argc, argv);
}
