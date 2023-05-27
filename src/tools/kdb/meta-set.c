/**
 * @file
 *
 * @brief Implementation of kdb meta-set command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta-set.h>

#include <command.h>

#include <cpp-main.h>

#define COMMAND_NAME "meta-set"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMetaSetSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Set a metavalue.", KEY_META, "command",
				   "meta-set", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Force setting the value", KEY_META,
				   "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/keyname", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/metaname", KEY_META, "description", "The meta name", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/metavalue", KEY_META, "description", "The value that should be set",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppMetaSet (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
