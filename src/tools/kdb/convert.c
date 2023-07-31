/**
 * @file
 *
 * @brief Implementation of kdb convert command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <convert.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "convert"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addConvertSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Convert configuration files using elektra.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/importfmt", KEY_META, "description",
				   "Format that the current configuration file is using.", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "0", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/exportfmt", KEY_META, "description",
			     "Format that it should be converted to.", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/importfile", KEY_META, "description",
				   "Full path to the current configuration file.", KEY_META, "args", "indexed", KEY_META, "args/index", "2",
				   KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/exportfile", KEY_META, "description",
				   "Full path to where the converted configuration file should be saved.", KEY_META, "args", "indexed",
				   KEY_META, "args/index", "3", KEY_END));
	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppConvert (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
