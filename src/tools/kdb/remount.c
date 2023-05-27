/**
 * @file
 *
 * @brief Implementation of kdb remount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <remount.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "remount"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addRemountSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Remount an existing backend with a different filename.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/filename", KEY_META, "description", "The new filename.", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description", "The new path.", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mountpoint", KEY_META, "description",
			     "The mountpoint that should be remounted", KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));


	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppRemount (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
