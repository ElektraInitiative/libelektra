/**
 * @file
 *
 * @brief Implementation of kdb cache command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cache.h>
#include <command.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "cache"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addCacheSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Enable, disable, clear the cache or revert to default.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/setting", KEY_META, "description",
				   "enable, disable, default or clear the cache.", KEY_META, "args", "indexed", KEY_META, "args/index", "0",
				   KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppCache (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
