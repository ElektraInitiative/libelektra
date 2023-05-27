/**
 * @file
 *
 * @brief Implementation of kdb plugin-list command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <plugin-list.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "plugin-list"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addPluginListSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "List plugins available to Elektra.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/provider", KEY_META, "description",
				   "Only list plugins that provide this functionality.", KEY_META, "opt/long", "provider", KEY_META,
				   "opt/arg", "required", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppPluginList (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
