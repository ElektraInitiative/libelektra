/**
 * @file
 *
 * @brief Implementation of kdb plugin-info command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <plugin-info.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "plugin-info"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addPluginInfoSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Print information about an Elektra plugin.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/load", KEY_META, "description",
				   "Load plugin even if system:/elektra is available.", KEY_META, "opt", "l", KEY_META, "opt/long", "load",
				   KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pluginconf", KEY_META, "description", "Add a plugin configuration.",
				   KEY_META, "opt", "c", KEY_META, "opt/long", "plugins-config", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/clausename", KEY_META, "description",
				   "Just print information from a certain clause.", KEY_META, "opt/long", "clause-name", KEY_META,
				   "opt/arg", "required", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/plugin", KEY_META, "description", "The plugin about info should be printed.",
			     KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppPluginInfo (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
