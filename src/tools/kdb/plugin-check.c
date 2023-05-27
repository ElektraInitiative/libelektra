/**
 * @file
 *
 * @brief Implementation of kdb plugin-check command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <plugin-check.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "plugin-check"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addPluginCheckSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Perform internal checks.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Also perform write tests.", KEY_META,
				   "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/plugin", KEY_META, "description",
				   "The plugin to perform the checks on, by default a check will be performed on the key database itself.",
				   KEY_META, "opt", "p", KEY_META, "opt/long", "plugin", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pluginconf", KEY_META, "description",
				   "Add a plugin configuration in addition to /module.", KEY_META, "opt", "c", KEY_META, "opt/long",
				   "plugin-config", KEY_META, "opt/arg", "required", KEY_END));
	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppPluginCheck (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
