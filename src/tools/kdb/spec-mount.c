/**
 * @file
 *
 * @brief Implementation of kdb spec-mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <spec-mount.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "spec-mount"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addSpecMountSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Mount a spec file to the key database.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mountpoint", KEY_META, "description",
				   "where to mount the backend, start with / for cascading mp", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "0", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/plugins/#", KEY_META, "description",
				   "Plugin and its config, <PLUGIN>[key1=val1,key2=val2,...]", KEY_META, "args", "remaining", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/resolver", KEY_META, "description",
				   "Specify the resolver plugin to use.", KEY_META, "opt", "R", KEY_META, "opt/arg/help", "NAME", KEY_META,
				   "opt/long", "resolver", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/addrecommended", KEY_META, "description", "Add recommended plugins.",
				   KEY_META, "opt", "W", KEY_META, "opt/long", "with-recommends", KEY_META, "opt/arg", "none", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pluginsconf", KEY_META, "description",
				   "Add a plugin configuration for all plugins.", KEY_META, "opt", "c", KEY_META, "opt/long",
				   "plugins-config", KEY_META, "opt/arg/help", "CONFIG", KEY_META, "opt/arg", "required", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppSpecMount (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
