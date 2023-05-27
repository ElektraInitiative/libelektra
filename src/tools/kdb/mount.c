/**
 * @file
 *
 * @brief Implementation of kdb mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <mount.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "mount"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMountSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Mount a new backend.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));
	/* TODO: not supported by libelektra-opts right now
		ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description",
					   "A filename (absolute for system, relative for cascading or user)", KEY_META, "args", "indexed",
					   KEY_META, "args/index", "0", KEY_END));
		ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mountpoint", KEY_META, "description",
					   "where to mount the backend, start with / for cascading mp", KEY_META, "args", "indexed",
	   KEY_META, "args/index", "1", KEY_END));

		ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/plugins/#", KEY_META, "description",
					   "Plugin and its config, <PLUGIN>[key1=val1,key2=val2,...]", KEY_META, "args", "remaining",
	   KEY_END));
	*/
	// TODO: placeholder until the above is supported
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/args/#", KEY_META, "description",
			     "filename mountpoint plugins and config, see man page for details", KEY_META, "args", "remaining", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/strategy", KEY_META, "description",
				   "Specify the strategy to resolve conflicts.", KEY_META, "opt", "s", KEY_META, "opt/arg/help", "STRATEGY",
				   KEY_META, "opt/long", "strategy", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/resolver", KEY_META, "description",
				   "Specify the resolver plugin to use.", KEY_META, "opt", "R", KEY_META, "opt/arg/help", "NAME", KEY_META,
				   "opt/long", "resolver", KEY_META, "opt/arg", "required", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Force the action to be done.",
				   KEY_META, "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/addrecommended", KEY_META, "description", "Add recommended plugins.",
				   KEY_META, "opt", "W", KEY_META, "opt/long", "with-recommends", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.",
				   KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supfirst", KEY_META, "description", "Suppress the first column.",
				   KEY_META, "opt", "1", KEY_META, "opt/long", "first", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supsecond", KEY_META, "description", "Suppress the second column.",
				   KEY_META, "opt", "2", KEY_META, "opt/long", "second", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supthird", KEY_META, "description", "Suppress the third column.",
				   KEY_META, "opt", "3", KEY_META, "opt/long", "third", KEY_META, "opt/arg", "none", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pluginsconf", KEY_META, "description",
				   "Add a plugin configuration for all plugins.", KEY_META, "opt", "c", KEY_META, "opt/long",
				   "plugins-config", KEY_META, "opt/arg/help", "CONFIG", KEY_META, "opt/arg", "required", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppMount (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
