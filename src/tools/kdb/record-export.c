/**
 * @file
 *
 * @brief Implementation of kdb record-export command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <record-export.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "record-export"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addRecordExportSpec (KeySet * spec)
{
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
			     "Exports the recorded changes into the specified output format.", KEY_META, "command", COMMAND_NAME, KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/source", KEY_META, "description", "The source key", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/format", KEY_META, "description", "The format that should be used.",
				   KEY_META, "opt", "f", KEY_META, "opt/arg/help", "FORMAT", KEY_META, "opt/long", "format", KEY_META,
				   "opt/arg", "required", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/noelektra", KEY_META, "description", "Omit the system:/elektra directory.",
			     KEY_META, "opt", "E", KEY_META, "opt/long", "without-elektra", KEY_META, "opt/arg", "none", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppRecordExport (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}
