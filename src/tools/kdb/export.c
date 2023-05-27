/**
 * @file
 *
 * @brief Implementation of kdb export command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <export.h>

#include <kdb.h>

#include "kdbhelper.h"
#include <cpp-main.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "export"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addExportSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Export keys from the key database.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));

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

int execCppExport (int argc, char** argv)
{
	return cpp_main (argc, argv);
}
