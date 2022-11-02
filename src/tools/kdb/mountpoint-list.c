/**
 * @file
 *
 * @brief Implementation of kdb mountpoint list command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <mountpoint-list.h>
#include <kdb.h>
#include <kdbease.h>
#include <kdbmount.h>
#include <string.h>

#define COMMAND_NAME "mountpoint/list"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMountpointListSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "List all mountpoints.", KEY_META, "command",
				   "list", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supfirst", KEY_META, "description", "Suppress the first column.",
				   KEY_META, "opt", "1", KEY_META, "opt/long", "first", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supsecond", KEY_META, "description", "Suppress the second column.",
				   KEY_META, "opt", "2", KEY_META, "opt/long", "second", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supthird", KEY_META, "description", "Suppress the third column.",
				   KEY_META, "opt", "3", KEY_META, "opt/long", "third", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.",
				   KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME));
}

int execMountpointList (KeySet * options, Key * errorKey)
{
	/* Options that can be set via command line arguments */
	/* 1. Standard options */
	/* TODO: Discuss the evaluation of global option (not specific to command) */
	bool optDebug = false;
	bool optVerbose = false;
	bool optVersion = false;
	bool optSuppressNewline = false;
	/* TODO: Implement option */
	const char * optProfile = NULL;
	/* TODO: Implement option */
	const char * optColor = NULL;

	/* 2. Command-specific options */
	bool optUseBinaryNullTermination = false;
	bool optSuppressFirstColumn = false;
	bool optSuppressSecondColumn = false;
	bool optSuppressThirdColumn = false;


	const Key * tmp;
	if ((tmp = GET_OPTION_KEY (options, "debug")))
		elektraKeyToBoolean (tmp, &optDebug);
	if ((tmp = GET_OPTION_KEY (options, "verbose")))
		elektraKeyToBoolean (tmp, &optVerbose);
	if ((tmp = GET_OPTION_KEY (options, "version")))
		elektraKeyToBoolean (tmp, &optVersion);
	if ((tmp = GET_OPTION_KEY (options, "nonewline")))
		elektraKeyToBoolean (tmp, &optSuppressNewline);

	/* command-specific options */
	if ((tmp = GET_OPTION_KEY (options, "nullterm")))
		elektraKeyToBoolean (tmp, &optUseBinaryNullTermination);
	if ((tmp = GET_OPTION_KEY (options, "first")))
		elektraKeyToBoolean (tmp, &optSuppressFirstColumn);
	if ((tmp = GET_OPTION_KEY (options, "second")))
		elektraKeyToBoolean (tmp, &optSuppressSecondColumn);
	if ((tmp = GET_OPTION_KEY (options, "third")))
		elektraKeyToBoolean (tmp, &optSuppressThirdColumn);


	KDB * const kdbHandle = kdbOpen (0, errorKey);
	if(kdbHandle)
	{
		const KeySet * const mountConf = getMountConfig (kdbHandle, errorKey, NULL);
		if (mountConf)
		{
			cOutputMtab (mountConf, optSuppressFirstColumn, optSuppressSecondColumn, optUseBinaryNullTermination);
			kdbClose (kdbHandle, errorKey);
			return 0;
		}
		else
		{
			kdbClose (kdbHandle, errorKey);
			return 1;
		}
	}
	else
	{
		return 1;
	}
}
