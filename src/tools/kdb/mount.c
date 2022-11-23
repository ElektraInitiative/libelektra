/**
* @file
*
* @brief Implementation of kdb mount command
*
* @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
*/

#include <mount.h>
#include <command.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>
#include <kdbmerge.h>
#include <kdbmount.h>

#define COMMAND_NAME "mount"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMountSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Mount a new backend.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.", KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));
	/* TODO: Better description text for -f */
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Force the action to be done.", KEY_META, "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/quiet", KEY_META, "description", "Only print error messages.", KEY_META, "opt", "q", KEY_META, "opt/long", "quiet", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/interactive", KEY_META, "description", "Ask the user interactively.", KEY_META, "opt", "i", KEY_META, "opt/long", "interactive", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/first", KEY_META, "description", "Suppress the first column.", KEY_META, "opt", "1", KEY_META, "opt/long", "first", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/second", KEY_META, "description", "Suppress the second column.", KEY_META, "opt", "2", KEY_META, "opt/long", "second", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/third", KEY_META, "description", "Suppress the third column.", KEY_META, "opt", "3", KEY_META, "opt/long", "third", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/with-recommends", KEY_META, "description", "Add recommended plugins.", KEY_META, "opt", "W", KEY_META, "opt/long", "with-recommends", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/strategy", KEY_META, "description", "Specify the strategy to resolve conflicts.", KEY_META, "opt", "s", KEY_META, "opt/arg/help", "STRATEGY", KEY_META, "opt/long", "strategy", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/resolver", KEY_META, "description", "Specify the resolver plugin to use.", KEY_META, "opt", "R", KEY_META, "opt/arg/help", "NAME", KEY_META, "opt/long", "resolver", KEY_META, "opt/arg", "required", KEY_END));

	/* Arguments of command */
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description", "A filename (absolute for system, relative for cascading or user).", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mountpoint", KEY_META, "description", "Where to mount the backend, start with / for cascading mountpoints.", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END));
	/* TODO: Enable support for using multiple plugins */
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/plugin", KEY_META, "description", "A list of plugins and their config to mount at that place.", KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));

	/* TODO: Enable support of arguments for the given plugins. */
	//ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/config", KEY_META, "description", "Each plugin my be followed by a (,-sep) list of keys and corresponding values.", KEY_META, "args", "indexed", KEY_META, "args/index", "3", KEY_END));


	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}
/* TODO: Implement better error-handling */
int execMount (KeySet * options, Key * errorKey)
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
	bool optForce = false;
	bool optQuiet = false;
	bool optInteractive = false;
	bool optUseBinaryNullTermination = false;
	bool optSuppressFirstColumn = false;
	bool optSuppressSecondColumn = false;
	bool optSuppressThirdColumn = false;
	/* TODO: Decide where to put code for evaluation of option (currently also in cmerge.c) */
	const char * optStrategy = NULL;
	/* TODO: Implement option */
	const char * optResolver = NULL;
	int mergeStrategy = MERGE_STRATEGY_ABORT;

	/* 3. Command arguments ("[path mountpoint] [plugin [config] [..]]" */
	/* current simplified parameters: "kdb mount [options] [path mountpoint] [plugin]" */
	const char * argPath = NULL;
	const char * argMountpoint = NULL;
	/* TODO: Support multiple plugins and arguments to plugins */
	const char * argPlugin = NULL;
	/* "With no arguments and not in interactive mode, the current mountpoints will be listed
	 * Then the options -012 take effect (otherwise these options can be used to suppress warnings)
	 * 1 and 2 will suppress the output of the respective column." (taken from legacy cpp-version of the mount-command) */
	bool listMode = false;


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
	if ((tmp = GET_OPTION_KEY (options, "force")))
		elektraKeyToBoolean (tmp, &optForce);
	if ((tmp = GET_OPTION_KEY (options, "quiet")))
		elektraKeyToBoolean (tmp, &optQuiet);
	if ((tmp = GET_OPTION_KEY (options, "interactive")))
		elektraKeyToBoolean (tmp, &optInteractive);
	if ((tmp = GET_OPTION_KEY (options, "nullterm")))
		elektraKeyToBoolean (tmp, &optUseBinaryNullTermination);
	if ((tmp = GET_OPTION_KEY (options, "first")))
		elektraKeyToBoolean (tmp, &optSuppressFirstColumn);
	if ((tmp = GET_OPTION_KEY (options, "second")))
		elektraKeyToBoolean (tmp, &optSuppressSecondColumn);
	if ((tmp = GET_OPTION_KEY (options, "third")))
		elektraKeyToBoolean (tmp, &optSuppressThirdColumn);

	/* TODO: Remove code duplication (code for processing strategy-parameter is also present in cmerge.c */
	if ((tmp = GET_OPTION_KEY (options, "strategy")))
	{
		elektraKeyToString (tmp, &optStrategy);
		if (elektraStrCmp (optStrategy, "our") == 0)
		{
			mergeStrategy = MERGE_STRATEGY_OUR;
		}
		else if (elektraStrCmp (optStrategy, "their") == 0)
		{
			mergeStrategy = MERGE_STRATEGY_THEIR;
		}
		else if (elektraStrCmp (optStrategy, "abort") != 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'%s' is not a valid strategy.", optStrategy);
			return 1;
		}
	}

	/* Command arguments */
	if ((argPath = getKeyNameFromOptions (options, GET_OPTION(options, "path"), errorKey, optVerbose)) == NULL)
	{
		// no path specified, just list the current mountpoints
		listMode = true;
	}
	if ((argMountpoint = getKeyNameFromOptions (options, GET_OPTION(options, "mountpoint"), errorKey, optVerbose)) == NULL)
	{
		elektraFree ((void *) argPath);
		return 1;
	}
	if ((argPlugin = getKeyNameFromOptions (options, GET_OPTION(options, "plugin"), errorKey, optVerbose)) == NULL)
	{
		elektraFree ((void *) argMountpoint);
		elektraFree ((void *) argPath);
		return 1;
	}


	/* Actual business logic of the command */
	/* TODO: Refactor the reduce number of arguments! */
	cReadMountConf (optUseBinaryNullTermination, optSuppressFirstColumn, optSuppressSecondColumn, optSuppressThirdColumn, optVerbose, optDebug);

	KDB * const kdbHandle = kdbOpen (0, errorKey);
	KeySet * mountConf = ksNew (0, KS_END);
	Key * const parentKey = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);
	if (!kdbHandle || !mountConf || !parentKey)
	{
		elektraFree ((void *) argMountpoint);
		elektraFree ((void *) argPath);
		elektraFree ((void *) argPlugin);

		if (parentKey)
			keyDel (parentKey);
		if (mountConf)
			ksDel (mountConf);
		if (kdbHandle)
			kdbClose (kdbHandle, errorKey);

		return 1;
	}

	/* TODO: Add error handling (see file mountbase.cpp) */
	kdbSet (kdbHandle, mountConf, parentKey);

	if(!optInteractive && listMode)
	{
		/* List mode and no interactive mode, so let's output the mounting table */
		cOutputMtab (mountConf, optSuppressFirstColumn, optSuppressSecondColumn, optUseBinaryNullTermination);
	}
	else
	{
		/* TODO: Check 2nd argument (numArgs) */
		cProcessArguments (optInteractive, (int) ksGetSize(options));
		cGetMountpoint (mountConf, optInteractive);
		/* TODO: give full pugins config */
		cBuildBackend (mountConf, argMountpoint, optForce, mergeStrategy, optInteractive, argPlugin);

		/* TODO: Not yet implemented function calls in CPP:
		 * askForConfirmation (cl);
		 * doIt ();
		 */
	}

	/* cleanup */
	elektraFree ((void *) argMountpoint);
	elektraFree ((void *) argPath);
	elektraFree ((void *) argPlugin);

	return 0;
}
