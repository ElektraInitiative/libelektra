/**
 * @file
 *
 * @brief Implementation of kdb mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cmerge.h>
#include <command.h>
#include <mountpoint-mount.h>
#include <mountpoint.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbmerge.h>
#include <kdbmount.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "mountpoint/mount"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMountSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Mount a new backend.", KEY_META, "command",
				   "mount", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/path", KEY_META, "description",
				   "A filename (absolute for system, relative for cascading or user)", KEY_META, "args", "indexed",
				   KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mountpoint", KEY_META, "description",
				   "where to mount the backend, start with / for cascading mp", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "1", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/plugins/#", KEY_META, "description",
				   "Plugin and its config, <PLUGIN>[key1=val1,key2=val2,...]", KEY_META, "args", "remaining", KEY_END));

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

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME));
}

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
	cReadMountConf (false, false, false, false, optVerbose, optDebug);

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

	/* TODO: Check 2nd argument (numArgs) */
	cProcessArguments (optInteractive, (int) ksGetSize(options));
	cGetMountpoint (mountConf, optInteractive);
	/* TODO: give full pugins config */
	cBuildBackend (mountConf, argMountpoint, optForce, mergeStrategy, optInteractive, argPlugin);

	/* TODO: Not yet implemented function calls in CPP:
	 * askForConfirmation (cl);
	 * doIt ();
	 */

	/* cleanup */
	elektraFree ((void *) argMountpoint);
	elektraFree ((void *) argPath);
	elektraFree ((void *) argPlugin);

	return 0;
}
