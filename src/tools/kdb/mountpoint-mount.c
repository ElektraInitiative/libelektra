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
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.",
				   KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supfirst", KEY_META, "description", "Suppress the first column.",
				   KEY_META, "opt", "1", KEY_META, "opt/long", "first", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supsecond", KEY_META, "description", "Suppress the second column.",
				   KEY_META, "opt", "2", KEY_META, "opt/long", "second", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/supthird", KEY_META, "description", "Suppress the third column.",
				   KEY_META, "opt", "3", KEY_META, "opt/long", "third", KEY_META, "opt/arg", "none", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMount (KeySet * options, Key * errorKey)
{
	int ret = 0;
	bool verbose = false;
	Key * tmp = GET_OPTION_KEY (options, "verbose");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "verbose"), &verbose);
	}

	const char * path = GET_OPTION (options, "path");

	const char * mountpoint = getKeyNameFromOptions (options, GET_OPTION (options, "mountpoint"), errorKey, verbose);
	if (mountpoint == NULL) return 1;

	Key * arrayParent = GET_OPTION_KEY (options, "plugins");
	KeySet * plugins = elektraArrayGet (arrayParent, options);

	ksRewind (plugins);
	Key * cur = NULL;
	while ((cur = ksNext (plugins)) != NULL)
	{
		printf ("PLUGIN ->  %s\n", keyString (cur));
	}
	printf ("\n");

	KDB * handle = kdbOpen (NULL, errorKey);

	KeySet * currentMountConfig = getMountConfig (handle, errorKey);

	Key * mountpointKey = keyNew (mountpoint, KEY_END);

	printKsNames (currentMountConfig);

	printf ("MOUNT");
}
