/**
 * @file
 *
 * @brief Implementation of kdb record-start command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbrecord.h>
#include <record-start.h>
#include <stdio.h>
#include <string.h>

#include <kdb.h>

#define COMMAND_NAME "record-start"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addRecordStartSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Start session recording.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/parent", KEY_META, "description",
				   "Restrict recording to a specific subtree of the KDB.", KEY_META, "opt", "P", KEY_META, "opt/arg/help",
				   "KEY", KEY_META, "opt/long", "parent", KEY_META, "opt/arg", "required", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execRecordStart (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS
	Key * parentKey;
	if (GET_OPTION_KEY (options, "parent") == NULL)
	{
		parentKey = keyNew ("/", KEY_END);
	}
	else
	{
		parentKey = getKeyFromOptions (GET_OPTION (options, "parent"), errorKey, verbose);
		if (parentKey == NULL)
		{
			RETURN (2)
		}
	}
	KDB * handle = kdbOpen (NULL, errorKey);

	if (!elektraRecordEnableRecording (handle, parentKey, errorKey))
	{
		ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "could not enable recording: %s", GET_ERR_DESC (errorKey));
		ret = 11;
	}

	if (!noNewLine)
	{
		printf ("\n");
	}
	keyDel (parentKey);
	kdbClose (handle, errorKey);
	RETURN (ret)
}
