/**
 * @file
 *
 * @brief Implementation of kdb meta-rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta-rm.h>

#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define COMMAND_NAME "meta/rm"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMetaRmSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Remove a metakey.", KEY_META, "command", "rm",
				   KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/keyname", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/metaname", KEY_META, "description", "The meta name", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "1", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMetaRm (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	const char * keyName = getKeyNameFromOptions (GET_OPTION (options, "keyname"), errorKey, verbose);
	if (keyName == NULL)
	{
		RETURN (2)
	}

	const char * metaName = GET_OPTION (options, "metaname");

	Key * parentKey = keyNew (keyName, KEY_END);

	if (keyGetNamespace (parentKey) == KEY_NS_NONE || keyGetNamespace (parentKey) == KEY_NS_CASCADING)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "key does not specify a namespace");
		elektraFree ((void *) keyName);
		keyDel (parentKey);
		RETURN (2)
	}

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, parentKey) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", keyName, GET_ERR (parentKey));
		ret = 5;
		goto cleanup;
	}

	Key * key = ksLookup (conf, parentKey, KDB_O_NONE);
	if (key == NULL)
	{
		CLI_ERROR_PRINT (CLI_LOG_NONE, "Did not find key '%s'", RED (keyName));
		ret = 11;
		goto cleanup;
	}

	if (keyGetMeta (key, metaName) == NULL)
	{
		CLI_ERROR_PRINT (CLI_LOG_NONE, "No meta key with name '%s' present", RED (metaName));
		keyDel (key);
		ret = 12;
		goto cleanup;
	}

	if (keySetMeta (key, metaName, NULL) != 0 || kdbSet (handle, conf, parentKey) == -1)
	{
		ret = 5;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not remove meta-key '%s' for '%s': %s", metaName, keyName,
							GET_ERR (parentKey));
	}

	keyDel (key);

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void*) keyName);
	keyDel (parentKey);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
