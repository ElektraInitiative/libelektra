/**
 * @file
 *
 * @brief Implementation of kdb meta-set command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta-set.h>

#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define COMMAND_NAME "meta/set"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMetaSetSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Set a metavalue.", KEY_META, "command", "set",
				   KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Force setting the value", KEY_META,
				   "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/keyname", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/metaname", KEY_META, "description", "The meta name", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/metavalue", KEY_META, "description", "The value that should be set",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMetaSet (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	bool force = false;
	tmp = GET_OPTION_KEY (options, "force");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "force"), &force);
		keyDel (tmp);
	}

	const char * name = getKeyNameFromOptions (GET_OPTION (options, "keyname"), errorKey, verbose);
	if (name == NULL)
	{
		RETURN (2)
	}

	const char * metaName = GET_OPTION (options, "metaname");
	const char * metaValue = GET_OPTION (options, "metavalue");

	Key * parentKey = keyNew (name, KEY_END);

	if (keyGetNamespace (parentKey) == KEY_NS_NONE || keyGetNamespace (parentKey) == KEY_NS_CASCADING)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "key does not specify a namespace");
		elektraFree ((void *) name);
		keyDel (parentKey);
		RETURN (2)
	}

	Key * maybeCascadingParent = keyDup (parentKey, KEY_CP_NAME);
	if (!force)
	{
		keySetNamespace (maybeCascadingParent, KEY_NS_CASCADING);
	}
	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, maybeCascadingParent) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (maybeCascadingParent));
		ret = 5;
		goto cleanup;
	}

	Key * key = ksLookup (conf, parentKey, KDB_O_NONE);
	if (key == NULL)
	{
		key = keyNew (name, KEY_END);
		keySetMeta (key, metaName, metaValue);
		ksAppendKey (conf, key);
		keyDel (key);
		CLI_PRINT (CLI_LOG_VERBOSE, "Creating key %s\n", keyName (key));
	}
	keySetMeta (key, metaName, metaValue);

	if (kdbSet (handle, conf, parentKey) == -1)
	{
		ret = 5;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not set meta-value '%s' for '%s': %s", metaName, name,
							GET_ERR (parentKey));
	}


cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) name);
	keyDel (maybeCascadingParent);
	keyDel (parentKey);
	keyDel (tmp);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
