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
	}

	const char * keyName = getKeyNameFromOptions (options, GET_OPTION (options, "keyname"), errorKey, verbose);
	if (keyName == NULL) return 1;

	const char * metaName = GET_OPTION (options, "metaname");
	const char * metaValue = GET_OPTION (options, "metavalue");

	Key * parentKey = keyNew (keyName, KEY_END);

	if (keyGetNamespace (parentKey) == KEY_NS_NONE || keyGetNamespace (parentKey) == KEY_NS_CASCADING) {
		ret = 1;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "key does not specify a namespace");
		elektraFree ((void*) keyName);
		keyDel (parentKey);
		return ret;
	}

	keySetNamespace (parentKey, KEY_NS_CASCADING);
	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, parentKey) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", keyName, GET_ERR (parentKey));
		ret = 1;
		goto cleanup;
	}

	Key * key = ksLookup (conf, parentKey, KDB_O_NONE);
	if (key == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", keyName, GET_ERR (parentKey));
		ret = 1;
		goto cleanup;
	}
	keySetMeta (key, metaName, metaValue);

	if (kdbSet (handle, conf, parentKey) == -1)
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not set meta-value '%s' for '%s': %s", metaName, keyName, GET_ERR (parentKey));
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
	return ret;
}
