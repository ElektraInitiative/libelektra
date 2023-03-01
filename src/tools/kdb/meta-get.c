/**
 * @file
 *
 * @brief Implementation of kdb meta-get command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta-get.h>

#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "meta/get"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMetaGetSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Get specific metadata from a key.", KEY_META,
				   "command", "get", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/keyname", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/metaname", KEY_META, "description", "The meta name", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "1", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMetaGet (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	Key * toLookUp = getKeyFromOptions (GET_OPTION (options, "keyname"), errorKey, verbose);
	if (toLookUp == NULL)
	{
		RETURN (2)
	}

	const char * metaName = GET_OPTION (options, "metaname");

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	Key * cascadingParent = keyDup (toLookUp, KEY_CP_NAME);
	keySetNamespace (cascadingParent, KEY_NS_CASCADING);
	if (kdbGet (handle, conf, cascadingParent) == -1)
	{
		ELEKTRA_SET_CLI_ERRORF (errorKey, "could not load '%s': %s", keyName (toLookUp), GET_ERR (cascadingParent));
		ret = 5;
		goto cleanup;
	}

	Key * key = ksLookup (conf, toLookUp, KDB_O_NONE);
	if (key == NULL)
	{
		ELEKTRA_SET_CLI_ERRORF (errorKey, "Key '%s' does not exist", keyName (toLookUp));
		ret = 11;
		goto cleanup;
	}
	const Key * metaKey = keyGetMeta (key, metaName);
	if (metaKey == NULL)
	{
		ELEKTRA_SET_CLI_ERRORF (errorKey, "Key '%s' does not contain a metakey with the name '%s'", keyName (toLookUp), metaName);
		ret = 12;
	}
	else
	{
		CLI_PRINT (CLI_LOG_NONE, "%s%s", keyString (metaKey), noNewLine ? "" : "\n");
	}

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	keyDel (cascadingParent);
	keyDel (key);
	keyDel (toLookUp);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
