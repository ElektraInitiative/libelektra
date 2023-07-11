/**
 * @file
 *
 * @brief Implementation of kdb meta-ls command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta-ls.h>

#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "meta/ls"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMetaLsSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "List available metadata names for a key.",
				   KEY_META, "command", "ls", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/keyname", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMetaLs (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	Key * toLookUp = getKeyFromOptions (GET_OPTION (options, "keyname"), errorKey, verbose);
	if (toLookUp == NULL)
	{
		RETURN (2)
	}

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, toLookUp) == -1)
	{
		ELEKTRA_SET_CLI_ERRORF (errorKey, "could not load '%s': %s", keyName (toLookUp), GET_ERR (toLookUp));
		ret = 5;
		goto cleanup;
	}

	Key * key = ksLookup (conf, toLookUp, KDB_O_NONE);

	if (key == NULL)
	{
		CLI_ERROR_PRINT (CLI_LOG_NONE, "Did not find key '%s'", RED (keyName (toLookUp)));
		ret = 11;
		goto cleanup;
	}

	KeySet * metaKeys = keyMeta (key);

	Key * cur = NULL;
	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		cur = ksAtCursor (metaKeys, it);
		CLI_PRINT (CLI_LOG_NONE, "%s%c", keyName (cur) + sizeof ("meta:/") - 1, nullTerm ? '\0' : '\n');
	}
	keyDel (key);

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	keyDel (toLookUp);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
