/**
 * @file
 *
 * @brief Implementation of kdb meta-show command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta-show.h>

#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "meta/show"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMetaShowSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Print all metakeys with their value for a key.", KEY_META, "command", "show", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/keyname", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMetaShow (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	const char * name = getKeyNameFromOptions (GET_OPTION (options, "keyname"), errorKey, verbose);
	if (name == NULL)
	{
		RETURN (2)
	}

	Key * toLookUp = keyNew (name, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, toLookUp) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (toLookUp));
		ret = 5;
		goto cleanup;
	}

	Key * key = ksLookup (conf, toLookUp, KDB_O_NONE);

	if (key == NULL)
	{
		CLI_ERROR_PRINT (CLI_LOG_NONE, "Did not find key '%s'", RED (name));
		ret = 11;
		goto cleanup;
	}

	KeySet * metaKeys = keyMeta (key);

	Key * cur = NULL;
	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		cur = ksAtCursor (metaKeys, it);
		CLI_PRINT (CLI_LOG_NONE, "%s: %s%c", keyName (cur) + sizeof ("meta:/") - 1, BOLD (keyString (cur)), nullTerm ? '\0' : '\n');
	}
	keyDel (key);

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) name);
	keyDel (toLookUp);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
