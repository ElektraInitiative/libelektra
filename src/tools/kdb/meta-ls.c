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
#include <stdlib.h>
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

	bool nullTerm = false;
	tmp = GET_OPTION_KEY (options, "nullterm");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (tmp, &nullTerm);
	}

	const char * name = getKeyNameFromOptions (options, GET_OPTION (options, "keyname"), errorKey, verbose);
	if (name == NULL) return 1;

	Key * toLookUp = keyNew (name, KEY_END);
	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, toLookUp) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (toLookUp));
		ret = 1;
		goto cleanup;
	}

	Key * key = ksLookup (conf, toLookUp, KDB_O_NONE);
	KeySet * metaKeys = keyMeta (key);

	Key * cur = NULL;
	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		cur = ksAtCursor (metaKeys, it);
		CLI_PRINT (CLI_LOG_NONE, "%s%c", keyName (cur), nullTerm ? '\0' : '\n');
	}

cleanup:
	kdbClose (handle, errorKey);
	ksDel (conf);
	keyDel (toLookUp);
	elektraFree ((void *) name);
	return ret;
}
