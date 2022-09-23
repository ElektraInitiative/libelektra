/**
 * @file
 *
 * @brief Implementation of kdb basename command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <basename.h>
#include <command.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "basename"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addBasenameSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Get the basename of a key.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execBasename (KeySet * options, Key * errorKey)
{
	GET_BASIC_OPTIONS
	int ret = 0;
	const char * name = getKeyNameFromOptions (GET_OPTION (options, "name"), errorKey, verbose);
	Key * key = keyNew (name, KEY_END);

	if (name == NULL)
	{
		ret = 11;
		goto cleanup;
	}

	if (key == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'%s' is not a valid key name.", name);
		ret = 11;
		goto cleanup;
	}
	CLI_PRINT (CLI_LOG_NONE, "%s", BOLD (keyBaseName (key)));

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) name);
	keyDel (tmp);
	keyDel (key);

	RETURN (ret)
}
