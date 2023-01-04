/**
 * @file
 *
 * @brief Implementation of kdb dirname command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <dirname.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "dirname"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addDirnameSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Get the dir name of a key.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execDirname (KeySet * options, Key * errorKey)
{
	GET_BASIC_OPTIONS

	const char * name = getKeyNameFromOptions (options, GET_OPTION (options, "name"), errorKey, verbose);
	if (name == NULL) return 1;

	Key * key = keyNew (name, KEY_END);
	if (key == NULL)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'%s' is not a valid key name.", name);
		elektraFree ((void *) name);
		return 1;
	}

	name = keyName (key);
	int dirnameLen = keyGetNameSize (key);
	while (name[--dirnameLen] != '/')
		;
	while (*name != '/')
	{
		name++;
		dirnameLen--;
	}
	if (!dirnameLen)
	{
		dirnameLen = 1;
	}
	CLI_PRINT (CLI_LOG_NONE, "%*.*s", dirnameLen, dirnameLen, BOLD (name));

	elektraFree ((void *) name);
	keyDel (key);
	return 0;
}
