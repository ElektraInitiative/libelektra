/**
 * @file
 *
 * @brief Implementation of kdb rm command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <rm.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "rm"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addRmSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Remove a key.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/recursive", KEY_META, "description", "Work in recursive mode.",
				   KEY_META, "opt", "r", KEY_META, "opt/long", "recursive", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Do not fail on missing key.",
				   KEY_META, "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The key name", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execRm (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	bool recursive = false;
	tmp = GET_OPTION_KEY (options, "recursive");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "recursive"), &recursive);
		keyDel (tmp);
	}

	bool force = false;
	tmp = GET_OPTION_KEY (options, "force");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "force"), &force);
		keyDel (tmp);
	}

	const char * name = getKeyNameFromOptions (GET_OPTION (options, "name"), errorKey, verbose);
	if (name == NULL)
	{
		RETURN (2)
	}

	Key * key = keyNew (name, KEY_END);

	if (keyGetNamespace (key) == KEY_NS_NONE || keyGetNamespace (key) == KEY_NS_CASCADING)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "key does not specify a namespace");
		elektraFree ((void *) name);
		keyDel (key);
		RETURN (2)
	}

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, conf, key) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (key));
		ret = 5;
		goto cleanup;
	}

	Key * t = ksLookup (conf, key, KDB_O_NONE);
	if (!force && !recursive && t == NULL)
	{
		CLI_ERROR_PRINT (CLI_LOG_NONE, "Did not find key '%s'", RED (name));
		ret = 11;
		goto cleanup;
	}
	keyDel (t);

	keySetString (key, NULL);
	if (!recursive)
	{
		ksLookup (conf, key, KDB_O_POP);
	}
	else
	{
		ksDel (ksCut (conf, key));
	}

	if (kdbSet (handle, conf, key) == -1)
	{
		ret = 5;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not save keyset after moving: %s", GET_ERR (key));
	}


cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) name);
	keyDel (key);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
