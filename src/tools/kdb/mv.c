/**
 * @file
 *
 * @brief Implementation of kdb mv command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <mv.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "mv"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMvSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Move a configuration within the key database.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/recursive", KEY_META, "description", "Work in recursive mode.",
				   KEY_META, "opt", "r", KEY_META, "opt/long", "recursive", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description",
				   "Overwrite even if target key already has a value.", KEY_META, "opt", "f", KEY_META, "opt/long", "force",
				   KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/source", KEY_META, "description", "The source key", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/destination", KEY_META, "description", "The destination key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "1", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execMv (KeySet * options, Key * errorKey)
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

	const char * sourceName = getKeyNameFromOptions (GET_OPTION (options, "source"), errorKey, verbose);
	if (sourceName == NULL)
	{
		RETURN (2)
	}

	const char * destName = getKeyNameFromOptions (GET_OPTION (options, "destination"), errorKey, verbose);
	if (destName == NULL)
	{
		elektraFree ((void *) sourceName);
		RETURN (2)
	}

	CLI_PRINT (CLI_LOG_DEBUG, "%s", "create keys... ");

	Key * sourceKey = keyNew (sourceName, KEY_END);
	Key * destKey = keyNew (destName, KEY_END);

	if (keyGetNamespace (sourceKey) == KEY_NS_NONE || keyGetNamespace (sourceKey) == KEY_NS_CASCADING)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "source key does not specify a namespace");
		elektraFree ((void *) sourceName);
		elektraFree ((void *) destName);
		keyDel (sourceKey);
		keyDel (destKey);
		RETURN (2)
	}
	if (keyGetNamespace (destKey) == KEY_NS_NONE || keyGetNamespace (destKey) == KEY_NS_CASCADING)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "destination key does not specify a namespace");
		elektraFree ((void *) sourceName);
		elektraFree ((void *) destName);
		keyDel (sourceKey);
		keyDel (destKey);
		RETURN (2)
	}

	CLI_PRINT (CLI_LOG_DEBUG, "%s", "ok\n");

	Key * root = keyNew ("/", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, conf, root) == -1)
	{ // could not load from kdb
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", sourceName, GET_ERR (root));
		ret = 5;
		goto cleanup;
	}

	Key * key = ksLookup (conf, sourceKey, KDB_O_NONE);
	if (!recursive && key == NULL)
	{ // if not recursive, it's not important that the source key itself exists
		CLI_ERROR_PRINT (CLI_LOG_NONE, "Did not find key '%s'", RED (sourceName));
		ret = 11;
		goto cleanup;
	}
	keyDel (key);

	Key * cur = NULL;

	size_t sourceNameLen = elektraStrLen (keyName (sourceKey));
	size_t destNameLen = elektraStrLen (keyName (destKey));

	CLI_PRINT (CLI_LOG_DEBUG, "source: %s [%ld]\n", keyName (sourceKey), sourceNameLen);
	CLI_PRINT (CLI_LOG_DEBUG, "dest: %s [%ld]\n", keyName (destKey), destNameLen);
	CLI_PRINT (CLI_LOG_DEBUG, "recursive: %d\n", recursive);

	long count = 0;
	KeySet * newConf = ksNew (ksGetSize (conf), KS_END);
	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		cur = ksAtCursor (conf, it);

		bool startsWithSrc = !elektraStrNCmp (keyName (cur), keyName (sourceKey), sourceNameLen - 1);
		bool equalsSrc = !elektraStrCmp (keyName (cur), keyName (sourceKey));
		CLI_PRINT (CLI_LOG_DEBUG, "checking if '%s' has to be moved\n", BOLD (keyName (cur)));
		CLI_PRINT (CLI_LOG_DEBUG, " starts-with: %d; ", startsWithSrc);
		CLI_PRINT (CLI_LOG_DEBUG, " equals: %d\n", equalsSrc);

		// starts-with if recursive, or equals if !recursive
		if ((recursive && startsWithSrc) || equalsSrc)
		{
			size_t newNameLen = destNameLen + (equalsSrc ? 0 : 1 + elektraStrLen (keyName (cur)) - sourceNameLen);
			char * newName = elektraMalloc (newNameLen);
			strcpy (newName, keyName (destKey));
			if (!equalsSrc)
			{
				strcat (newName, "/");
				strcat (newName, &keyName (cur)[sourceNameLen]);
			}
			Key * targetKey = ksLookupByName (conf, newName, KDB_O_NONE);
			if (!force && targetKey != NULL && elektraStrCmp (keyString (cur), keyString (targetKey)) != 0)
			{
				CLI_ERROR_PRINT (CLI_LOG_NONE, "Destination key '%s' already has value '%s', won't overwrite!", newName,
						 keyString (targetKey));
				ret = 12;
				elektraFree (newName);
				keyDel (targetKey);
				ksDel (newConf);
				goto cleanup;
			}
			keyDel (targetKey);

			CLI_PRINT (CLI_LOG_VERBOSE, "-> moving '%s' to '%s'\n", BOLD (keyName (cur)), BOLD (newName));
			Key * tmpKey = keyDup (cur, KEY_CP_ALL);
			keySetName (tmpKey, newName);
			ksAppendKey (newConf, tmpKey);
			elektraFree (newName);
			count++;
		}
		else
		{ // keep every key that is not, or not under the source key
			ksAppendKey (newConf, cur);
		}
	}

	if (kdbSet (handle, newConf, root) == -1)
	{ // writing changes back to kdb failed
		ret = 5;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "could not save keyset after moving: %s", GET_ERR (root));
	}
	ksDel (newConf);

	CLI_PRINT (CLI_LOG_VERBOSE, "\nmoved %ld keys", count);


cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) sourceName);
	elektraFree ((void *) destName);
	keyDel (root);
	keyDel (destKey);
	keyDel (sourceKey);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
