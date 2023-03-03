/**
 * @file
 *
 * @brief Implementation of kdb cp command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <cp.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "cp"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addCpSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "Copy a configuration within the key database.", KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/recursive", KEY_META, "description", "Work in recursive mode.",
				   KEY_META, "opt", "r", KEY_META, "opt/long", "recursive", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/source", KEY_META, "description", "The source key", KEY_META, "args",
				   "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/destination", KEY_META, "description", "The destination key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "1", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCp (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	bool recursive = false;
	tmp = GET_OPTION_KEY (options, "recursive");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "recursive"), &recursive);
	}

	const char * sourceName = getKeyNameFromOptions (options, GET_OPTION (options, "source"), errorKey, verbose);
	if (sourceName == NULL) return 1;

	const char * destName = getKeyNameFromOptions (options, GET_OPTION (options, "destination"), errorKey, verbose);
	if (destName == NULL)
	{
		elektraFree ((void *) sourceName);
		return 1;
	}

	Key * sourceKey = keyNew (sourceName, KEY_END);
	Key * destKey = keyNew (destName, KEY_END);

	if (keyGetNamespace (sourceKey) == KEY_NS_NONE || keyGetNamespace (sourceKey) == KEY_NS_CASCADING)
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "source key does not specify a namespace");
		elektraFree ((void *) sourceName);
		elektraFree ((void *) destName);
		keyDel (sourceKey);
		keyDel (destKey);
		return ret;
	}
	if (keyGetNamespace (destKey) == KEY_NS_NONE || keyGetNamespace (destKey) == KEY_NS_CASCADING)
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "destination key does not specify a namespace");
		elektraFree ((void *) sourceName);
		elektraFree ((void *) destName);
		keyDel (sourceKey);
		keyDel (destKey);
		return ret;
	}

	Key * root = keyNew ("/", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, conf, root) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", sourceName, GET_ERR (root));
		ret = 1;
		goto cleanup;
	}

	Key * cur = NULL;


	size_t sourceNameLen = elektraStrLen (keyName (sourceKey));
	size_t destNameLen = elektraStrLen (keyName (destKey));

	long count = 0;
	KeySet * newConf = ksNew (ksGetSize (conf), KS_END);
	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		cur = ksAtCursor (conf, it);

		bool startsWithSrc = !elektraStrNCmp (keyName (cur), keyName (sourceKey), sourceNameLen - 1);
		bool equalsSrc = !elektraStrCmp (keyName (cur), keyName (sourceKey));

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
			CLI_PRINT (CLI_LOG_VERBOSE, "-> moving '%s' to '%s'\n", BOLD (keyName (cur)), BOLD (newName));
			Key * tmpKey = keyDup (cur, KEY_CP_ALL);
			keySetName (tmpKey, newName);
			ksAppendKey (newConf, tmpKey);
			elektraFree (newName);
			count++;
		}
		ksAppendKey (newConf, cur);
	}

	if (kdbSet (handle, newConf, root) == -1)
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not save keyset after moving: %s", GET_ERR (root));
	}
	CLI_PRINT (CLI_LOG_VERBOSE, "\nmoved %ld keys", count);


cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) sourceName);
	keyDel (root);
	elektraFree ((void *) fmtBuffer);
	ksDel (conf);
	kdbClose (handle, errorKey);
	return ret;
}
