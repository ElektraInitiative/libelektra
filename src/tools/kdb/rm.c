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
	}

	const char * name = getKeyNameFromOptions (options, GET_OPTION (options, "name"), errorKey, verbose);
	if (name == NULL) return 1;

	Key * key = keyNew (name, KEY_END);

	if (keyGetNamespace (key) == KEY_NS_NONE || keyGetNamespace (key) == KEY_NS_CASCADING)
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "source key does not specify a namespace");
		elektraFree ((void *) name);
		keyDel (key);
		return ret;
	}

	Key * root = keyNew ("/", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, conf, root) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (root));
		ret = 1;
		goto cleanup;
	}

	Key * cur = NULL;

	size_t keyNameLen = elektraStrLen (keyName (key));

	long count = 0;
	KeySet * newConf = ksNew (ksGetSize (conf), KS_END);
	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		cur = ksAtCursor (conf, it);

		bool startsWithSrc = !elektraStrNCmp (keyName (cur), keyName (key), keyNameLen - 1);
		bool equalsSrc = !elektraStrCmp (keyName (cur), keyName (key));

		// starts-with if recursive, or equals if !recursive
		if ((recursive && startsWithSrc) || equalsSrc)
		{
			count++;
			CLI_PRINT (CLI_LOG_VERBOSE, "removing '%s\n", BOLD (keyName (cur)));
			continue;
		}
		ksAppendKey (newConf, cur);
	}

	if (kdbSet (handle, newConf, root) == -1)
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not save keyset after moving: %s", GET_ERR (root));
	}
	CLI_PRINT (CLI_LOG_VERBOSE, "\nremoved %ld keys", count);


cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) name);
	keyDel (root);
	elektraFree ((void *) fmtBuffer);
	ksDel (conf);
	kdbClose (handle, errorKey);
	return ret;
}
