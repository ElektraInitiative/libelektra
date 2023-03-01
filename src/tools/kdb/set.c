/**
 * @file
 *
 * @brief KDB set subcommand
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <colors.h>
#include <command.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <set.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "set"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addSetSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Set the value of an individual key.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Force setting the value", KEY_META,
				   "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/value", KEY_META, "description", "The value that should be set",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execSet (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	bool force = false;
	tmp = GET_OPTION_KEY (options, "force");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "force"), &force);
	}

	Key * parentKey = getKeyFromOptions (GET_OPTION (options, "name"), errorKey, verbose);
	if (parentKey == NULL)
	{
		RETURN (2)
	}

	const char * value = GET_OPTION (options, "value");

	if (keyGetNamespace (parentKey) == KEY_NS_NONE || keyGetNamespace (parentKey) == KEY_NS_CASCADING)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERROR (errorKey, "key does not specify a namespace");
		keyDel (parentKey);
		RETURN (2)
	}

	Key * maybeCascadingParent = keyDup (parentKey, KEY_CP_NAME);
	if (!force)
	{
		keySetNamespace (maybeCascadingParent, KEY_NS_CASCADING);
	}
	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, maybeCascadingParent) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", keyName (parentKey),
							GET_ERR (maybeCascadingParent));
		ret = 5;
		goto cleanup;
	}
	keyCopyAllMeta (errorKey, parentKey);

	Key * key = ksLookup (conf, parentKey, KDB_O_NONE);
	if (key == NULL)
	{
		key = keyNew (keyName (parentKey), KEY_END);
		ksAppendKey (conf, key);
		CLI_PRINT (CLI_LOG_NONE, "Create a new key %s with string \"%s\"", keyName (key), value);
	}
	else
	{
		CLI_PRINT (CLI_LOG_NONE, "Set string to \"%s\"", value);
	}
	keySetString (key, value); // can't fail, since neither value or key can be null

	if (kdbSet (handle, conf, parentKey) == -1)
	{
		ret = 5;
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not set value for '%s': %s", keyName (parentKey),
							GET_ERR (parentKey));
	}
	keyCopyAllMeta (errorKey, parentKey);

	keyDel (key);

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	keyDel (parentKey);
	keyDel (maybeCascadingParent);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
