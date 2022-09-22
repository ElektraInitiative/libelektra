/**
 * @file
 *
 * @brief Implementation of kdb ls command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <ls.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "ls"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addLsSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description",
				   "List keys or keynames below a given name. To also get the value use export.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mindepth", KEY_META, "description", "Specify the minimum depth.",
				   KEY_META, "opt", "m", KEY_META, "opt/long", "min-depth", KEY_META, "opt/arg", "required", KEY_META,
				   "opt/arg/help", "MIN", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/maxdepth", KEY_META, "description", "Specify the maximum depth.",
				   KEY_META, "opt", "M", KEY_META, "opt/long", "max-depth", KEY_META, "opt/arg", "required", KEY_META,
				   "opt/arg/help", "MAX", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execLs (KeySet * options, Key * errorKey)
{
	GET_BASIC_OPTIONS

	tmp = GET_OPTION_KEY (options, "mindepth");
	kdb_long_t minDepth = 0;
	if (tmp != NULL)
	{
		elektraKeyToLong (tmp, &minDepth);
		keyDel (tmp);
	}

	tmp = GET_OPTION_KEY (options, "maxdepth");
	kdb_long_t maxDepth = -1;
	if (tmp != NULL)
	{
		elektraKeyToLong (tmp, &maxDepth);
		keyDel (tmp);
	}

	if (minDepth < 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "the minimum depth has to be a positive number");
		RETURN (2)
	}
	if (maxDepth != -1 && maxDepth < 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "the maximum depth has to be a positive number");
		RETURN (2)
	}
	if (maxDepth != -1 && maxDepth <= minDepth)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "the maximum depth has to be larger than the minimum depth");
		RETURN (2)
	}

	const char * name = getKeyNameFromOptions (GET_OPTION (options, "name"), errorKey, verbose);
	if (name == NULL)
	{
		RETURN (2)
	}

	Key * whereToLook = keyNew (name, KEY_END);
	int rootDepth = getKeyNameDepth (name);

	KeySet * searchIn = ksNew (0, KS_END);

	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, searchIn, whereToLook) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (whereToLook));
		elektraFree ((void *) name);
		keyDel (whereToLook);
		ksDel (searchIn);
		kdbClose (handle, errorKey);
		RETURN (5)
	}
	keyCopyAllMeta (errorKey, whereToLook);
	kdbClose (handle, errorKey);

	CLI_PRINT (CLI_LOG_VERBOSE, "size of all keys in mount point: %zd%c", ksGetSize (searchIn), nullTerm ? '\0' : '\n');
	KeySet * part = ksCut (searchIn, whereToLook);
	keyDel (whereToLook);
	CLI_PRINT (CLI_LOG_VERBOSE, "size of requested keys: %zd%c", ksGetSize (part), nullTerm ? '\0' : '\n');

	Key * cur = NULL;
	bool first = true;
	for (elektraCursor it = 0; it < ksGetSize (part); ++it)
	{
		cur = ksAtCursor (part, it);
		int currentDepth = getKeyNameDepth (keyName (cur));
		if ((maxDepth == -1 || currentDepth < rootDepth + maxDepth) && currentDepth >= rootDepth + minDepth)
		{
			CLI_PRINT (CLI_LOG_NONE, "%s%s", first ? "" : (nullTerm ? "\0" : "\n"), keyName (cur));
			first = false;
		}
	}

	if (!noNewLine)
	{
		printf ("%c", nullTerm ? '\0' : '\n');
	}

	elektraFree ((void *) name);
	keyDel (tmp);
	ksDel (part);
	ksDel (searchIn);

	RETURN (0)
}

int getKeyNameDepth (const char * name)
{
	size_t nameLen = strlen (name);
	int slashCount = 0;

	for (size_t i = 0; i < nameLen; i++)
	{
		slashCount += name[i] == '/' && (i == 0 || name[i - 1] != '\\');
	}

	return slashCount;
}
