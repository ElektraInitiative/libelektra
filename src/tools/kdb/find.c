/**
 * @file
 *
 * @brief Implementation of kdb find command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <find.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <regex-wrapper.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "find"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addFindSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Find keys that match a regex pattern.",
				   KEY_META, "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/pattern", KEY_META, "description", "The patter that should be matched",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execFind (KeySet * options, Key * errorKey)
{
	int ret = 0;
	GET_BASIC_OPTIONS

	const char * patternString = GET_OPTION (options, "pattern");
	int regex = 0;
	if (regcompWrapper (&regex, patternString))
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "could not compile regex pattern: '%s'", patternString);
		RETURN (11)
	}

	Key * root = keyNew ("/", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);

	if (kdbGet (handle, conf, root) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load key database: %s", GET_ERR (root));
		ret = 5;
		goto cleanup;
	}
	CLI_PRINT (CLI_LOG_DEBUG, "loaded %s\n", "/");

	Key * cur = NULL;

	int regexResult;
	int count = 0;

	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		cur = ksAtCursor (conf, it);
		regexResult = regexecWrapper (regex, keyName (cur));
		CLI_PRINT (CLI_LOG_DEBUG, "matching '%s' -> %d\n", keyName (cur), regexResult);

		if (!regexResult)
		{
			CLI_PRINT (CLI_LOG_NONE, "%s%c", keyName (cur), nullTerm ? '\0' : '\n');
			count++;
		}
		else if (regexResult != 1)
		{
			CLI_PRINT (CLI_LOG_DEBUG, "matching '%s' with '%s' failed", keyName (cur), patternString);
		}
	}

	CLI_PRINT (CLI_LOG_VERBOSE, "\nfound %ld keys", count);

cleanup:
	if (!noNewLine)
	{
		printf ("%c", nullTerm ? '\0' : '\n');
	}

	regfreeWrapper (regex);
	keyDel (root);
	ksDel (conf);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
