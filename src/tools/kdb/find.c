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
#include <regex.h>
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

	regex_t regex;
	if (regcomp (&regex, patternString, 0))
	{
		ret = 1;
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey, "could not compile regex pattern: '%s'", patternString);
		return ret;
	}

	Key * root = keyNew ("/", KEY_END);

	KeySet * conf = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	CLI_PRINT (CLI_LOG_DEBUG, "loading %s\n", "/");

	if (kdbGet (handle, conf, root) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load key database", GET_ERR (root));
		ret = 1;
		goto cleanup;
	}
	CLI_PRINT (CLI_LOG_DEBUG, "loaded %s\n", "/");

	Key * cur = NULL;

	int regexResult;
	int count = 0;
	char errorMessageBuff[100];

	for (elektraCursor it = 0; it < ksGetSize (conf); ++it)
	{
		cur = ksAtCursor (conf, it);
		regexResult = regexec (&regex, keyName (cur), 0, NULL, 0);
		CLI_PRINT (CLI_LOG_DEBUG, "matching '%s' -> %d\n", keyName (cur), regexResult);

		if (!regexResult)
		{
			CLI_PRINT (CLI_LOG_NONE, "%s\n", keyName (cur));
			count++;
		}
		else if (regexResult != REG_NOMATCH)
		{
			regerror (regexResult, &regex, errorMessageBuff, sizeof (errorMessageBuff));
			CLI_PRINT (CLI_LOG_VERBOSE, "matching '%s' with '%s' failed: %s", keyName (cur), patternString, errorMessageBuff);
		}
	}

	CLI_PRINT (CLI_LOG_VERBOSE, "\nfound %ld keys", count);

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	keyDel (root);
	elektraFree ((void *) fmtBuffer);
	ksDel (conf);
	regfree (&regex);
	kdbClose (handle, errorKey);
	return ret;
}
