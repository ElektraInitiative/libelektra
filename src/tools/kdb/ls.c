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
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/nullterm", KEY_META, "description", "Use binary 0 termination.",
				   KEY_META, "opt", "0", KEY_META, "opt/long", "null", KEY_META, "opt/arg", "none", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/name", KEY_META, "description", "The name of the key", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "0", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execLs (KeySet * options, Key * errorKey)
{

	Key * tmp = GET_OPTION_KEY (options, "mindepth");
	kdb_long_t minDepth = 0;
	if (tmp != NULL)
	{
		elektraKeyToLong (tmp, &minDepth);
	}

	tmp = GET_OPTION_KEY (options, "maxdepth");
	kdb_long_t maxDepth = -1;
	if (tmp != NULL)
	{
		elektraKeyToLong (tmp, &maxDepth);
	}


	if (maxDepth != -1 && maxDepth <= minDepth)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "the maximum depth has to be larger than the minimum depth");
		return -1;
	}
	if (maxDepth != -1 && maxDepth < 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "the maximum depth has to be a positive number");
		return -1;
	}
	if (minDepth < 0)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "the minimum depth has to be a positive number");
		return -1;
	}


	bool verbose = false;
	tmp = GET_OPTION_KEY (options, "verbose");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "verbose"), &verbose);
	}

	bool nullTerm = false;
	tmp = GET_OPTION_KEY (options, "nullterm");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (tmp, &nullTerm);
	}

	const char * name = getKeyNameFromOptions (options, GET_OPTION (options, "name"), errorKey, verbose);
	if (name == NULL) return 1;

	Key * whereToLook = keyNew (name, KEY_END);
	int rootDepth = getKeyNameDepth (name);

	KeySet * searchIn = ksNew (0, KS_END);

	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, searchIn, whereToLook) == -1)
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "could not load '%s': %s", name, GET_ERR (whereToLook));
		elektraFree ((void *) name);
		kdbClose (handle, errorKey);
		ksDel (searchIn);
		keyDel (whereToLook);
		return -1;
	}
	kdbClose (handle, errorKey);
	if (verbose)
	{
		printf ("size of all keys: %zd\n", ksGetSize (searchIn));
	}
	KeySet * part = ksCut (searchIn, whereToLook);
	keyDel (whereToLook);
	if (verbose)
	{
		printf ("size of requested keys: %zd\n", ksGetSize (part));
	}

	Key * cur = NULL;
	for (elektraCursor it = 0; it < ksGetSize (part); ++it)
	{
		cur = ksAtCursor (part, it);
		int currentDepth = getKeyNameDepth (keyName (cur));
		if ((maxDepth == -1 || currentDepth < rootDepth + maxDepth) && currentDepth >= rootDepth + minDepth)
		{
			printf ("%s%c", keyName (cur), nullTerm ? '\0' : '\n');
		}
	}

	elektraFree ((void *) name);
	ksDel (part);
	ksDel (searchIn);
	return 0;
}

int getKeyNameDepth (const char * name)
{
	int nameLen = strlen (name);
	int slashCount = 0;

	for (int i = 0; i < nameLen; i++)
	{
		slashCount += name[i] == '/' && (i == 0 || name[i - 1] != '\\');
	}

	return slashCount;
}
