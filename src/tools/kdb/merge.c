/**
 * @file
 *
 * @brief Implementation of kdb cmerge command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <merge.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbmerge.h>
#include <stdio.h>
#include <string.h>

#define COMMAND_NAME "merge"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMergeSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Three-way merge of Key sets.", KEY_META,
				   "command", COMMAND_NAME, KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/ourpath", KEY_META, "description", "Path to the keyset to serve as our.",
			     KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/theirpath", KEY_META, "description", "Path to the keyset to serve as their.",
			     KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/basepath", KEY_META, "description", "Path to the base keyset.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/resultpath", KEY_META, "description",
				   "Path without keys where the merged keyset will be saved.", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "3", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/strategy", KEY_META, "description",
			     "strategy  to use in case of a conflict. Options: ours,theirs,abort(default)", KEY_META, "opt", "s", KEY_META,
			     "opt/arg/help", "STRATEGY", KEY_META, "opt/long", "strategy", KEY_META, "opt/arg", "required", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/force", KEY_META, "description", "Overwrite existing keys in  result.",
				   KEY_META, "opt", "f", KEY_META, "opt/long", "force", KEY_META, "opt/arg", "none", KEY_END));


	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}


void printKsNames (KeySet * ks)
{
	Key * cur = NULL;
	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		cur = ksAtCursor (ks, it);
		printf (" %s\n", keyName (cur));
	}
}

int getKeySet (Key * where, KeySet ** ks, char * verboseName, Key * errorKey)
{
	KDB * handle = kdbOpen (NULL, errorKey);
	int result = kdbGet (handle, *ks, where);
	kdbClose (handle, errorKey);
	KeySet * old = *ks;
	*ks = ksCut (*ks, where);
	ksDel (old);
	ksLookup (*ks, where, 0);
	if (verboseName != NULL)
	{
		printf ("got %s: %s with keys\n", verboseName, keyName (where));
		printKsNames (*ks);
	}
	return result;
}

int execMerge (KeySet * options, Key * errorKey)
{
	GET_BASIC_OPTIONS

	// optional args
	bool force = false;
	tmp = GET_OPTION_KEY (options, "force");
	if (tmp != NULL)
	{
		elektraKeyToBoolean (GET_OPTION_KEY (options, "force"), &force);
		keyDel (tmp);
	}

	tmp = GET_OPTION_KEY (options, "strategy");
	int strategy = MERGE_STRATEGY_ABORT;
	if (tmp != NULL)
	{
		const char * strategyName;
		elektraKeyToString (tmp, &strategyName);
		keyDel (tmp);
		if (elektraStrCmp (strategyName, "ours") == 0)
		{
			strategy = MERGE_STRATEGY_OUR;
		}
		else if (elektraStrCmp (strategyName, "theirs") == 0)
		{
			strategy = MERGE_STRATEGY_THEIR;
		}
		else if (elektraStrCmp (strategyName, "abort") != 0)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "'%s' is not a valid strategy.", strategyName);
			RETURN (2)
		}
	}

	// required args
	const char * ourpath = getKeyNameFromOptions (GET_OPTION (options, "ourpath"), errorKey, verbose);
	if (ourpath == NULL)
	{
		RETURN (2)
	}

	const char * theirpath = getKeyNameFromOptions (GET_OPTION (options, "theirpath"), errorKey, verbose);
	if (theirpath == NULL)
	{
		elektraFree ((void *) ourpath);
		RETURN (2)
	}

	const char * basepath = getKeyNameFromOptions (GET_OPTION (options, "basepath"), errorKey, verbose);
	if (basepath == NULL)
	{
		elektraFree ((void *) ourpath);
		elektraFree ((void *) theirpath);
		RETURN (2)
	}

	const char * resultpath = getKeyNameFromOptions (GET_OPTION (options, "resultpath"), errorKey, verbose);
	if (resultpath == NULL)
	{
		elektraFree ((void *) ourpath);
		elektraFree ((void *) theirpath);
		elektraFree ((void *) basepath);
		RETURN (2)
	}

	int ret = 0;
	Key * oursRoot = keyNew (ourpath, KEY_END);
	Key * theirsRoot = keyNew (theirpath, KEY_END);
	Key * baseRoot = keyNew (basepath, KEY_END);
	Key * resultRoot = keyNew (resultpath, KEY_END);

	KeySet * ours = ksNew (0, KS_END);
	KeySet * theirs = ksNew (0, KS_END);
	KeySet * base = ksNew (0, KS_END);

	KeySet * discard = NULL;

	KeySet * atResult = ksNew (0, KS_END);
	KDB * handle = kdbOpen (NULL, errorKey);
	if (kdbGet (handle, atResult, resultRoot) < 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "get for \'%s\': %s", keyName (resultRoot), GET_ERR (resultRoot));
		ret = 5;
		goto cleanup;
	}
	discard = ksCut (atResult, resultRoot);
	if (ksGetSize (discard) != 0 && !force)
	{ // overwrite existing values
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "There are keys in the result path. Use -f to override them.");
		ret = 3;
		goto cleanup;
	}
	else if (ksGetSize (discard) != 0)
	{
		CLI_PRINT (CLI_LOG_VERBOSE, "will remove %ld keys, because %s was set", ksGetSize (discard), BOLD ("-f"));
	}

	if (getKeySet (oursRoot, &ours, verbose ? "our" : NULL, errorKey) < 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "get for \'%s\': %s", keyName (oursRoot), GET_ERR (resultRoot));
		ret = 3;
		goto cleanup;
	}
	if (getKeySet (theirsRoot, &theirs, verbose ? "their" : NULL, errorKey) < 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "get for \'%s\': %s", keyName (theirsRoot), GET_ERR (resultRoot));
		ret = 3;
		goto cleanup;
	}
	if (getKeySet (baseRoot, &base, verbose ? "base" : NULL, errorKey) < 0)
	{
		ELEKTRA_SET_INTERNAL_ERRORF (errorKey, "get for \'%s\': %s", keyName (baseRoot), GET_ERR (resultRoot));
		ret = 3;
		goto cleanup;
	}

	Key * mergeInfo = keyNew ("/", KEY_END);
	KeySet * mergeResult = elektraMerge (ours, oursRoot, theirs, theirsRoot, base, baseRoot, resultRoot, strategy, mergeInfo);

	int nrConflicts = elektraMergeGetConflicts (mergeInfo);
	keyDel (mergeInfo);

	if (mergeResult == NULL)
	{
		if (nrConflicts > 0 && strategy == MERGE_STRATEGY_ABORT)
		{
			ELEKTRA_SET_CONFLICTING_STATE_ERRORF (errorKey, "Aborted due to %d conflicts.", nrConflicts);
			ret = 12;
		}
		else
		{
			ELEKTRA_SET_INTERNAL_ERROR (errorKey, "An error occurred during the merge.");
			ret = 11;
		}
		goto cleanup;
	}

	if (ksAppend (atResult, mergeResult) < 0)
	{ // not possible, but here for completeness
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (errorKey, "An error occurred during the merge.");
		ret = 11;
		ksDel (mergeResult);
		goto cleanup;
	}
	if (kdbSet (handle, atResult, resultRoot) < 0)
	{ // could not save result back to kdb
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (errorKey, "Could not set result: %s", GET_ERR (errorKey));
		ret = 5;
		ksDel (mergeResult);
		goto cleanup;
	}
	ksDel (mergeResult);

cleanup:
	if (!noNewLine)
	{
		printf ("\n");
	}
	elektraFree ((void *) ourpath);
	elektraFree ((void *) theirpath);
	elektraFree ((void *) basepath);
	elektraFree ((void *) resultpath);
	keyDel (oursRoot);
	keyDel (theirsRoot);
	keyDel (baseRoot);
	keyDel (resultRoot);
	ksDel (ours);
	ksDel (theirs);
	ksDel (base);
	ksDel (atResult);
	ksDel (discard);
	kdbClose (handle, errorKey);

	RETURN (ret)
}
