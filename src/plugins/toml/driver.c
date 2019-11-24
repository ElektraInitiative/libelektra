#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>

#include "driver.h"
#include "error.h"
#include "null_indicator.h"
#include "parser.h"
#include "utility.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

static Driver * createDriver (Key * parent, KeySet * keys);
static void destroyDriver (Driver * driver);
static int driverParse (Driver * driver);
static void driverNewCommentList (Driver * driver, const char * comment, size_t spaceCount);
static void driverClearCommentList (Driver * driver);
static void driverDrainCommentsToKey (Key * key, Driver * driver);
static void firstCommentAsInlineToPrevKey (Driver * driver);
static void driverCommitLastScalarToParentKey (Driver * driver);
static void driverClearLastScalar (Driver * driver);

static void pushCurrKey (Driver * driver);
static void setCurrKey (Driver * driver, const Key * key);
static void setPrevKey (Driver * driver, Key * key);
static void resetCurrKey (Driver * driver);
static void extendCurrKey (Driver * driver, const char * name);
static ParentList * pushParent (ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);
static bool sameString (const char * raw, const char * transformed, char terminator, int terminatorCount);

int tomlRead (KeySet * keys, Key * parent)
{
	Driver * driver = createDriver (parent, keys);
	int status = 0;
	if (driver != NULL)
	{
		status = driverParse (driver);
		destroyDriver (driver);
	}
	else
	{
		status = 1;
	}


	// dumpKS(keys);
	ksRewind (keys);
	return status;
}

static Driver * createDriver (Key * parent, KeySet * keys)
{
	Driver * driver = (Driver *) elektraCalloc (sizeof (Driver));
	if (driver == NULL)
	{
		return NULL;
	}
	driver->root = parent;
	driver->keys = keys;
	driver->parentStack = pushParent (NULL, keyDup (parent));
	driver->filename = elektraStrDup (keyString (parent));
	driver->simpleTableActive = false;
	driver->drainCommentsOnKeyExit = true;
	driver->errorSet = false;
	return driver;
}

static void destroyDriver (Driver * driver)
{
	if (driver != NULL)
	{
		setCurrKey (driver, NULL);
		setPrevKey (driver, NULL);
		driverClearLastScalar (driver);
		if (driver->filename != NULL)
		{
			elektraFree (driver->filename);
			driver->filename = NULL;
		}
		while (driver->parentStack != NULL)
		{
			driver->parentStack = popParent (driver->parentStack);
		}
		while (driver->indexStack != NULL)
		{
			driver->indexStack = popIndex (driver->indexStack);
		}
		while (driver->tableArrayStack != NULL)
		{
			driver->tableArrayStack = popTableArray (driver->tableArrayStack);
		}
		commentListFree (driver->commentRoot);
		driver->commentRoot = NULL;
		driver->commentBack = NULL;
		elektraFree (driver);
	}
}

static int driverParse (Driver * driver)
{
	// ksAppendKey (driver->keys, driver->root);
	FILE * file = fopen (driver->filename, "rb");
	if (file == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (driver->root, keyString (driver->root));
		return 1;
	}
	yyin = file;
	int yyResult = yyparse (driver);
	fclose (file);
	return driver->errorSet == true || yyResult != 0;
}

void driverExitToml (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driverDrainCommentsToKey (driver->root, driver);
}

void driverEnterKey (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	resetCurrKey (driver);
}

void driverExitKey (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	Key * existing = ksLookup (driver->keys, driver->currKey, 0);
	if (existing != NULL)
	{
		keyRewindMeta (existing);
		bool isTableArrayList = false;
		for (const Key * meta = keyNextMeta (existing); meta != NULL; meta = keyNextMeta (existing))
		{
			if (strcmp (keyName (meta), "tomltype") == 0 && strcmp (keyString (meta), "tablearray") == 0)
			{
				isTableArrayList = true;
				break;
			}
		}
		if (!isTableArrayList)
		{
			// Only allow table array keys to be read multiple times
			driverError (driver, ERROR_SEMANTIC, driver->currLine, "Malformed input: Multiple occurences of keyname: '%s'",
				     keyName (existing));
		}
	}

	pushCurrKey (driver);
	if (driver->drainCommentsOnKeyExit)
	{
		driverDrainCommentsToKey (driver->parentStack->key, driver);
	}
	setOrderForKey (driver->parentStack->key, driver->order++);
}

void driverExitKeyValue (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driverCommitLastScalarToParentKey (driver);

	if (driver->prevKey != NULL)
	{
		keyDecRef (driver->prevKey);
		keyDel (driver->prevKey);
		driver->prevKey = NULL;
	}
	if (driver->prevKey != NULL)
	{
		keyDecRef (driver->prevKey);
	}
	driver->prevKey = driver->parentStack->key;
	keyIncRef (driver->prevKey);

	driver->parentStack = popParent (driver->parentStack);
}

void driverExitOptCommentKeyPair (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->commentRoot != NULL)
	{
		if (driver->prevKey == NULL)
		{
			driverError (driver, ERROR_INTERNAL, 0, "Wanted to assign inline comment to keypair, but keypair key is NULL.");
			return;
		}
		if (driver->commentRoot->next != NULL)
		{
			driverError (driver, ERROR_INTERNAL, 0,
				     "More than one comment existing after exiting keypair, expected up to one.");
			return;
		}
		int err = keyAddInlineComment (driver->prevKey, driver->commentRoot) != 0;
		if (err != 0)
		{
			driverErrorGeneric (driver, err, "driverExitOptCommentTable", "keyAddInlineComment");
		}
		driverClearCommentList (driver);
	}
}

void driverExitOptCommentTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->commentRoot != NULL)
	{
		if (driver->parentStack->key == NULL)
		{
			driverError (driver, ERROR_INTERNAL, 0, "Wanted to assign inline comment to table, but table key is NULL.");
			return;
		}
		if (driver->commentRoot->next != NULL)
		{
			driverError (driver, ERROR_INTERNAL, 0, "More than one comment existing after exiting table, expected up to one.");
			return;
		}
		int err = keyAddInlineComment (driver->parentStack->key, driver->commentRoot);
		if (err != 0)
		{
			driverErrorGeneric (driver, err, "driverExitOptCommentTable", "keyAddInlineComment");
		}
		driverClearCommentList (driver);

		if (!driver->simpleTableActive) // if we're here and not in a simple table, we exited an table array
		{
			// We need to emit the table array key ending with /#n (having no value)
			// Otherwise, the inline comment we just added will be ignored, if the table array is empty
			ksAppendKey (driver->keys, driver->parentStack->key);
		}
	}
}

void driverExitSimpleKey (Driver * driver, Scalar * name)
{
	if (driver->errorSet)
	{
		freeScalar (name);
		return;
	}
	if (name == NULL)
	{
		return;
	}

	// scalar must be singline line literal/basic string or bare string
	// if we got int/float/boolean/date, we must check, if it fits the
	// criteria for a BARE_STRING
	switch (name->type)
	{
	case SCALAR_STRING_LITERAL:
	case SCALAR_STRING_BASIC:
	case SCALAR_STRING_BARE: // always valid
		break;
	case SCALAR_STRING_ML_LITERAL:
	case SCALAR_STRING_ML_BASIC: // always invalid
		driverError (driver, ERROR_SEMANTIC, name->line,
			     "Malformed input: Invalid simple key: Found multiline string, but is not allowed");
		break;
	case SCALAR_FLOAT_NUM: // split up floating point numbers (contains a DOT, so we get 2 simple keys instead)
	{
		const char * dot = strchr (name->str, '.');
		if (dot != NULL)
		{
			size_t splitPos = dot - name->str;
			char * first = elektraCalloc (sizeof (char) * (splitPos + 1));
			char * second = elektraCalloc (sizeof (char) * (elektraStrLen (name->str) - splitPos - 1));
			strncpy (first, name->str, splitPos);
			strncpy (second, dot + 1, elektraStrLen (name->str) - splitPos - 1);
			if (isValidBareString (first) && isValidBareString (second))
			{
				extendCurrKey (driver, first);
				extendCurrKey (driver, second);
			}
			else
			{
				driverError (driver, ERROR_SEMANTIC, name->line,
					     "Malformed input: Invalid simple key: '%s' contains invalid characters, only alphanumeric, "
					     "underline, "
					     "hyphen allowed",
					     name->str);
			}
			elektraFree (first);
			elektraFree (second);
		}
	}
	break;
	default: // check validity
		if (!isValidBareString (name->str))
		{
			driverError (driver, ERROR_SEMANTIC, name->line,
				     "Malformed input: Invalid simple key: '%s' contains invalid characters, only alphanumeric, underline, "
				     "hyphen allowed");
		}
		break;
	}
	if (name->type != SCALAR_FLOAT_NUM)
	{
		char * translated = translateScalar (name);
		extendCurrKey (driver, translated);
		elektraFree (translated);
	}
	driver->currLine = name->line;
	freeScalar (name);
}

void driverExitValue (Driver * driver, Scalar * scalar)
{
	if (driver->errorSet)
	{
		freeScalar (scalar);
		return;
	}
	if (scalar == NULL)
	{
		return;
	}
	switch (scalar->type)
	{
	case SCALAR_STRING_BARE: // No bare on rhs allowed
		driverError (driver, ERROR_SEMANTIC, scalar->line, "Malformed input: Found bare string on rhs, but is not allowed");
		break;
	case SCALAR_DATE_OFFSET_DATETIME:
	case SCALAR_DATE_LOCAL_DATETIME:
	case SCALAR_DATE_LOCAL_DATE:
	case SCALAR_DATE_LOCAL_TIME: // check semantics of datetimes
		if (!isValidDateTime (scalar))
		{
			driverError (driver, ERROR_SEMANTIC, scalar->line, "Malformed input: Invalid datetime: '%s'", scalar->str);
		}
		break;
	default: // all other scalar types allowed and valid, no semantic invalidities
		break;
	}
	driverClearLastScalar (driver);
	driver->lastScalar = scalar;
	driver->currLine = scalar->line;
}

void driverEnterSimpleTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->simpleTableActive)
	{
		driver->parentStack = popParent (driver->parentStack);
	}
	else
	{
		driver->simpleTableActive = true;
	}
	resetCurrKey (driver);
}

void driverExitSimpleTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	keySetMeta (driver->parentStack->key, "tomltype", "simpletable");
	ksAppendKey (driver->keys, driver->parentStack->key);
}

void driverEnterTableArray (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->simpleTableActive)
	{
		driver->parentStack = popParent (driver->parentStack);
		driver->simpleTableActive = false;
	}
	if (driver->tableArrayStack != NULL)
	{
		driver->parentStack = popParent (driver->parentStack); // pop old table array key
	}
	setCurrKey (driver, driver->root);
	driver->drainCommentsOnKeyExit = false; // don't assign comments on unindexed table array keys
}

void driverExitTableArray (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	int rel = driver->tableArrayStack == NULL ? -1 : keyRel (driver->tableArrayStack->key, driver->parentStack->key);
	if (rel == 0) // same table array name -> next element
	{
		driver->tableArrayStack->currIndex++;
	}
	else if (rel > 0) // below top name -> push new sub table array
	{
		driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->parentStack->key);
	}
	else if (rel < 0) // no relation, pop table array stack until some relation exists (or NULL)
	{
		while (driver->tableArrayStack != NULL && keyRel (driver->tableArrayStack->key, driver->parentStack->key) < 0)
		{
			driver->tableArrayStack = popTableArray (driver->tableArrayStack);
		}
		if (driver->tableArrayStack == NULL)
		{
			driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->parentStack->key);
		}
		else
		{
			driver->tableArrayStack->currIndex++;
		}
	}
	driver->parentStack = popParent (driver->parentStack); // pop key name without any indices (was pushed after exiting key)
	driver->order--;				       // Undo order increment, table array entries are ordered by their index

	Key * key = buildTableArrayKeyName (driver->tableArrayStack);
	Key * rootNameKey = keyDup (key);
	keyAddName (rootNameKey, "..");
	Key * existingRoot = ksLookup (driver->keys, rootNameKey, 0);
	if (existingRoot == NULL)
	{
		existingRoot = rootNameKey;
		keySetMeta (existingRoot, "tomltype", "tablearray");
		keySetMeta (existingRoot, "array", "#0");
		setOrderForKey (existingRoot, driver->order++);
		ksAppendKey (driver->keys, existingRoot);
	}
	else
	{
		keyDel (rootNameKey);
		keyUpdateArrayMetakey (existingRoot, driver->tableArrayStack->currIndex);
	}

	driver->parentStack = pushParent (driver->parentStack, key);

	driverDrainCommentsToKey (driver->parentStack->key, driver);
	driver->drainCommentsOnKeyExit = true; // only set to false while table array unindexed key is generated
}

void driverEnterArray (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driver->indexStack = pushIndex (driver->indexStack, 0);
	const Key * meta = findMetaKey(driver->parentStack->key, "array");	// check for nested arrays
	if (meta != NULL) {
		ELEKTRA_ASSERT(elektraStrCmp(keyString(meta), "") != 0, "Empty array index shouldn't be possible, we should've already called driverEnterArrayElement once");
		Key * key = keyAppendIndex(0, driver->parentStack->key);
		driver->parentStack = pushParent(driver->parentStack, key);
	}
	keySetMeta (driver->parentStack->key, "array", "");
}

void driverExitArray (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	firstCommentAsInlineToPrevKey (driver);
	// TODO: Handle comments after last element in array (and inside array brackets)
	// Must check on how (and where) the trailing comments should be stored
	// Afterwards, the next line can be removed
	driverDrainCommentsToKey (NULL, driver);

	driver->indexStack = popIndex (driver->indexStack);
	ksAppendKey (driver->keys, driver->parentStack->key);
}

void driverEmptyArray (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driverEnterArray (driver);
	driverExitArray (driver);
}

void driverEnterArrayElement (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->indexStack->value == SIZE_MAX)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Array index at maximum range of size_t: SIZE_MAX");
		return;
	}

	if (driver->indexStack->value > 0 && driver->commentRoot != NULL)
	{ // first comment of non-first array elements is inline comment of previous element
		firstCommentAsInlineToPrevKey (driver);
	}

	Key * key = keyAppendIndex (driver->indexStack->value, driver->parentStack->key);

	keySetMeta (driver->parentStack->key, "array", keyBaseName (key));
	driver->parentStack = pushParent (driver->parentStack, key);

	driver->indexStack->value++;

	driverDrainCommentsToKey (driver->parentStack->key, driver);
}

void driverExitArrayElement (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	// driverEnterArrayElement (driver);
	if (driver->lastScalar != NULL) // NULL can happen on eg inline tables as elements
	{
		driverCommitLastScalarToParentKey (driver);
	}
	if (driver->prevKey != NULL)
	{
		keyDecRef (driver->prevKey);
		keyDel (driver->prevKey);
	}
	driver->prevKey = driver->parentStack->key;
	keyIncRef (driver->prevKey);
	driver->parentStack = popParent (driver->parentStack);
}

void driverEnterInlineTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	keySetMeta (driver->parentStack->key, "tomltype", "inlinetable");
	ksAppendKey (driver->keys, driver->parentStack->key);
}

void driverExitInlineTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driverClearLastScalar (driver);
}

void driverEmptyInlineTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driverEnterInlineTable (driver);
	// Don't need to call exit, because no scalar value emission possible in empty inline table
}

void driverExitComment (Driver * driver, Scalar * comment)
{
	if (driver->errorSet)
	{
		freeScalar (comment);
		return;
	}
	if (comment == NULL)
	{
		return;
	}
	if (driver->newlineCount > 0)
	{
		if (driver->commentRoot == NULL)
		{
			driverNewCommentList (driver, NULL, 0);
			driver->newlineCount--;
		}
		driver->commentBack = commentListAddNewlines (driver->commentBack, driver->newlineCount);
		if (driver->commentBack == NULL)
		{
			driverErrorGeneric (driver, ERROR_MEMORY, "driverExitComment", "commentListAddNewlines");
		}
		driver->newlineCount = 0;
	}

	if (driver->commentRoot == NULL)
	{
		driverNewCommentList (driver, comment->str, comment->leadingSpaces);
	}
	else
	{
		driver->commentBack = commentListAdd (driver->commentBack, comment->str, comment->leadingSpaces);
		if (driver->commentBack == NULL)
		{
			driverErrorGeneric (driver, ERROR_MEMORY, "driverExitComment", "commentListAdd");
		}
	}
	driver->currLine = comment->line;
	freeScalar (comment);
}

void driverExitNewline (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->newlineCount == SIZE_MAX)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Newline counter at maximum range of size_t: SIZE_MAX");
		return;
	}
	driver->newlineCount++;
}

static void driverNewCommentList (Driver * driver, const char * comment, size_t spaceCount)
{
	if (driver->commentRoot != NULL || driver->commentBack != NULL)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Wanted to create new comment list, but comment list already existing.");
	}
	driver->commentRoot = commentListNew (comment, spaceCount);
	driver->commentBack = driver->commentRoot;
}

static void driverClearCommentList (Driver * driver)
{
	commentListFree (driver->commentRoot);
	driver->commentRoot = NULL;
	driver->commentBack = NULL;
}

static void firstCommentAsInlineToPrevKey (Driver * driver)
{
	if (driver->commentRoot != NULL)
	{
		CommentList * comment = driver->commentRoot;
		if (driver->commentRoot->next == NULL)
		{
			ELEKTRA_ASSERT (driver->commentBack == driver->commentRoot,
					"Expected comment root to be back, because root has no next");
			driver->commentRoot = NULL;
			driver->commentBack = NULL;
		}
		else
		{
			driver->commentRoot = driver->commentRoot->next;
			comment->next = NULL;
		}
		int err = keyAddInlineComment (driver->prevKey, comment);
		if (err != 0)
		{
			driverErrorGeneric (driver, err, "firstCommentAsInlineToPrevKey", "keyAddInlineComment");
		}
		commentListFree (comment); // only clears the inline comment recently added to the key
	}
}

static void driverDrainCommentsToKey (Key * key, Driver * driver)
{
	if (driver->newlineCount > 0)
	{
		if (driver->commentRoot == NULL)
		{
			driverNewCommentList (driver, NULL, 0);
			driver->newlineCount--;
		}
		driver->commentBack = commentListAddNewlines (driver->commentBack, driver->newlineCount);
		if (driver->commentBack == NULL)
		{
			driverErrorGeneric (driver, ERROR_MEMORY, "driverDrainCommentsToKey", "commentListAddNewlines");
		}

		driver->newlineCount = 0;
	}

	if (key != NULL)
	{
		int err = keyAddCommentList (key, driver->commentRoot);
		if (err != 0)
		{
			driverErrorGeneric (driver, err, "driverDrainCommentsToKey", "keyAddCommentList");
		}
	}
	driverClearCommentList (driver);
}

static void pushCurrKey (Driver * driver)
{
	driver->parentStack = pushParent (driver->parentStack, driver->currKey);
}

static void setCurrKey (Driver * driver, const Key * key)
{
	if (driver->currKey != NULL)
	{
		keyDecRef (driver->currKey);
		keyDel (driver->currKey);
	}
	if (key != NULL)
	{
		driver->currKey = keyNew (keyName (key), KEY_END);
		keyIncRef (driver->currKey);
	}
	else
	{
		driver->currKey = NULL;
	}
}

static void setPrevKey (Driver * driver, Key * key)
{
	if (driver->prevKey != NULL)
	{
		keyDecRef (driver->prevKey);
		keyDel (driver->prevKey);
	}
	driver->prevKey = key;
	if (key != NULL)
	{
		keyIncRef (key);
	}
}

static void resetCurrKey (Driver * driver)
{
	setCurrKey (driver, driver->parentStack->key);
}

static void extendCurrKey (Driver * driver, const char * name)
{
	if (driver->currKey == NULL)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Wanted to extend current key, but current key is NULL.");
		return;
	}
	if (elektraStrLen (name) == 1)
	{
		driverError (driver, ERROR_SYNTACTIC, 0, "Wanted to extend current key with empty name, but mustn't be empty.");
	}
	keyAddBaseName (driver->currKey, name);
}

static ParentList * pushParent (ParentList * top, Key * key)
{
	// printf ("PUSH %s\n", keyName (key));
	ParentList * parent = elektraCalloc (sizeof (ParentList));
	parent->key = key;
	keyIncRef (key);
	parent->next = top;
	return parent;
}

static ParentList * popParent (ParentList * top)
{
	// printf ("POP %s -> %s\n", keyName (top->key), top->next == NULL ? "NULL" : keyName (top->next->key));
	ParentList * newTop = top->next;
	keyDecRef (top->key);
	keyDel (top->key);
	elektraFree (top);
	return newTop;
}

static IndexList * pushIndex (IndexList * top, int value)
{
	IndexList * newIndex = elektraCalloc (sizeof (IndexList));
	newIndex->value = value;
	newIndex->next = top;
	return newIndex;
}

static IndexList * popIndex (IndexList * top)
{
	IndexList * newTop = top->next;
	elektraFree (top);
	return newTop;
}

static void driverCommitLastScalarToParentKey (Driver * driver)
{
	if (driver->lastScalar == NULL)
	{
		return;
	}
	if (driver->parentStack == 0)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Wanted to assign scalar to top parent key, but top parent key is NULL.");
		return;
	}
	char * elektraStr = translateScalar (driver->lastScalar);
	if (elektraStr == NULL)
	{
		driverError (driver, ERROR_MEMORY, 0, "Could allocate memory for scalar translation");
		return;
	}
	if (elektraStrCmp (elektraStr, NULL_INDICATOR) == 0)
	{
		keySetBinary (driver->parentStack->key, NULL, 0);
	}
	else
	{
		keySetString (driver->parentStack->key, elektraStr);

		//const char * type = getTypeCheckerType (driver->lastScalar);
		// keySetMeta (driver->parentStack->key, "type", type);

		switch (driver->lastScalar->type)
		{
		case SCALAR_STRING_LITERAL:
			if (!sameString (driver->lastScalar->str, elektraStr, '\'', 1))
			{
				char * orig = stripTerminators (driver->lastScalar->str, 1);
				if (orig == NULL)
				{
					elektraFree (elektraStr);
					driverError (driver, ERROR_MEMORY, 0, "Could allocate memory for stripped string");
					return;
				}
				keySetMeta (driver->parentStack->key, "origvalue", orig);
				elektraFree (orig);
			}
			break;
		case SCALAR_STRING_ML_LITERAL:
			if (!sameString (driver->lastScalar->str, elektraStr, '\'', 3))
			{
				char * orig = stripTerminators (driver->lastScalar->str, 3);
				if (orig == NULL)
				{
					elektraFree (elektraStr);
					driverError (driver, ERROR_MEMORY, 0, "Could allocate memory for stripped string");
					return;
				}
				keySetMeta (driver->parentStack->key, "origvalue", orig);
				elektraFree (orig);
			}
			break;
		case SCALAR_STRING_BASIC:
			if (!sameString (driver->lastScalar->str, elektraStr, '"', 1))
			{
				char * orig = stripTerminators (driver->lastScalar->str, 1);
				if (orig == NULL)
				{
					elektraFree (elektraStr);
					driverError (driver, ERROR_MEMORY, 0, "Could allocate memory for stripped string");
					return;
				}
				keySetMeta (driver->parentStack->key, "origvalue", orig);
				elektraFree (orig);
			}
			break;
		case SCALAR_STRING_ML_BASIC:
			if (!sameString (driver->lastScalar->str, elektraStr, '"', 3))
			{
				char * orig = stripTerminators (driver->lastScalar->str, 3);
				if (orig == NULL)
				{
					elektraFree (elektraStr);
					driverError (driver, ERROR_MEMORY, 0, "Could allocate memory for stripped string");
					return;
				}
				keySetMeta (driver->parentStack->key, "origvalue", orig);
				elektraFree (orig);
			}
			break;
		case SCALAR_BOOLEAN: // done by type plugin
			break;
		default:
			if (elektraStrCmp (elektraStr, driver->lastScalar->str) != 0)
			{
				keySetMeta (driver->parentStack->key, "origvalue", driver->lastScalar->str);
			}
			break;
		}
	}
	elektraFree (elektraStr);

	ksAppendKey (driver->keys, driver->parentStack->key);
	driverClearLastScalar (driver);
}

static bool sameString (const char * raw, const char * transformed, char terminator, int terminatorCount)
{
	raw += terminatorCount;
	while (*raw != 0 && *transformed != 0)
	{
		if (*raw != *transformed)
		{
			return false;
		}
		raw++;
		transformed++;
	}
	for (int i = 0; i < terminatorCount; i++)
	{
		if (*raw != terminator)
		{
			return false;
		}
		raw++;
	}
	return *raw == 0 && *transformed == 0;
}

static void driverClearLastScalar (Driver * driver)
{
	freeScalar (driver->lastScalar);
	driver->lastScalar = NULL;
}
