/**
 * @file driver.c
 *
 * @brief Used by the TOML lexer/parser for generating appropriate Elektra Key/Values.
 *
 * All functions of the format driverEnter/driverExit are strongly bound to their similarly named grammar rules in the bison parser.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdb.h>
#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>

#include "driver.h"
#include "error.h"
#include "parser.h"
#include "utility.h"

extern int yyparse (Driver * driver);
extern int yylineno;
extern void initializeLexer (FILE * file);
extern void clearLexer (void);

static Driver * createDriver (ElektraKey * parent, ElektraKeyset * keys);
static void destroyDriver (Driver * driver);
static int driverParse (Driver * driver);
static void driverNewCommentList (Driver * driver, const char * comment, const char * orig);
static void driverClearCommentList (Driver * driver);
static bool driverDrainCommentsToKey (ElektraKey * key, Driver * driver);
static void firstCommentAsInlineToPrevKey (Driver * driver);
static void driverCommitLastScalarToParentKey (Driver * driver);
static void driverClearLastScalar (Driver * driver);

static void pushCurrKey (Driver * driver);
static void setCurrKey (Driver * driver, const ElektraKey * key);
static void setPrevKey (Driver * driver, ElektraKey * key);
static void resetCurrKey (Driver * driver);
static void extendCurrKey (Driver * driver, const char * name);
static ParentList * pushParent (ParentList * top, ElektraKey * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);
static void assignStringMetakeys (ElektraKey * key, const char * origStr, const char * translatedStr, Driver * driver);
static bool handleSpecialStrings (const char * string, ElektraKey * key);
static void assignStringTomlType (ElektraKey * key, ScalarType stringType);
static void assignOrigValueIfDifferent (ElektraKey * key, const char * origValue);

int tomlRead (ElektraKeyset * keys, ElektraKey * parent)
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

	elektraKeysetRewind (keys);
	return status;
}

static Driver * createDriver (ElektraKey * parent, ElektraKeyset * keys)
{
	Driver * driver = (Driver *) elektraCalloc (sizeof (Driver));
	if (driver == NULL)
	{
		return NULL;
	}
	driver->root = parent;
	driver->keys = keys;
	driver->parentStack = pushParent (NULL, elektraKeyDup (parent, ELEKTRA_KEY_CP_ALL));
	driver->filename = elektraStrDup (elektraKeyString (parent));
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
	FILE * file = fopen (driver->filename, "rb");
	if (file == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (driver->root, elektraKeyString (driver->root));
		return 1;
	}
	initializeLexer (file);
	int yyResult = yyparse (driver);
	clearLexer ();
	fclose (file);
	return driver->errorSet == true || yyResult != 0;
}

void driverExitToml (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	if (driver->commentRoot != NULL)
	{
		ElektraKey * root = elektraKeyNew (elektraKeyName (driver->root), ELEKTRA_KEY_END);
		elektraKeysetAppendKey (driver->keys, root);
		driverDrainCommentsToKey (root, driver);
	}
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
	ElektraKey * existing = elektraKeysetLookup (driver->keys, driver->currKey, 0);
	if (existing != NULL && !isTableArray (existing) && elektraKeyCmp (existing, driver->root) != 0)
	{
		// Only allow table array keys to be read multiple times
		driverError (driver, ERROR_SEMANTIC, driver->currLine,
			     "Malformed input: Multiple occurences of keyname '%s', but keynames must be unique.", elektraKeyName (existing));
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
		elektraKeyDecRef (driver->prevKey);
		elektraKeyDel (driver->prevKey);
		driver->prevKey = NULL;
	}
	if (driver->prevKey != NULL)
	{
		elektraKeyDecRef (driver->prevKey);
	}
	driver->prevKey = driver->parentStack->key;
	elektraKeyIncRef (driver->prevKey);

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
			// if none is existing yet (because of preceding comments)
			// Otherwise, the inline comment we just added will be ignored, if the table array is empty
			if (elektraKeysetLookup (driver->keys, driver->parentStack->key, 0) == NULL)
			{
				elektraKeysetAppendKey (driver->keys, driver->parentStack->key);
			}
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

	// scalar must be single line literal/basic string or bare string
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
				driverError (
					driver, ERROR_SEMANTIC, name->line,
					"Malformed input: Invalid bare simple key: '%s' contains invalid characters, only alphanumeric, "
					"underline, "
					"hyphen allowed. Consider adding quotations around the string.",
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
			driverError (
				driver, ERROR_SEMANTIC, name->line,
				"Malformed input: Invalid bare simple key: '%s' contains invalid characters, only alphanumeric, underline, "
				"hyphen allowed. Consider adding quotations around the string.");
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
		driverError (
			driver, ERROR_SEMANTIC, scalar->line,
			"Malformed input: Found a bare string value, which is not allowed. Consider adding quotations around the string.");
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
	elektraKeySetMeta (driver->parentStack->key, "tomltype", "simpletable");
	elektraKeysetAppendKey (driver->keys, driver->parentStack->key);
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

	if (driver->tableArrayStack != NULL &&
	    elektraKeyCmp (driver->tableArrayStack->key, driver->parentStack->key) == 0) // same table array name -> next element
	{
		driver->tableArrayStack->currIndex++;
	}
	else if (driver->tableArrayStack != NULL &&
		 elektraKeyIsBelow (driver->tableArrayStack->key, driver->parentStack->key)) // below top name -> push new sub table array
	{
		driver->tableArrayStack = pushTableArray (driver->tableArrayStack, driver->parentStack->key);
	}
	else // no relation, pop table array stack until some relation exists (or NULL)
	{
		while (driver->tableArrayStack != NULL && elektraKeyCmp (driver->tableArrayStack->key, driver->parentStack->key) != 0)
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
	driver->order--;				       // Undo order increment

	ElektraKey * key = buildTableArrayKeyName (driver->tableArrayStack);
	ElektraKey * rootNameKey = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	elektraKeyAddName (rootNameKey, "..");
	ElektraKey * existingRoot = elektraKeysetLookup (driver->keys, rootNameKey, 0);

	if (existingRoot == NULL)
	{
		existingRoot = rootNameKey;
		elektraKeySetMeta (existingRoot, "tomltype", "tablearray");
		elektraKeySetMeta (existingRoot, "array", "#0");
		setOrderForKey (existingRoot, driver->order++);
		elektraKeysetAppendKey (driver->keys, existingRoot);
	}
	else
	{
		elektraKeyDel (rootNameKey);
		keyUpdateArrayMetakey (existingRoot, driver->tableArrayStack->currIndex);
	}

	driver->parentStack = pushParent (driver->parentStack, key);

	if (driverDrainCommentsToKey (driver->parentStack->key, driver))
	{ // we have to emit the array index key, because it has comments in previous lines
		elektraKeysetAppendKey (driver->keys, driver->parentStack->key);
	}
	driver->drainCommentsOnKeyExit = true; // only set to false while table array unindexed key is generated
}

void driverEnterArray (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	driver->indexStack = pushIndex (driver->indexStack, 0);
	const ElektraKey * meta = elektraKeyGetMeta (driver->parentStack->key, "array"); // check for nested arrays
	if (meta != NULL)
	{
		ELEKTRA_ASSERT (elektraStrCmp (elektraKeyString (meta), "") != 0,
				"Empty array index shouldn't be possible, we should've already called driverEnterArrayElement once");
		ElektraKey * key = keyAppendIndex (0, driver->parentStack->key);
		setOrderForKey (key, driver->order++);
		driver->parentStack = pushParent (driver->parentStack, key);
	}
	elektraKeySetMeta (driver->parentStack->key, "array", "");
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
	elektraKeysetAppendKey (driver->keys, driver->parentStack->key);
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

	ElektraKey * key = keyAppendIndex (driver->indexStack->value, driver->parentStack->key);
	// setOrderForKey (key, driver->order++); // TODO: no order for array elements

	elektraKeySetMeta (driver->parentStack->key, "array", elektraKeyBaseName (key));
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
		elektraKeyDecRef (driver->prevKey);
		elektraKeyDel (driver->prevKey);
	}
	driver->prevKey = driver->parentStack->key;
	elektraKeyIncRef (driver->prevKey);
	driver->parentStack = popParent (driver->parentStack);
}

void driverEnterInlineTable (Driver * driver)
{
	if (driver->errorSet)
	{
		return;
	}
	elektraKeySetMeta (driver->parentStack->key, "tomltype", "inlinetable");
	elektraKeysetAppendKey (driver->keys, driver->parentStack->key);
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
			driverNewCommentList (driver, NULL, NULL);
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
		driverNewCommentList (driver, comment->str, comment->orig);
	}
	else
	{
		driver->commentBack = commentListAdd (driver->commentBack, comment->str, comment->orig);
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

static void driverNewCommentList (Driver * driver, const char * comment, const char * orig)
{
	if (driver->commentRoot != NULL || driver->commentBack != NULL)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Wanted to create new comment list, but comment list already existing.");
	}
	driver->commentRoot = commentListNew (comment, orig);
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

static bool driverDrainCommentsToKey (ElektraKey * key, Driver * driver)
{
	if (driver->newlineCount > 0)
	{
		if (driver->commentRoot == NULL)
		{
			driverNewCommentList (driver, NULL, NULL);
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
	bool drainedComments = driver->commentRoot != NULL;
	driverClearCommentList (driver);
	return drainedComments;
}

static void pushCurrKey (Driver * driver)
{
	driver->parentStack = pushParent (driver->parentStack, driver->currKey);
}

static void setCurrKey (Driver * driver, const ElektraKey * key)
{
	if (driver->currKey != NULL)
	{
		elektraKeyDecRef (driver->currKey);
		elektraKeyDel (driver->currKey);
	}
	if (key != NULL)
	{
		driver->currKey = elektraKeyNew (elektraKeyName (key), ELEKTRA_KEY_END);
		elektraKeyIncRef (driver->currKey);
	}
	else
	{
		driver->currKey = NULL;
	}
}

static void setPrevKey (Driver * driver, ElektraKey * key)
{
	if (driver->prevKey != NULL)
	{
		elektraKeyDecRef (driver->prevKey);
		elektraKeyDel (driver->prevKey);
	}
	driver->prevKey = key;
	if (key != NULL)
	{
		elektraKeyIncRef (key);
	}
}

static void resetCurrKey (Driver * driver)
{
	setCurrKey (driver, driver->parentStack->key);
}

static void extendCurrKey (Driver * driver, const char * name)
{
	ELEKTRA_ASSERT (name != NULL, "Name extension must not be NULL, but was");
	if (driver->currKey == NULL)
	{
		driverError (driver, ERROR_INTERNAL, 0, "Wanted to extend current key, but current key is NULL.");
		return;
	}
	elektraKeyAddBaseName (driver->currKey, name);
}

static ParentList * pushParent (ParentList * top, ElektraKey * key)
{
	ParentList * parent = elektraCalloc (sizeof (ParentList));
	parent->key = key;
	elektraKeyIncRef (key);
	parent->next = top;
	return parent;
}

static ParentList * popParent (ParentList * top)
{
	ParentList * newTop = top->next;
	elektraKeyDecRef (top->key);
	elektraKeyDel (top->key);
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

	elektraKeySetString (driver->parentStack->key, elektraStr);

	switch (driver->lastScalar->type)
	{
	case SCALAR_STRING_BASIC:
	case SCALAR_STRING_LITERAL:
	case SCALAR_STRING_ML_BASIC:
	case SCALAR_STRING_ML_LITERAL:
		if (!handleSpecialStrings (elektraStr, driver->parentStack->key))
		{
			assignStringMetakeys (driver->parentStack->key, driver->lastScalar->orig, elektraStr, driver);
		}
		assignStringTomlType (driver->parentStack->key, driver->lastScalar->type);
		break;
	case SCALAR_BOOLEAN:
		elektraKeySetMeta (driver->parentStack->key, "type", "boolean");
		break;
	case SCALAR_FLOAT_NUM:
	case SCALAR_FLOAT_INF:
	case SCALAR_FLOAT_POS_INF:
	case SCALAR_FLOAT_NEG_INF:
	case SCALAR_FLOAT_NAN:
	case SCALAR_FLOAT_POS_NAN:
	case SCALAR_FLOAT_NEG_NAN:
		elektraKeySetMeta (driver->parentStack->key, "type", "double");
		assignOrigValueIfDifferent (driver->parentStack->key, driver->lastScalar->orig);
		break;
	case SCALAR_INTEGER_DEC:
		elektraKeySetMeta (driver->parentStack->key, "type", "long_long");
		assignOrigValueIfDifferent (driver->parentStack->key, driver->lastScalar->orig);
		break;
	case SCALAR_INTEGER_BIN:
	case SCALAR_INTEGER_OCT:
	case SCALAR_INTEGER_HEX:
		elektraKeySetMeta (driver->parentStack->key, "type", "unsigned_long_long");
		assignOrigValueIfDifferent (driver->parentStack->key, driver->lastScalar->orig);
		break;
	default:
		assignOrigValueIfDifferent (driver->parentStack->key, driver->lastScalar->orig);
		break;
	}

	elektraFree (elektraStr);

	elektraKeysetAppendKey (driver->keys, driver->parentStack->key);
	driverClearLastScalar (driver);
}

static void assignOrigValueIfDifferent (ElektraKey * key, const char * origValue)
{
	if (elektraStrCmp (elektraKeyString (key), origValue) != 0)
	{
		elektraKeySetMeta (key, "origvalue", origValue);
	}
}


// handles base64 encoded or null-indicator strings
static bool handleSpecialStrings (const char * string, ElektraKey * key)
{
	if (isNullString (string))
	{
		elektraKeySetBinary (key, NULL, 0);
		return true;
	}
	else if (isBase64String (string))
	{
		return true;
	}
	else
	{
		return false;
	}
}

static void assignStringMetakeys (ElektraKey * key, const char * origStr, const char * translatedStr, Driver * driver)
{
	const ElektraKey * metaType = elektraKeyGetMeta (key, "type");
	// Don't overwrite "binary" typed metakeys -> See base64 plugin meta mode
	// Don't assign it empty strings, otherwise the type plugin complains
	// TODO (kodebach): string length 0, once type allows zero length on type=string
	if ((metaType == NULL || elektraStrCmp (elektraKeyString (metaType), "binary") != 0) && elektraStrLen (translatedStr) > 1)
	{
		elektraKeySetMeta (key, "type", "string");
	}
	if (strcmp (origStr, translatedStr) != 0)
	{
		char * orig = elektraStrDup (origStr);
		if (orig == NULL)
		{
			driverError (driver, ERROR_MEMORY, 0, "Could not allocate memory");
			return;
		}
		elektraKeySetMeta (key, "origvalue", orig);
		elektraFree (orig);
	}
}

static void assignStringTomlType (ElektraKey * key, ScalarType stringType)
{
	switch (stringType)
	{
	case SCALAR_STRING_BASIC:
		elektraKeySetMeta (key, "tomltype", "string_basic");
		break;
	case SCALAR_STRING_ML_BASIC:
		elektraKeySetMeta (key, "tomltype", "string_ml_basic");
		break;
	case SCALAR_STRING_LITERAL:
		elektraKeySetMeta (key, "tomltype", "string_literal");
		break;
	case SCALAR_STRING_ML_LITERAL:
		elektraKeySetMeta (key, "tomltype", "string_ml_literal");
		break;
	default:
		ELEKTRA_ASSERT (0, "Not a valid string type %d", stringType);
	}
}

static void driverClearLastScalar (Driver * driver)
{
	freeScalar (driver->lastScalar);
	driver->lastScalar = NULL;
}

int yyerror (Driver * driver, const char * msg)
{
	driverError (driver, ERROR_SYNTACTIC, yylineno, "%s", msg);
	return 0;
}

void driverError (Driver * driver, int err, int lineno, const char * format, ...)
{
	driver->errorSet = true;

	if (err == ERROR_MEMORY)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (driver->root);
		return;
	}

	char * msg;
	va_list args;
	va_start (args, format);
	msg = elektraVFormat (format, args);
	va_end (args);

	switch (err)
	{
	case ERROR_INTERNAL:
		ELEKTRA_SET_INTERNAL_ERRORF (driver->root, "Line %d~(%d:%d-%d:%d): %s", lineno, yylloc.first_line, yylloc.first_column,
					     yylloc.last_line, yylloc.last_column - 1, msg);
		break;
	case ERROR_SYNTACTIC:
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (driver->root, "Line %d~(%d:%d-%d:%d): %s", lineno, yylloc.first_line,
							 yylloc.first_column, yylloc.last_line, yylloc.last_column - 1, msg);
		break;
	case ERROR_SEMANTIC:
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (driver->root, "Line %d~(%d:%d-%d:%d): %s", lineno, yylloc.first_line,
							yylloc.first_column, yylloc.last_line, yylloc.last_column - 1, msg);
		break;
	default:
		ELEKTRA_SET_INTERNAL_ERRORF (driver->root, "Line %d~(%d:%d-%d:%d): %s", lineno, yylloc.first_line, yylloc.first_column,
					     yylloc.last_line, yylloc.last_column - 1, msg);
		break;
	}

	elektraFree (msg);
}

void driverErrorGeneric (Driver * driver, int err, const char * caller, const char * callee)
{
	driver->errorSet = true;

	switch (err)
	{
	case ERROR_INTERNAL:
		ELEKTRA_SET_INTERNAL_ERRORF (driver->root, "%s: Error during call of %s", caller, callee);
		break;
	case ERROR_MEMORY:
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (driver->root);
		break;
	case ERROR_SYNTACTIC:
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (driver->root, "%s: Error during call of %s", caller, callee);
		break;
	case ERROR_SEMANTIC:
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (driver->root, "%s: Error during call of %s", caller, callee);
		break;
	default:
		ELEKTRA_SET_INTERNAL_ERRORF (driver->root, "%s: Error during call of %s", caller, callee);
		break;
	}
}
