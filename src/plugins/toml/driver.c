#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>

#include <kdb.h>
#include <kdbhelper.h>

#include "driver.h"
#include "parser.h"
#include "utility.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

// Function declarations
static void driverNewCommentList (Driver * driver, const char * comment, size_t spaceCount);
static void driverClearCommentList (Driver * driver);
static void driverDrainCommentsToKey (Key * key, Driver * driver);
static void firstCommentAsInlineToPrevKey (Driver * driver);
static void driverCommitLastScalarToParentKey (Driver * driver);
static void driverClearLastScalar (Driver * driver);

static void pushCurrKey (Driver * driver);
static void setCurrKey (Driver * driver, const Key * parent);
static void resetCurrKey (Driver * driver);
static void extendCurrKey (Driver * driver, const char * name);
static ParentList * pushParent (ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);

Driver * createDriver (const Key * parent)
{
	Driver * driver = (Driver *) elektraCalloc (sizeof (Driver));
	driver->root = keyDup (parent);
	driver->parentStack = pushParent (NULL, keyDup (parent));
	driver->filename = keyString (parent);
	driver->tableActive = false;
	driver->drainCommentsOnKeyExit = true;
	return driver;
}

int driverParse (Driver * driver, KeySet * returned)
{
	driver->keys = returned;
	driver->file = fopen (driver->filename, "rb");
	if (driver->file == NULL)
	{
		driverError (driver, 0, "Could not open file: '%s'", driver->filename);
		return 1;
	}
	yyin = driver->file;
	ksAppendKey (driver->keys, driver->root);
	int yyResult = yyparse (driver);
	return yyResult || driver->lastError != NULL;
}

void driverError (Driver * driver, int lineno, const char * format, ...)
{
	va_list args;
	va_start (args, format);
	char * msg = elektraCalloc (256);
	// TODO: proper error handling
	if (lineno > 0)
	{
		snprintf (msg, 256, "Line ~%d: ", lineno);
		size_t len = strlen (msg);
		assert (len <= 256);
		vsnprintf (msg + len, 256 - len, format, args);
	}
	else
	{
		vsnprintf (msg, 256, format, args);
	}
	va_end (args);
	if (driver->lastError != NULL)
	{
		elektraFree (driver->lastError);
	}
	driver->lastError = msg;
	printf ("[ERROR] %s\n", msg);
}

void driverExitToml (Driver * driver)
{
	driverDrainCommentsToKey (driver->root, driver);
}

void driverEnterKey (Driver * driver)
{
	resetCurrKey (driver);
}

void driverExitKey (Driver * driver)
{
	Key * existing = ksLookup (driver->keys, driver->currKey, 0);
	if (existing != NULL)
	{
		keyRewindMeta (existing);
		bool isTableArrayList = false;
		for (const Key * meta = keyNextMeta (existing); meta != NULL; meta = keyNextMeta (existing))
		{
			if (strcmp (keyName (meta), "type") == 0 && strcmp (keyString (meta), "tablearray") == 0)
			{
				isTableArrayList = true;
				break;
			}
		}
		if (!isTableArrayList)
		{
			// Only allow table array keys to be read multiple times
			driverError (driver, driver->currLine, "Malformed input: Multiple occurences of keyname: '%s'", keyName (existing));
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
	driverCommitLastScalarToParentKey (driver);

	if (driver->prevKey != NULL)
	{
		keyDecRef (driver->prevKey);
		keyDel (driver->prevKey);
		driver->prevKey = NULL;
	}
	driver->prevKey = driver->parentStack->key;
	keyIncRef (driver->prevKey);

	driver->parentStack = popParent (driver->parentStack);
}

void driverExitOptCommentKeyPair (Driver * driver)
{
	if (driver->commentRoot != NULL)
	{
		assert (driver->prevKey != NULL);
		assert (driver->commentRoot->next == NULL);
		keyAddInlineComment (driver->prevKey, driver->commentRoot);
		driverClearCommentList (driver);
	}
}

void driverExitOptCommentTable (Driver * driver)
{
	if (driver->commentRoot != NULL)
	{
		assert (driver->commentRoot->next == NULL);
		assert (driver->prevKey != NULL);
		keyAddInlineComment (driver->parentStack->key, driver->commentRoot);
		driverClearCommentList (driver);

		// We need to emit the table array key ending with /#n (having no value)
		// Otherwise, inline comments on empty table arrays will get ignored
		ksAppendKey (driver->keys, driver->parentStack->key);
	}
}

void driverExitSimpleKey (Driver * driver, const Scalar * name)
{
	extendCurrKey (driver, name->str);
	driver->currLine = name->line;
}

void driverExitScalar (Driver * driver, Scalar * scalar)
{
	if (driver->lastScalar != NULL)
	{
		elektraFree (driver->lastScalar->str);
		elektraFree (driver->lastScalar);
	}
	driver->lastScalar = scalar;
	// keySetString (driver->parentStack->key, scalar->str);
	// ksAppendKey (driver->keys, driver->parentStack->key);
	driver->currLine = scalar->line;
	// printf ("Added Scalar: %s -> %s\n", keyName (driver->parentStack->key), keyString (driver->parentStack->key));
}

void driverEnterSimpleTable (Driver * driver)
{
	if (driver->tableActive)
	{
		driver->parentStack = popParent (driver->parentStack);
	}
	else
	{
		driver->tableActive = true;
	}
	resetCurrKey (driver);
}

void driverExitSimpleTable (Driver * driver)
{
	keySetMeta (driver->parentStack->key, "type", "simpletable");
	ksAppendKey (driver->keys, driver->parentStack->key);
}

void driverEnterTableArray (Driver * driver)
{
	if (driver->tableActive)
	{
		driver->parentStack = popParent (driver->parentStack);
		driver->tableActive = false;
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
	driver->order--;				       // Undo order increment, which is done after each key name exit

	Key * key = buildTableArrayKeyName (driver->tableArrayStack);

	char * indexStr = indexToArrayString (driver->tableArrayStack->currIndex);
	Key * rootNameKey = keyDup (key);
	keyAddName (rootNameKey, "..");
	Key * existingRoot = ksLookup (driver->keys, rootNameKey, 0);
	if (existingRoot == NULL)
	{
		existingRoot = rootNameKey;
		keySetMeta (existingRoot, "type", "tablearray");
		setOrderForKey (existingRoot, driver->order++);
		ksAppendKey (driver->keys, existingRoot);
	}
	else
	{
		keyDel (rootNameKey);
	}
	keySetMeta (existingRoot, "array", indexStr);
	elektraFree (indexStr);

	driver->parentStack = pushParent (driver->parentStack, key);

	driverDrainCommentsToKey (driver->parentStack->key, driver);
	driver->drainCommentsOnKeyExit = true; // only set to false while table array unindexed key is generated
}

void driverEnterArray (Driver * driver)
{
	driver->indexStack = pushIndex (driver->indexStack, 0);
	keySetMeta (driver->parentStack->key, "array", "");
}

void driverExitArray (Driver * driver)
{
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
	driverEnterArray (driver);
	driverExitArray (driver);
}

void driverEnterArrayElement (Driver * driver)
{
	if (driver->indexStack->value == SIZE_MAX)
	{
		driverError (driver, 0, "Array index at maximum range of size_t: SIZE_MAX");
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
	assert (driver->lastScalar != NULL);
	driverEnterArrayElement (driver);
	driverCommitLastScalarToParentKey (driver);

	driver->prevKey = driver->parentStack->key;
	keyIncRef (driver->prevKey);
	driver->parentStack = popParent (driver->parentStack);
}

void driverEnterInlineTable (Driver * driver)
{
	keySetMeta (driver->parentStack->key, "type", "inlinetable");
	ksAppendKey (driver->keys, driver->parentStack->key);
}

void driverExitInlineTable (Driver * driver)
{
	driverClearLastScalar (driver);
}

void driverEmptyInlineTable (Driver * driver)
{
	driverEnterInlineTable (driver);
	// Don't need to call exit, because no scalar value emission possible in empty inline table
}

void driverExitComment (Driver * driver, const Scalar * comment)
{
	if (driver->newlineCount > 0)
	{
		if (driver->commentRoot == NULL)
		{
			driverNewCommentList (driver, NULL, 0);
			driver->newlineCount--;
		}
		driver->commentBack = commentListAddNewlines (driver->commentBack, driver->newlineCount);
		driver->newlineCount = 0;
	}

	if (driver->commentRoot == NULL)
	{
		driverNewCommentList (driver, comment->str, driver->spaceCount);
	}
	else
	{
		driver->commentBack = commentListAdd (driver->commentBack, comment->str, driver->spaceCount);
	}
	driver->spaceCount = 0;

	driver->currLine = comment->line;
}

// TODO: handle spaces
void driverExitSpace (Driver * driver)
{
	if (driver->spaceCount == SIZE_MAX)
	{
		driverError (driver, 0, "Space counter at maximum range of size_t: SIZE_MAX");
		return;
	}
	driver->spaceCount++;
}

void driverExitNewline (Driver * driver)
{
	if (driver->newlineCount == SIZE_MAX)
	{
		driverError (driver, 0, "Newline counter at maximum range of size_t: SIZE_MAX");
		return;
	}
	driver->newlineCount++;
}


static void driverNewCommentList (Driver * driver, const char * comment, size_t spaceCount)
{
	assert (driver->commentRoot == NULL);
	assert (driver->commentBack == NULL);
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
			assert (driver->commentBack == driver->commentRoot);
			driver->commentRoot = NULL;
			driver->commentBack = NULL;
		}
		else
		{
			driver->commentRoot = driver->commentRoot->next;
			comment->next = NULL;
		}
		keyAddInlineComment (driver->prevKey, comment);
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
		commentListAddNewlines (driver->commentBack, driver->newlineCount);
		driver->newlineCount = 0;
	}

	if (key != NULL)
	{
		keyAddCommentList (key, driver->commentRoot);
	}
	driverClearCommentList (driver);
}

static void pushCurrKey (Driver * driver)
{
	driver->parentStack = pushParent (driver->parentStack, driver->currKey);
}

static void setCurrKey (Driver * driver, const Key * parent)
{
	assert (parent != NULL);
	if (driver->currKey != NULL)
	{
		keyDecRef (driver->currKey);
		keyDel (driver->currKey);
	}
	driver->currKey = keyNew (keyName (parent), KEY_END);
	keyIncRef (driver->currKey);
	// keyClear (driver->currKey);
}

static void resetCurrKey (Driver * driver)
{
	setCurrKey (driver, driver->parentStack->key);
}

static void extendCurrKey (Driver * driver, const char * name)
{
	assert (driver->currKey != NULL);
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
	if (driver->lastScalar != NULL)
	{
		assert (driver->parentStack != NULL);
		// printf ("COMMIT %s -> %s\n", keyName (driver->parentStack->key), driver->lastScalar->str);
		keySetString (driver->parentStack->key, driver->lastScalar->str);
		ksAppendKey (driver->keys, driver->parentStack->key);
		driverClearLastScalar (driver);
	}
}

static void driverClearLastScalar (Driver * driver)
{
	if (driver->lastScalar != NULL) elektraFree (driver->lastScalar->str);
	elektraFree (driver->lastScalar);
	driver->lastScalar = NULL;
}
