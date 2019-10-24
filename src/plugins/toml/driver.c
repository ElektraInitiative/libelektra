#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>

#include <kdb.h>
#include <kdbhelper.h>

#include "driver.h"
#include "parser.h"

extern int yyparse (Driver * driver);
extern FILE * yyin;

// Function declarations
static void pushCurrKey (Driver * driver);
static void setCurrKey (Driver * driver, const Key * parent);
static void resetCurrKey (Driver * driver);
static void extendCurrKey (Driver * driver, const char * name);
static char * getChildFraction (const Key * parent, const Key * child);
static Key * buildTableArrayKeyName (const TableArray * ta);
static TableArray * pushTableArray (TableArray * top, Key * key);
static TableArray * popTableArray (TableArray * top);
static ParentList * pushParent (ParentList * top, Key * key);
static ParentList * popParent (ParentList * top);
static IndexList * pushIndex (IndexList * top, int value);
static IndexList * popIndex (IndexList * top);
static void drainCommentsToKey (Driver * driver, Key * key);
static void firstCommentAsInlineToPrevKey (Driver * driver);
static void newlinesToCommentList (Driver * driver);
static void addCommentListToKey (Key * key, CommentList * root);
static void addInlineCommentToKey (Key * key, CommentList * root);
static void freeCommentList (CommentList * root);
static void addCommentToKey (Key * key, const char * commentStr, size_t index, size_t spaces);
static void addComment (Driver * driver, const char * comment, size_t spaceCount);
static CommentList * appendComment (CommentList * back, const char * comment, int spaces);
static void lastScalarToParentKey (Driver * driver);
static Key * indexToKey (size_t index, const Key * parent);
static char * indexToArrayString (size_t index);
static char * intToStr (size_t i);
static void setOrderForKey (Key * key, size_t order);
static void setPlainIntMeta (Key * key, const char * metaKeyName, size_t value);

Driver * createDriver (const Key * parent)
{
	Driver * driver = elektraMalloc (sizeof (Driver));
	memset (driver, 0, sizeof (Driver));
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
	char * msg = elektraMalloc (256);
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
	drainCommentsToKey (driver, driver->root);
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
		bool isTableArray = false;
		for (const Key * meta = keyNextMeta (existing); meta != NULL; meta = keyNextMeta (existing))
		{
			if (strcmp (keyName (meta), "type") == 0 && strcmp (keyString (meta), "tablearray") == 0)
			{
				isTableArray = true;
				break;
			}
		}
		if (!isTableArray)
		{
			// Only allow table array keys to be read multiple times
			driverError (driver, driver->currLine, "Malformed input: Multiple occurences of keyname: '%s'", keyName (existing));
		}
	}

	pushCurrKey (driver);
	if (driver->drainCommentsOnKeyExit)
	{
		drainCommentsToKey (driver, driver->parentStack->key);
	}
	setOrderForKey (driver->parentStack->key, driver->order++);
}

void driverExitKeyValue (Driver * driver)
{
	lastScalarToParentKey (driver);

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
		addInlineCommentToKey (driver->prevKey, driver->commentRoot);
		freeCommentList (driver->commentRoot);
		driver->commentRoot = NULL;
	}
}

void driverExitOptCommentTable (Driver * driver)
{
	if (driver->commentRoot != NULL)
	{
		assert (driver->commentRoot->next == NULL);
		assert (driver->prevKey != NULL);
		addInlineCommentToKey (driver->parentStack->key, driver->commentRoot);
		freeCommentList (driver->commentRoot);
		driver->commentRoot = NULL;

		// We need to emit the table array key ending with /#n, no sub keys
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

	drainCommentsToKey (driver, driver->parentStack->key);
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
	drainCommentsToKey (driver, NULL);

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

	Key * key = keyNew (keyName (driver->parentStack->key), KEY_END);

	char * indexStr = indexToArrayString (driver->indexStack->value);
	keyAddBaseName (key, indexStr);
	elektraFree (indexStr);

	keySetMeta (driver->parentStack->key, "array", keyBaseName (key));
	driver->parentStack = pushParent (driver->parentStack, key);

	driver->indexStack->value++;

	drainCommentsToKey (driver, driver->parentStack->key);
}

void driverExitArrayElement (Driver * driver)
{
	assert (driver->lastScalar != NULL);
	driverEnterArrayElement (driver);
	lastScalarToParentKey (driver);

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
	if (driver->lastScalar != NULL)
	{
		elektraFree (driver->lastScalar->str);
		elektraFree (driver->lastScalar);
		driver->lastScalar = NULL;
	}
}

void driverEmptyInlineTable (Driver * driver)
{
	driverEnterInlineTable (driver);
	// Don't need to call exit, because no scalar value emission possible in empty inline table
}

void driverExitComment (Driver * driver, const Scalar * comment)
{
	newlinesToCommentList (driver);
	addComment (driver, comment->str, driver->spaceCount);
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

static void drainCommentsToKey (Driver * driver, Key * key)
{
	newlinesToCommentList (driver);
	if (key != NULL)
	{
		addCommentListToKey (key, driver->commentRoot);
	}
	else
	{
		// printf ("WARNING: Draining comments to NULL\n");
	}
	freeCommentList (driver->commentRoot);
	driver->commentRoot = NULL;
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
		addInlineCommentToKey (driver->prevKey, comment);
		freeCommentList (comment);
	}
}

static void newlinesToCommentList (Driver * driver)
{
	while (driver->newlineCount > 0)
	{
		addComment (driver, NULL, 0);
		driver->newlineCount--;
	}
}

static void addInlineCommentToKey (Key * key, CommentList * root)
{
	assert (root->next == NULL); // there is only 1 inline comment possible
	addCommentToKey (key, root->comment, 0, root->spaces);
}

static void addCommentListToKey (Key * key, CommentList * root)
{
	int index = 1;
	while (root != NULL)
	{
		addCommentToKey (key, root->comment, index++, root->spaces);
		root = root->next;
	}
}

static void addCommentToKey (Key * key, const char * commentStr, size_t index, size_t spaces)
{
	// printf ("ADD COMMENT TO KEY: key = '%s', comment = '%s', index = %d, spaces = %d\n", keyName (key), commentStr, index, spaces);

	// add comment str
	char * indexStr = indexToArrayString ((size_t) index);
	size_t metaLen = strlen (indexStr) + 9;
	char * metaName = elektraCalloc (sizeof (char) * metaLen);
	snprintf (metaName, metaLen, "comment/%s", indexStr);
	elektraFree (indexStr);
	if (commentStr != NULL)
	{
		keySetMeta (key, metaName, commentStr);
	}

	// add start symbol
	size_t metaInfoLen = metaLen + 6;
	char * metaInfoName = elektraCalloc (sizeof (char) * metaInfoLen);
	snprintf (metaInfoName, metaInfoLen, "%s/start", metaName);
	if (commentStr != NULL)
	{
		keySetMeta (key, metaInfoName, "#");
	}
	else
	{
		keySetMeta (key, metaInfoName, "");
	}

	// add space count
	snprintf (metaInfoName, metaInfoLen, "%s/space", metaName);
	setPlainIntMeta (key, metaInfoName, spaces);

	elektraFree (metaInfoName);
	elektraFree (metaName);
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

static char * getChildFraction (const Key * parent, const Key * child)
{
	// printf ("Determining child fraction of:\n\t%s\n\t%s\n", keyName (parent), keyName (child));
	if (!keyIsBelow (parent, child))
	{
		return NULL;
	}
	else
	{
		Key * childDup = keyDup (child);
		size_t fracSize = 256;
		char * fraction = elektraMalloc (sizeof (char) * fracSize);
		memset (fraction, 0, sizeof (char) * fracSize);
		do
		{
			const char * baseName = keyBaseName (childDup);
			if (strlen (fraction) + strlen (baseName) >= fracSize)
			{
				fracSize *= 2;
				elektraRealloc ((void **) &fraction, fracSize);
			}
			char * fracDup = strdup (fraction); // TODO: avoid allocation
			snprintf (fraction, fracSize, "%s/%s", baseName, fracDup);
			elektraFree (fracDup);
			keyAddName (childDup, "..");
		} while (keyRel (parent, childDup) != 0);
		fraction[strlen (fraction) - 1] = 0;
		elektraRealloc ((void **) &fraction, strlen (fraction) + 1);
		keyDel (childDup);
		// printf ("got fraction: %s\n", fraction);
		return fraction;
	}
}

static TableArray * pushTableArray (TableArray * top, Key * key)
{
	TableArray * ta = elektraCalloc (sizeof (TableArray));
	ta->key = key;
	keyIncRef (key);
	if (top != NULL)
	{
		ta->keyStr = getChildFraction (top->key, key);
	}
	if (ta->keyStr == NULL)
	{
		ta->keyStr = strdup (keyName (key));
	}
	ta->currIndex = 0;
	ta->next = top;

	return ta;
}

static TableArray * popTableArray (TableArray * top)
{
	TableArray * newTop = top->next;
	keyDecRef (top->key);
	keyDel (top->key);
	elektraFree (top->keyStr);
	elektraFree (top);
	return newTop;
}

static Key * buildTableArrayKeyName (const TableArray * ta)
{
	if (ta->next == NULL || !keyIsBelow (ta->next->key, ta->key))
	{
		return indexToKey (ta->currIndex, ta->key);
	}
	else
	{
		Key * key = buildTableArrayKeyName (ta->next);
		keyAddName (key, ta->keyStr);
		char * index = indexToArrayString (ta->currIndex);
		keyAddBaseName (key, index);
		elektraFree (index);
		return key;
	}
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

static void addComment (Driver * driver, const char * comment, size_t spaceCount)
{
	// printf ("STORING comment: '%s'\n", comment);
	if (driver->commentRoot == NULL)
	{
		driver->commentRoot = elektraCalloc (sizeof (CommentList));
		if (comment != NULL)
		{
			driver->commentRoot->comment = strdup (comment);
		}
		driver->commentRoot->spaces = driver->spaceCount;
		driver->commentRoot->next = NULL;
		driver->commentBack = driver->commentRoot;
	}
	else
	{
		driver->commentBack = appendComment (driver->commentBack, comment, spaceCount);
	}
}

static CommentList * appendComment (CommentList * back, const char * comment, int spaces)
{
	back->next = elektraCalloc (sizeof (CommentList));
	if (comment != NULL)
	{
		back->next->comment = strdup (comment);
	}
	back->next->spaces = spaces;
	back->next->next = NULL;
	return back->next;
}

static void freeCommentList (CommentList * root)
{
	while (root != NULL)
	{
		CommentList * nextComment = root->next;
		elektraFree (root->comment);
		elektraFree (root);
		root = nextComment;
	}
}

static void lastScalarToParentKey (Driver * driver)
{
	if (driver->lastScalar != NULL)
	{
		assert (driver->parentStack != NULL);
		// printf ("COMMIT %s -> %s\n", keyName (driver->parentStack->key), driver->lastScalar->str);
		keySetString (driver->parentStack->key, driver->lastScalar->str);
		ksAppendKey (driver->keys, driver->parentStack->key);
		elektraFree (driver->lastScalar->str);
		elektraFree (driver->lastScalar);
		driver->lastScalar = NULL;
	}
}

static Key * indexToKey (size_t index, const Key * parent)
{
	// Key * indexKey = keyDup (parent);
	Key * indexKey = keyNew (keyName (parent), KEY_END);

	char * indexStr = indexToArrayString (index);
	keyAddBaseName (indexKey, indexStr);
	elektraFree (indexStr);
	return indexKey;
}

static char * indexToArrayString (size_t index)
{
	size_t digits = 1;
	for (size_t value = index; value > 9; digits++)
	{
		value /= 10;
	}
	int strLen = 1 +	    //  '#'
		     (digits - 1) + // underscores
		     digits +       // actual digits
		     1;		    // '\0'
	char * str = elektraCalloc (sizeof (char) * strLen);
	memset (str, '_', sizeof (char) * strLen);
	str[0] = '#';
	str[strLen - 1] = 0;
	snprintf (str + 1 + (digits - 1), strLen, "%lu", index);
	return str;
}

static void setPlainIntMeta (Key * key, const char * metaKeyName, size_t value)
{
	char * str = intToStr (value);
	keySetMeta (key, metaKeyName, str);
	elektraFree (str);
}

static char * intToStr (size_t i)
{
	char * str = (char *) elektraMalloc (sizeof (char) * 40);
	snprintf (str, 40, "%lu", i);
	return str;
}

static void setOrderForKey (Key * key, size_t order)
{
	setPlainIntMeta (key, "order", order);
}
