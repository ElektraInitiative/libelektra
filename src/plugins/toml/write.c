#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbmeta.h>
#include <kdbproposal.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "error.h"
#include "sort.h"
#include "type.h"
#include "utility.h"
#include "write.h"

typedef enum
{
	KEY_TYPE_ASSIGNMENT,
	KEY_TYPE_SIMPLE_TABLE,
	KEY_TYPE_TABLE_ARRAY
} KeyType;

typedef struct
{
	char * filename;
	FILE * f;
	Key * rootKey;
	Key ** keyArray;
	size_t arraySize;
	size_t cursor;
	TypeChecker * checker;
	bool errorSet;
} Writer;

typedef struct CommentList_
{
	size_t index;
	const char * content;
	char start;
	size_t spaces;
	struct CommentList_ * next;
} CommentList;

typedef struct ArrayInfo_
{
	Key * name;
	size_t maxIndex;
	struct ArrayInfo_ * next;
} ArrayInfo;


static Writer * createWriter (Key * rootKey, Key ** keyArray, size_t arraySize);
static void destroyWriter (Writer * writer);
static int writeKeys (Key * parent, Writer * writer);
static int writeAssignment (Key * parent, Key * key, Writer * writer);
static int writeSimpleTable (Key * parent, Key * key, Writer * writer);
static int writeTableArray (Key * parent, Key * key, Writer * writer);
static int writeInlineTableBody (Key * key, Writer * writer);
static int writeInlineTableElements (Key * parent, Writer * writer);
static int writeArrayBody (Key * key, Writer * writer);
static int writeArrayElements (Key * parent, Writer * writer);
static int writeValue (Key * key, Writer * writer);
static int writeScalar (Key * key, Writer * writer);
static int writeQuoted (const char * value, char quoteChar, int quouteCount, Writer * writer);
static int writeRelativeKeyName (Key * parent, Key * key, Writer * writer);
static int writeTableArrayHeader (Key * parent, Key * root, Key * key, Writer * writer);
static int writeSimpleTableHeader (Key * parent, Key * key, Writer * writer);
static int writePrecedingComments (const CommentList * commentList, Writer * writer);
static int writeInlineComment (const CommentList * commentList, Writer * writer);
static int writeComment (const CommentList * comment, Writer * writer);
static int writeNewline (Writer * writer);
static CommentList * collectComments (Key * key, Writer * writer);
static void freeComments (CommentList * comments);
static KeyType getKeyType (Key * key);
static bool isMultilineString (const char * str);
static bool isTrue (const char * boolStr);
static void addMissingArrayKeys (KeySet * keys, Key * parent);
static void pruneInvalidArrayKeys (KeySet * keys);
static ArrayInfo * updateArrayInfo (ArrayInfo * root, Key * name, size_t index);
static int keyCmpOrderWrapper (const void * a, const void * b);
static Key * getCurrentKey (Writer * writer);
static Key * getNextKey (Writer * writer);
static void writerError (Writer * writer, int err, const char * format, ...);

int tomlWrite (KeySet * keys, Key * parent)
{
	addMissingArrayKeys (keys, parent);
	pruneInvalidArrayKeys (keys);
	ksRewind (keys);

	size_t arraySize = ksGetSize (keys);
	Key ** keyArray = sortKeySet (keys);
	if (keyArray == NULL)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parent);
		return 1;
	}

	printf ("+++ DUMP BEFORE WRITE +++\n");
	dumpMemKS (keyArray, arraySize);

	Writer * w = createWriter (parent, keyArray, arraySize);
	if (w == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parent, keyString (parent));
		return 1;
	}

	int result = 0;
	if (keyCmp (getCurrentKey (w), parent) == 0)
	{
		getNextKey (w);
	}
	result |= writeKeys (parent, w);
	CommentList * comments = collectComments (parent, w);
	writePrecedingComments (comments, w);
	freeComments (comments);

	destroyWriter (w);
	ksRewind (keys);
	return result;
}

static Writer * createWriter (Key * rootKey, Key ** keyArray, size_t arraySize)
{
	Writer * writer = elektraCalloc (sizeof (Writer));
	if (writer == NULL)
	{
		return NULL;
	}
	writer->filename = elektraStrDup (keyString (rootKey));
	if (writer->filename == NULL)
	{
		destroyWriter (writer);
		return NULL;
	}
	ELEKTRA_LOG_DEBUG ("Writing file %s\n", writer->filename);
	writer->f = fopen (writer->filename, "w");
	if (writer->f == NULL)
	{
		destroyWriter (writer);
		return NULL;
	}
	writer->rootKey = rootKey;
	writer->keyArray = keyArray;
	writer->arraySize = arraySize;
	writer->cursor = 0;
	writer->errorSet = false;
	writer->checker = createTypeChecker ();
	if (writer->checker == NULL)
	{
		destroyWriter (writer);
		return NULL;
	}

	return writer;
}

static void destroyWriter (Writer * writer)
{
	if (writer != NULL)
	{
		if (writer->filename != NULL)
		{
			elektraFree (writer->filename);
			writer->filename = NULL;
		}
		if (writer->f != NULL)
		{
			fclose (writer->f);
			writer->f = NULL;
		}
		if (writer->keyArray != NULL)
		{
			elektraFree (writer->keyArray);
		}
		destroyTypeChecker (writer->checker);
	}
}

static Key * getCurrentKey (Writer * writer)
{
	if (writer->cursor >= writer->arraySize)
	{
		return NULL;
	}
	return writer->keyArray[writer->cursor];
}

static Key * getNextKey (Writer * writer)
{
	writer->cursor++;
	return getCurrentKey (writer);
}

static int writeKeys (Key * parent, Writer * writer)
{
	Key * key = getCurrentKey (writer);

	int result = 0;

	while (result == 0 && key != NULL && keyIsBelow (parent, key) == 1)
	{
		switch (getKeyType (key))
		{
		case KEY_TYPE_ASSIGNMENT:
			result |= writeAssignment (parent, key, writer);
			result |= writeNewline (writer);
			break;
		case KEY_TYPE_SIMPLE_TABLE:
			if (keyCmp (parent, writer->rootKey) != 0)
			{
				return result;
			}
			result |= writeSimpleTable (parent, key, writer);
			break;
		case KEY_TYPE_TABLE_ARRAY:
			if (keyCmp (parent, writer->rootKey) != 0)
			{
				return result;
			}
			result |= writeTableArray (parent, key, writer);
			break;
		}
		key = getCurrentKey (writer);
	}
	return result;
}

static int writeAssignment (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	CommentList * comments = collectComments (key, writer);
	result |= writePrecedingComments (comments, writer);
	result |= writeRelativeKeyName (parent, key, writer);
	result |= fputs (" = ", writer->f) == EOF;
	result |= writeValue (key, writer);
	result |= writeInlineComment (comments, writer);
	freeComments (comments);
	return result;
}

static int writeSimpleTable (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	result |= writeSimpleTableHeader (parent, key, writer);
	getNextKey (writer);
	result |= writeKeys (key, writer);

	return result;
}

static int writeTableArray (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	Key * arrayRoot = key;
	size_t maxIndex = getArrayMax (arrayRoot);
	size_t nextIndex = 0;
	key = getNextKey (writer);

	while (result == 0 && nextIndex <= maxIndex)
	{
		if (keyIsBelow (arrayRoot, key) == 1)
		{
			char * subIndex = getDirectSubKeyName (arrayRoot, key);
			if (subIndex == NULL)
			{
				return 1;
			}
			size_t foundIndex = arrayStringToIndex (subIndex);
			Key * elementRoot = keyDup (arrayRoot);
			if (elementRoot == NULL)
			{
				elektraFree (subIndex);
				return 1;
			}
			keyAddName (elementRoot, subIndex);
			elektraFree (subIndex);
			while (nextIndex < foundIndex) // write empty table array headers
			{
				result |= writeTableArrayHeader (parent, arrayRoot, NULL, writer);
				nextIndex++;
			}

			result |= writeTableArrayHeader (parent, arrayRoot, key, writer);

			if (keyCmp (elementRoot, key) == 0) // holds for table array entries with comments
			{
				getNextKey (writer);
			}
			result |= writeKeys (elementRoot, writer);
			nextIndex++;

			keyDel (elementRoot);
			key = getCurrentKey (writer);
		}
		else
		{
			while (nextIndex <= maxIndex)
			{
				result |= writeTableArrayHeader (parent, arrayRoot, NULL, writer);
				nextIndex++;
			}
		}
	}

	return result;
}

static int writeArrayBody (Key * key, Writer * writer)
{
	int result = 0;
	result |= fputc ('[', writer->f) == EOF;
	result |= writeArrayElements (key, writer);
	result |= fputc (']', writer->f) == EOF;
	return result;
}

static int writeArrayElements (Key * parent, Writer * writer)
{
	int result = 0;
	Key * key = getNextKey (writer);
	while (keyIsDirectlyBelow (parent, key) == 1)
	{
		CommentList * comments = collectComments (key, writer);
		result |= writePrecedingComments (comments, writer);
		result |= writeValue (key, writer);
		key = getCurrentKey (writer);
		if (keyIsDirectlyBelow (parent, key) == 1)
		{
			result |= fputs (", ", writer->f) == EOF;
		}
		result |= writeInlineComment (comments, writer);
		if (comments != NULL && comments->index == 0)
		{
			result |= writeNewline (writer);
		}
		freeComments (comments);
	}
	return result;
}

static int writeValue (Key * key, Writer * writer)
{
	int result = 0;
	if (isArray (key))
	{
		result |= writeArrayBody (key, writer);
	}
	else if (isInlineTable (key))
	{
		result |= writeInlineTableBody (key, writer);
	}
	else
	{
		result |= writeScalar (key, writer);
		getNextKey (writer);
	}
	return result;
}

static int writeRelativeKeyName (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	bool placeDot = false;
	const char * keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
	const char * keyStop = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (key);
	while (keyPart < keyStop)
	{
		if (!isArrayIndex (keyPart))
		{
			if (placeDot)
			{
				result |= fputc ('.', writer->f) == EOF;
			}
			else
			{
				placeDot = true;
			}
			bool bare = isBareString (writer->checker, keyPart);
			if (!bare)
			{
				result |= fputc ('"', writer->f) == EOF;
			}
			result |= fputs (keyPart, writer->f) == EOF;
			if (!bare)
			{
				result |= fputc ('"', writer->f) == EOF;
			}
		}
		keyPart += elektraStrLen (keyPart);
	}
	return result;
}

static int writeScalar (Key * key, Writer * writer)
{
	int result = 0;
	ELEKTRA_ASSERT (keyGetUnescapedNameSize (key) != 0, "NULL keys should have been handled by null plugin");

	keyRewindMeta (key);
	const Key * origValue = findMetaKey (key, "origvalue");
	const Key * type = findMetaKey (key, "type");
	const char * valueStr = keyString (key);
	if (origValue != NULL)
	{
		valueStr = keyString (origValue);
	}
	ELEKTRA_ASSERT (elektraStrLen (valueStr) > 1, "Empty strings should have been handled by null plugin");
	if (type != NULL)
	{
		if (elektraStrCmp (keyString (type), "boolean") == 0)
		{
			if (isTrue (valueStr))
			{
				result |= fputs ("true", writer->f) == EOF;
			}
			else
			{
				result |= fputs ("false", writer->f) == EOF;
			}
		}
		else if (elektraStrCmp (keyString (type), "string") == 0)
		{
			result |= writeQuoted (valueStr, '"', isMultilineString (valueStr) ? 3 : 1, writer);
		}
		else
		{
			result |= fputs (valueStr, writer->f) == EOF;
		}
	}
	else if (isNumber (writer->checker, valueStr) || isDateTime (writer->checker, valueStr))
	{
		result |= fputs (valueStr, writer->f) == EOF;
	}
	else
	{
		result |= writeQuoted (valueStr, '"', isMultilineString (valueStr) ? 3 : 1, writer);
	}
	return result;
}

static int writeQuoted (const char * value, char quoteChar, int quouteCount, Writer * writer)
{
	int result = 0;
	for (int i = 0; i < quouteCount; i++)
	{
		result |= fputc (quoteChar, writer->f) == EOF;
	}
	result |= fputs (value, writer->f) == EOF;
	for (int i = 0; i < quouteCount; i++)
	{
		result |= fputc (quoteChar, writer->f) == EOF;
	}
	return result;
}

static bool isTrue (const char * boolStr)
{
	if (elektraStrCmp (boolStr, "true") == 0 || elektraStrCmp (boolStr, "1") == 0)
	{
		return true;
	}
	else
	{
		return false;
	}
}

static bool isMultilineString (const char * str)
{
	while (*str != 0)
	{
		if (*str++ == '\n')
		{
			return true;
		}
	}
	return false;
}

static int writeInlineTableBody (Key * key, Writer * writer)
{
	int result = 0;
	result |= fputs ("{ ", writer->f) == EOF;
	result |= writeInlineTableElements (key, writer);
	result |= fputs (" }", writer->f) == EOF;
	return result;
}

static int writeInlineTableElements (Key * parent, Writer * writer)
{
	int result = 0;
	Key * key = getNextKey (writer);
	bool firstElement = true;
	while (keyIsBelow (parent, key) == 1)
	{
		if (firstElement)
		{
			firstElement = false;
		}
		else
		{
			result |= fputs (", ", writer->f) == EOF;
		}
		result |= writeAssignment (parent, key, writer);
		key = getCurrentKey (writer);
	}
	return result;
}

static int writeTableArrayHeader (Key * parent, Key * root, Key * key, Writer * writer)
{
	int result = 0;
	CommentList * comments = (key != NULL ? collectComments (key, writer) : NULL);

	result |= writePrecedingComments (comments, writer);
	result |= fputs ("[[", writer->f) == EOF;
	result |= writeRelativeKeyName (parent, root, writer);
	result |= fputs ("]]", writer->f) == EOF;
	result |= writeInlineComment (comments, writer);
	result |= writeNewline (writer);
	return result;
}

static int writeSimpleTableHeader (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	CommentList * comments = collectComments (key, writer);
	result |= writePrecedingComments (comments, writer);
	result |= fputc ('[', writer->f) == EOF;
	result |= writeRelativeKeyName (parent, key, writer);
	result |= fputc (']', writer->f) == EOF;
	result |= writeInlineComment (comments, writer);
	result |= writeNewline (writer);
	freeComments (comments);
	return result;
}

static int writePrecedingComments (const CommentList * commentList, Writer * writer)
{
	int result = 0;
	while (commentList != NULL)
	{
		if (commentList->index > 0)
		{
			result |= writeComment (commentList, writer);
			result |= writeNewline (writer);
		}
		commentList = commentList->next;
	}
	return result;
}

static int writeInlineComment (const CommentList * commentList, Writer * writer)
{
	int result = 0;
	while (commentList != NULL)
	{
		if (commentList->index == 0)
		{
			result |= writeComment (commentList, writer);
			break;
		}
		commentList = commentList->next;
	}
	return result;
}

static int writeComment (const CommentList * comment, Writer * writer)
{
	int result = 0;
	for (size_t i = 0; i < comment->spaces; i++) // TODO: do with comment->spaces
	{
		result |= fputc (' ', writer->f) == EOF;
	}
	if (comment->start != '\0')
	{
		result |= fputc (comment->start, writer->f) == EOF;
	}
	if (comment->content != NULL)
	{
		result |= fputs (comment->content, writer->f) == EOF;
	}
	return result;
}


static int writeNewline (Writer * writer)
{
	return fputc ('\n', writer->f) == EOF;
}

static KeyType getKeyType (Key * key)
{
	const Key * meta = findMetaKey (key, "tomltype");
	if (meta != NULL)
	{
		if (elektraStrCmp (keyString (meta), "simpletable") == 0)
		{
			return KEY_TYPE_SIMPLE_TABLE;
		}
		else if (elektraStrCmp (keyString (meta), "tablearray") == 0)
		{
			return KEY_TYPE_TABLE_ARRAY;
		}
	}
	return KEY_TYPE_ASSIGNMENT;
}

static CommentList * collectComments (Key * key, Writer * writer)
{
	keyRewindMeta (key);
	const Key * meta;
	CommentList * commentRoot = NULL;
	CommentList * commentBack = NULL;
	size_t currIndex = 0;
	while ((meta = keyNextMeta (key)) != 0)
	{
		const char * pos = (const char *) keyUnescapedName (meta);
		const char * stop = pos + keyGetUnescapedNameSize (meta);
		if (elektraStrCmp (pos, "comment") == 0)
		{
			int subDepth = 0;
			while (pos < stop)
			{
				if (subDepth == 1)
				{
					size_t readIndex = arrayStringToIndex (pos);
					if (readIndex != currIndex || commentRoot == NULL)
					{
						CommentList * nextComment = (CommentList *) elektraCalloc (sizeof (CommentList));
						if (nextComment == NULL)
						{
							freeComments (commentRoot);
							writerError (writer, ERROR_MEMORY, NULL);
							return NULL;
						}
						if (commentBack != NULL)
						{
							commentBack->next = nextComment;
							commentBack = nextComment;
						}
						else
						{
							commentRoot = nextComment;
							commentBack = commentRoot;
						}
						commentBack->index = readIndex;
						currIndex = readIndex;
					}

					if (pos + elektraStrLen (pos) >= stop)
					{ // meta key holding the array content
						commentBack->content = keyString (meta);
					}
				}
				else if (subDepth == 2)
				{
					const char * fieldName = pos;
					if (elektraStrCmp (fieldName, "start") == 0)
					{
						if (elektraStrLen (keyString (meta)) > 1)
						{
							commentBack->start = keyString (meta)[0];
						}
						else
						{
							commentBack->start = '\0';
						}
					}
					else if (elektraStrCmp (fieldName, "space") == 0)
					{
						if (sscanf (keyString (meta), "%lu", &commentBack->spaces) == EOF)
						{
							// printf ("[ERROR] Cant read space value: %s\n", keyString (meta));
							freeComments (commentRoot);
							return NULL;
						}
					}
				}

				subDepth++;
				pos += elektraStrLen (pos);
			}
		}
	}
	return commentRoot;
}

static void freeComments (CommentList * comments)
{
	while (comments != NULL)
	{
		CommentList * next = comments->next;
		elektraFree (comments);
		comments = next;
	}
}

static void addMissingArrayKeys (KeySet * keys, Key * parent)
{
	ArrayInfo * arrays = NULL;
	ksRewind (keys);
	Key * key;
	while ((key = ksNext (keys)) != NULL)
	{
		if (isTableArray (key) && !isArray (key))
		{
			arrays = updateArrayInfo (arrays, key, 0);
		}
		Key * name = keyNew (keyName (key), KEY_END);
		if (name == NULL)
		{
			return;
		}
		do
		{
			if (isArrayIndex (keyBaseName (name)))
			{
				size_t index = arrayStringToIndex (keyBaseName (name));
				keyAddName (name, "..");
				arrays = updateArrayInfo (arrays, name, index);
			}
			else
			{
				keyAddName (name, "..");
			}
		} while (keyCmp (parent, name) != 0);
		keyDel (name);
	}
	while (arrays != NULL)
	{
		Key * arrayRoot = ksLookup (keys, arrays->name, 0);
		if (arrayRoot == NULL)
		{
			keyUpdateArrayMetakey (arrays->name, arrays->maxIndex);
			ksAppendKey (keys, arrays->name);
		}
		else
		{
			const Key * meta = findMetaKey (arrayRoot, "array");
			if (meta == NULL)
			{
				keyUpdateArrayMetakey (arrayRoot, arrays->maxIndex);
			}
			keyDel (arrays->name);
		}
		ArrayInfo * next = arrays->next;
		elektraFree (arrays);
		arrays = next;
	}
}

static void pruneInvalidArrayKeys (KeySet * keys)
{
	ksRewind (keys);
	KeySet * pruneSet = ksNew (8, KS_END);
	Key * key = ksNext (keys);
	while (key != NULL)
	{
		const Key * meta = findMetaKey (key, "array");
		if (meta != NULL)
		{
			Key * sub;
			while ((sub = ksNext (keys)) != NULL && keyIsBelow (key, sub) == 1)
			{
				char * directSub = getDirectSubKeyName (key, sub);
				if (!isArrayIndex (directSub))
				{
					ksAppendKey (pruneSet, key);
					break;
				}
				elektraFree (directSub);
			}
			key = ksCurrent (keys);
		}
		else
		{
			key = ksNext (keys);
		}
	}
	ksRewind (pruneSet);
	while ((key = ksNext (pruneSet)) != NULL)
	{
		Key * prune = ksLookup (keys, key, KDB_O_POP);
		ELEKTRA_ASSERT (prune != NULL, "Key must exist in keyset");
		keyDel (prune);
	}
}

static ArrayInfo * updateArrayInfo (ArrayInfo * root, Key * name, size_t index)
{
	ArrayInfo * ptr = root;
	while (ptr != NULL)
	{
		if (keyCmp (ptr->name, name) == 0)
		{
			if (index > ptr->maxIndex)
			{
				ptr->maxIndex = index;
			}
			return root;
		}
		ptr = ptr->next;
	}
	ArrayInfo * element = elektraCalloc (sizeof (ArrayInfo));
	if (element == NULL)
	{
		return NULL;
	}
	element->name = keyDup (name);
	element->maxIndex = index;
	element->next = root;
	return element;
}

static void writerError (Writer * writer, int err, const char * format, ...)
{
	if (format != NULL)
	{
		va_list args;
		char msg[512];
		va_start (args, format);
		if (writer->cursor < writer->arraySize)
		{
			snprintf (msg, 512, "Key '%s': ", keyName (getCurrentKey (writer)));
			size_t len = elektraStrLen (msg) - 1;
			vsnprintf (msg + len, 512 - len, format, args);
		}
		else
		{
			vsnprintf (msg, 512, format, args);
		}
		va_end (args);
		if (!writer->errorSet)
		{
			writer->errorSet = true;
			emitElektraError (writer->rootKey, err, msg);
			ELEKTRA_LOG_DEBUG ("Error: %s", msg);
		}
		else
		{
			ELEKTRA_LOG_DEBUG ("Additional Error: %s", msg);
		}
	}
	else
	{
		if (!writer->errorSet)
		{
			writer->errorSet = true;
			emitElektraError (writer->rootKey, err, NULL);
		}
	}
}

static KeySet * collectSubKeys(Key * parent, KeySet * ks) {
	KeySet * subs = ksNew(0, KS_END);
	ksRewind(ks);
	Key * key;
	while ((key = ksNext(ks)) != NULL) {
		if (keyIsBelow(parent, key) == 1) {
			ksAppendKey(subs, key);
		}
	}
	return subs;
}
