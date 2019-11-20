#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

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
	KeySet * keys;
} Writer;

typedef struct CommentList_
{
	size_t index;
	const char * content;
	char start;
	size_t spaces;
	struct CommentList_ * next;
} CommentList;

static Writer * createWriter (Key * rootKey, KeySet * keys);
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
static int writeRelativeKeyName (Key * parent, Key * key, Writer * writer);
static int writeTableArrayHeader (Key * parent, Key * root, Key * key, Writer * writer);
static int writeSimpleTableHeader (Key * parent, Key * key, Writer * writer);
static int writePrecedingComments (const CommentList * commentList, Writer * writer);
static int writeInlineComment (const CommentList * commentList, Writer * writer);
static int writeComment (const CommentList * comment, Writer * writer);
static int writeNewline (Writer * writer);
static CommentList * collectComments (Key * key);
static void freeComments (CommentList * comments);
static KeyType getKeyType (Key * key);
static bool isBoolean (const char * str);
static bool isNumber (const char * str);
static bool * isTrue (const char * boolStr);

int tomlWrite (KeySet * keys, Key * parent)
{
	// dumpKS (keys);
	ksRewind (keys);

	Writer * w = createWriter (parent, keys);
	if (w == NULL)
	{
		ELEKTRA_SET_RESOURCE_ERROR (parent, keyString (parent));
		return 1;
	}
	int result = 0;
	ksRewind (w->keys);
	if (keyCmp (ksNext (w->keys), parent) == 0)
	{
		ksNext (w->keys);
	}
	result |= writeKeys (parent, w);
	CommentList * comments = collectComments (parent);
	writePrecedingComments (comments, w);
	freeComments (comments);

	destroyWriter (w);
	ksRewind (keys);
	return result;
}

static Writer * createWriter (Key * rootKey, KeySet * keys)
{
	Writer * writer = elektraCalloc (sizeof (Writer));
	writer->filename = elektraStrDup (keyString (rootKey));
	ELEKTRA_LOG_DEBUG ("Writing file %s\n", writer->filename);
	if (writer->filename == 0)
	{
		destroyWriter (writer);
		return NULL;
	}
	writer->f = fopen (writer->filename, "w");
	if (writer->f == NULL)
	{
		destroyWriter (writer);
		return NULL;
	}
	writer->rootKey = rootKey;
	writer->keys = keys;

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
	}
}

static int writeKeys (Key * parent, Writer * writer)
{
	printf ("*** WRITE KEYS FOR %s\n", keyName (parent));
	Key * key = ksCurrent (writer->keys);

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
			result |= writeSimpleTable (parent, key, writer);
			break;
		case KEY_TYPE_TABLE_ARRAY:
			result |= writeTableArray (parent, key, writer);
			break;
		}
		key = ksCurrent (writer->keys);
	}
	return result;
}

static int writeAssignment (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	CommentList * comments = collectComments (key);
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
	ksNext (writer->keys);
	result |= writeKeys (key, writer);

	return result;
}

static int writeTableArray (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	Key * arrayRoot = key;
	size_t maxIndex = getArrayMax (arrayRoot);
	size_t nextIndex = 0;
	key = ksNext (writer->keys);

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
				ksNext (writer->keys);
			}
			result |= writeKeys (elementRoot, writer);
			nextIndex++;

			keyDel (elementRoot);
			key = ksCurrent (writer->keys);
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
	Key * key = ksNext (writer->keys);
	while (keyIsDirectlyBelow (parent, key) == 1)
	{
		CommentList * comments = collectComments (key);
		result |= writePrecedingComments (comments, writer);
		result |= writeValue (key, writer);
		key = ksCurrent (writer->keys);
		if (keyIsDirectlyBelow (parent, key))
		{
			result |= fputs (", ", writer->f) == EOF;
		}
		result |= writeInlineComment (comments, writer);
		result |= writeNewline (writer);
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
		ksNext (writer->keys);
	}
	return result;
}

static int writeRelativeKeyName (Key * parent, Key * key, Writer * writer)
{
	int result = 0;
	char * relativeName = getRelativeKeyName (parent, key);
	if (relativeName != NULL)
	{
		result |= fputs (relativeName, writer->f) == EOF;
		elektraFree (relativeName);
	}
	return result;
}

static int writeScalar (Key * key, Writer * writer)
{
	printf ("**** Writing scalar: Key = '%s', Value = '%s'\n", keyName (key), keyString (key));
	int result = 0;
	keyRewindMeta (key);
	const Key * origValue = findMetaKey (key, "origvalue");
	if (origValue != NULL)
	{
		result |= fputs (keyString (origValue), writer->f) == EOF;
	}
	else
	{
		const char * valueStr = keyString (key);
		if (isNumber (valueStr)) {
			result |= fputs (valueStr, writer->f) == EOF;
		}
		else if (isBoolean (valueStr))
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
		else
		{
			result |= fputc ('"', writer->f) == EOF;
			result |= fputs (valueStr, writer->f) == EOF;
			result |= fputc ('"', writer->f) == EOF;
		}
	}
	return result;
}

static bool isNumber (const char * str)
{
	int base = 10;
	const char * ptr = str;
	if (*ptr == '0')
	{
		switch (*++ptr)
		{
		case 'b':
			base = 2;
			break;
		case 'o':
			base = 8;
			break;
		case 'x':
			base = 16;
			break;
		case 0:
			return true;
		default:
			return false;
		}
		ptr++;
	}
	else if (*ptr == '+' || *ptr == '-')
	{
		ptr++;
		str++;
		if (*ptr == 0)
		{
			return false;
		}
	}

	while (*ptr != 0)
	{
		if (base == 10)
		{
			if (*ptr == '0' && ptr == str && *(ptr + 1) != 0)
			{
				return false;
			}
			else if (*ptr == '_')
			{
				if (ptr == str || *(ptr + 1) == '_')
				{
					return false;
				}
				else
				{
					ptr++;
					continue;
				}
			}
		}
		if (base <= 10)
		{
			if (!(*ptr >= '0' && *ptr < '0' + base))
			{
				return false;
			}
		}
		else if (base == 16)
		{
			if (!(*ptr >= '0' && *ptr <= '9' || *ptr >= 'a' && *ptr <= 'f' || *ptr >= 'A' && *ptr <= 'F'))
			{
				return false;
			}
		}

		ptr++;
	}
	return true;
}

bool isFloat(const char * str) {
	const char * ptr = str;
	const char * dot = NULL;
	const char * exponent = NULL;
	if (*ptr == '+' || *ptr == '-') {
		ptr++;
		str++;
	}
	while (*ptr != 0) {
		if (*ptr >= '0' && *ptr <= '9') {
		} else if (*ptr == '.') {
			if (exponent != NULL || dot != NULL) {
				return false;
			} else
				dot = ptr;
			}
		} else if (*ptr == 'e' || *ptr == 'E') {
			if (exponent != NULL) {
				return false;
			} else if (ptr == str) {
				return false;
			}
			exponent = ptr;
			if (*(exponent + 1) == '+' ||
				*(exponent + 1) == '-') {
				ptr++;
			}
		} else if (*ptr == '0') {
			if (dot == NULL) {
				if (ptr == str && *(ptr + 1) != '.'){	
					return false;	// no leading zeros, except for 0.*
				}
			} else if (exponent != NULL) {
				if (ptr - 1 == exponent || *(ptr - 1) == '+' || *(ptr - 1) == '-') {
					return false;
				}
			}
		}
		ptr++;
	}
}

bool validDigits(const char * start, bool allowLeadingZero) {
	/*while (*ptr != 0) {
		if (*ptr >= '0'
	}*/
}


static bool * isTrue (const char * boolStr)
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


static bool isBoolean (const char * str)
{
	return elektraStrCmp (str, "true") == 0 || elektraStrCmp (str, "false") == 0 || elektraStrCmp (str, "0") == 0 ||
	       elektraStrCmp (str, "1") == 0;
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
	Key * key = ksNext (writer->keys);
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
		key = ksCurrent (writer->keys);
	}
	return result;
}

static int writeTableArrayHeader (Key * parent, Key * root, Key * key, Writer * writer)
{
	int result = 0;
	CommentList * comments = (key != NULL ? collectComments (key) : NULL);

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
	CommentList * comments = collectComments (key);
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
	for (size_t i = 0; i < (comment->index == 0 ? 4 : 0); i++) // TODO: do with comment->spaces
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

static CommentList * collectComments (Key * key)
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
							printf ("[ERROR] Cant read space value: %s\n", keyString (meta));
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
