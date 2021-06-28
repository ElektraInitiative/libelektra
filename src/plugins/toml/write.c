/**
 * @file write.c
 *
 * @brief Contains functionality for writing a TOML file from an Elektra keyset
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <kdb.h>
#include <kdbassert.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbmeta.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "error.h"
#include "integer.h"
#include "meta.h"
#include "node.h"
#include "prepare.h"
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

static Writer * createWriter (Key * parent);

static void destroyWriter (Writer * writer);
static void writerError (Writer * writer, int err, const char * format, ...);
static int writeTree (Node * node, Writer * writer);
static int writeSimpleTableHeader (const char * name, Writer * writer);
static int writeOpeningSequence (Node * node, Writer * writer);
static int writeClosingSequence (Node * node, Writer * writer);
static int writeTableArrayHeader (const char * name, Writer * writer);
static int writeScalar (Key * key, Writer * writer);
static int writeQuoted (const char * value, char quoteChar, int quouteCount, Writer * writer);
static int writeMetakeys (Key * key, Writer * writer);
static int writePrecedingComments (const CommentList * commentList, Writer * writer);
static int writeInlineComment (const CommentList * commentList, bool emitNewline, Writer * writer);
static int writeComment (const CommentList * comment, Writer * writer);
static int writeNewline (Writer * writer);
static int writeFileTrailingComments (Key * parent, Writer * writer);
static CommentList * collectComments (Key * key, Writer * writer);
static void freeComments (CommentList * comments);
static bool shouldWriteValue (Node * node);
static bool needsKeyAssignment (Node * node);
static bool isListElement (Node * node);
static bool isLastChild (Node * node);
static bool hasInlineComment (Node * node);
static bool isMultilineString (const char * str);
static bool needNewlineBeforeComment (Node * node);

int tomlWrite (KeySet * keys, Key * parent)
{
	elektraCursor cursor = ksGetCursor (keys);
	prepareKeySet (keys, parent);

	ksRewind (keys);
	ksNext (keys);
	if (keyCmp (ksCurrent (keys), parent) == 0)
	{
		ksNext (keys);
	}
	Node * root = buildTree (NULL, parent, keys);
	if (root == NULL)
	{
		return 1;
	}
	Writer * writer = createWriter (parent);
	if (writer == NULL)
	{
		destroyTree (root);
		ELEKTRA_SET_RESOURCE_ERROR (parent, keyString (parent));
		return 1;
	}
	int result = 0;

	result |= writeTree (root, writer);
	Key * parentKey = ksLookup (keys, parent, 0);
	if (parentKey != NULL)
	{
		result |= writeFileTrailingComments (parentKey, writer);
	}

	destroyWriter (writer);
	destroyTree (root);
	ksSetCursor (keys, cursor);
	return result;
}

static Writer * createWriter (Key * parent)
{
	Writer * writer = elektraCalloc (sizeof (Writer));
	if (writer == NULL)
	{
		return NULL;
	}
	writer->filename = elektraStrDup (keyString (parent));
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
	writer->rootKey = parent;
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
		destroyTypeChecker (writer->checker);
		elektraFree (writer);
	}
}

static void writerError (Writer * writer, int err, const char * format, ...)
{
	if (format != NULL)
	{
		va_list args;
		char msg[512];
		va_start (args, format);
		vsnprintf (msg, 512, format, args);
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


static int writeTree (Node * node, Writer * writer)
{
	int result = 0;
	CommentList * comments = NULL;

	if (keyCmp (node->key, writer->rootKey) != 0)
	{
		comments = collectComments (node->key, writer);
		bool hasComments = comments != NULL || hasWriteableMetakeys (node->key);

		// Comments/Metakeys will be dropped if parent is an inline table
		if (hasComments && node->parent->type != NT_INLINE_TABLE)
		{
			bool needNewline = needNewlineBeforeComment (node);
			if (hasComments && needNewline)
			{
				result |= fputc ('\n', writer->f) == EOF;
			}
			result |= writePrecedingComments (comments, writer);
			result |= writeMetakeys (node->key, writer);
		}
	}

	if (node->type == NT_SIMPLE_TABLE)
	{
		result |= writeSimpleTableHeader (node->relativeName, writer);
		result |= writeInlineComment (comments, false, writer);
		result |= writeNewline (writer);
	}
	if (node->parent != NULL && node->parent->type == NT_TABLE_ARRAY)
	{
		result |= writeTableArrayHeader (node->parent->relativeName, writer);
		result |= writeInlineComment (comments, false, writer);
		result |= writeNewline (writer);
	}

	if (needsKeyAssignment (node))
	{
		result |= fputs (node->relativeName, writer->f) == EOF;
		result |= fputs (" = ", writer->f) == EOF;
	}

	result |= writeOpeningSequence (node, writer);

	for (size_t i = 0; i < node->childCount; i++)
	{
		result |= writeTree (node->children[i], writer);
	}

	result |= writeClosingSequence (node, writer);

	if (shouldWriteValue (node))
	{
		result |= writeScalar (node->key, writer);
	}

	bool listElement = isListElement (node);
	if (listElement)
	{
		if (!isLastChild (node))
		{
			result |= fputc (',', writer->f) == EOF;
			if (!hasInlineComment (node))
			{
				result |= fputc (' ', writer->f) == EOF;
			}
		}
		ELEKTRA_ASSERT (node->type == NT_LEAF || node->type == NT_ARRAY || node->type == NT_INLINE_TABLE ||
					node->type == NT_LIST_ELEMENT,
				"Invalid type of list element, only NT_LEAF, NT_ARRAY or NT_INLINE_TABLE expected, but found other: %d",
				node->type);
		result |= writeInlineComment (comments, true, writer);
	}
	else
	{
		switch (node->type)
		{
		case NT_LEAF:
		case NT_ARRAY:
		case NT_INLINE_TABLE:
			result |= writeInlineComment (comments, false, writer);
			if (!listElement)
			{
				result |= writeNewline (writer);
			}
		default:
			break;
		}
	}

	freeComments (comments);

	return result;
}

static bool shouldWriteValue (Node * node)
{
	switch (node->type)
	{
	case NT_LEAF:
		return true;
	case NT_LIST_ELEMENT:
		if (node->parent != NULL && node->parent->type == NT_ARRAY)
		{
			return true;
		}
		else
		{
			return false;
		}
	default:
		return false;
	}
}

static bool isListElement (Node * node)
{
	if (node->parent == NULL)
	{
		return false;
	}
	switch (node->parent->type)
	{
	case NT_ARRAY:
	case NT_INLINE_TABLE:
		return true;
	default:
		return false;
	}
}

static bool hasInlineComment (Node * node)
{
	return keyGetMeta (node->key, "comment/#0/space") != NULL;
}

static bool isLastChild (Node * node)
{
	if (node->parent == NULL)
	{
		return false;
	}
	return node->parent->children[node->parent->childCount - 1] == node;
}

static bool needsKeyAssignment (Node * node)
{
	switch (node->type)
	{
	case NT_LEAF:
	case NT_ARRAY:
	case NT_INLINE_TABLE:
		if (node->parent == NULL)
		{
			return false;
		}
		else if (node->parent->type == NT_ARRAY)
		{
			return false;
		}
		else
		{
			return true;
		}
	default:
		return false;
	}
}

static int writeOpeningSequence (Node * node, Writer * writer)
{
	switch (node->type)
	{
	case NT_ARRAY:
		return fputs ("[", writer->f) == EOF;
	case NT_INLINE_TABLE:
		return fputs ("{ ", writer->f) == EOF;
	default:
		return 0;
	}
}

static int writeClosingSequence (Node * node, Writer * writer)
{
	switch (node->type)
	{
	case NT_ARRAY:
		return fputs ("]", writer->f) == EOF;
	case NT_INLINE_TABLE:
		return fputs (" }", writer->f) == EOF;
	default:
		return 0;
	}
}

static int writeSimpleTableHeader (const char * name, Writer * writer)
{
	int result = 0;
	result |= fputc ('[', writer->f) == EOF;
	result |= fputs (name, writer->f) == EOF;
	result |= fputc (']', writer->f) == EOF;
	return result;
}

static int writeTableArrayHeader (const char * name, Writer * writer)
{
	int result = 0;
	result |= fputs ("[[", writer->f) == EOF;
	result |= fputs (name, writer->f) == EOF;
	result |= fputs ("]]", writer->f) == EOF;
	return result;
}

static int writeScalar (Key * key, Writer * writer)
{
	int result = 0;

	const Key * origValue = keyGetMeta (key, "origvalue");
	const Key * type = keyGetMeta (key, "type");
	const char * valueStr = keyString (key);

	if (isBase64String (valueStr))
	{
		if (elektraStrCmp (valueStr, "@BASE64") == 0) // could also only match for length
		{
			return writeQuoted ("@NULL", '\'', 1, writer);
		}
		else
		{
			return writeQuoted (valueStr, '\'', 1, writer);
		}
	}

	if (origValue != NULL)
	{
		valueStr = keyString (origValue);
	}

	if (type != NULL && elektraStrCmp (keyString (type), "boolean") == 0)
	{
		if (elektraStrCmp (valueStr, "0") == 0)
		{
			result |= fputs ("false", writer->f) == EOF;
		}
		else if (elektraStrCmp (valueStr, "1") == 0)
		{
			result |= fputs ("true", writer->f) == EOF;
		}
		else
		{
			writerError (writer, ERROR_SYNTACTIC, "Expected a boolean value of either 0 or 1, but got %s", valueStr);
			result = 1;
		}
	}
	else if (type != NULL && elektraStrCmp (keyString (type), "string") == 0)
	{
		result |= writeQuoted (valueStr, '"', isMultilineString (valueStr) ? 3 : 1, writer);
	}
	else if (isFloat (writer->checker, valueStr) || isValidIntegerAnyBase (valueStr) || isDateTime (writer->checker, valueStr))
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

static int writeMetakeys (Key * key, Writer * writer)
{
	int result = 0;
	keyRewindMeta (key);
	const Key * meta;
	while ((meta = keyNextMeta (key)) != NULL)
	{
		if (shouldWriteMetakey (meta))
		{
			result |= writeMetakeyAsComment (meta, writer->f);
			result |= writeNewline (writer);
		}
	}
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

static int writeInlineComment (const CommentList * commentList, bool emitNewline, Writer * writer)
{
	int result = 0;
	while (commentList != NULL)
	{
		if (commentList->index == 0)
		{
			result |= writeComment (commentList, writer);
			if (emitNewline)
			{
				result |= writeNewline (writer);
			}
			break;
		}
		commentList = commentList->next;
	}
	return result;
}

static int writeComment (const CommentList * comment, Writer * writer)
{
	int result = 0;
	for (size_t i = 0; i < comment->spaces; i++)
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

static int writeFileTrailingComments (Key * parent, Writer * writer)
{
	int result = 0;

	CommentList * comments = collectComments (parent, writer);
	if (comments != NULL)
	{
		result |= writePrecedingComments (comments, writer);
		freeComments (comments);
	}

	result |= writeMetakeys (parent, writer);

	return result;
}

static CommentList * collectComments (Key * key, Writer * writer)
{
	keyRewindMeta (key);
	const Key * meta;
	CommentList * commentRoot = NULL;
	CommentList * commentBack = NULL;
	size_t currIndex = 0;
	while ((meta = keyNextMeta (key)) != NULL)
	{
		const char * pos = (const char *) keyUnescapedName (meta);
		const char * stop = pos + keyGetUnescapedNameSize (meta);
		pos += 2; // skip namespace
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

// We may need an additional newline before starting with comments in a list, otherwise
// a comment preceding a value may be written as an inline comment of the previous value.
static bool needNewlineBeforeComment (Node * node)
{
	if (isListElement (node))
	{
		if (!isFirstChildren (node))
		{
			for (size_t prevIndex = 0; prevIndex + 1 < node->parent->childCount; prevIndex++)
			{
				if (node->parent->children[prevIndex + 1] == node)
				{
					if (!hasInlineComment (node->parent->children[prevIndex]))
					{
						return true;
					}
				}
			}
		}
	}
	return false;
}
