/**
 * @file write.c
 *
 * @brief Contains functionality for writing a TOML file from an Elektra keyset
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include <elektra/ease/meta.h>
#include <elektra/ease/old_ease.h>
#include <elektra/kdb/errors.h>
#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <internal/utility/assert.h>
#include <internal/utility/old_helper.h>
#include <regex.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "codepoint.h"
#include "error.h"
#include "integer.h"
#include "node.h"
#include "prepare.h"
#include "type.h"
#include "utility.h"
#include "write.h"

#define ASCII_CONTROL                                                                                                                      \
	"\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F"                                                                 \
	"\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1A\x1B\x1C\x1D\x1E\x1F"                                                                 \
	"\x7F"

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
	char start;
	const char * space;
	const char * content;
	struct CommentList_ * next;
} CommentList;

typedef enum
{
	STRING_BASIC = 0x0,
	STRING_LITERAL = 0x1,
	STRING_MULTILINE = 0x2,
} StringType;

static Writer * createWriter (Key * parent);

static void destroyWriter (Writer * writer);
static int writeTree (Node * node, Writer * writer);
static int writeSimpleTableHeader (const char * name, Writer * writer);
static int writeOpeningSequence (Node * node, Writer * writer);
static int writeClosingSequence (Node * node, Writer * writer);
static int writeTableArrayHeader (const char * name, Writer * writer);
static int writeScalar (Key * key, Writer * writer);
static int writeString (Key * key, StringType stringType, const char * value, Writer * writer);
static int writePrecedingComments (const CommentList * commentList, Writer * writer);
static int writeInlineComment (const CommentList * commentList, bool emitNewline, Writer * writer);
static int writeComment (const CommentList * comment, Writer * writer);
static int writeNewline (Writer * writer);
static int writeFileTrailingComments (Key * parent, Writer * writer);
static int collectComments (CommentList ** comments, Key * key, Writer * writer);
static void freeComments (CommentList * comments);
static bool shouldWriteValue (Node * node);
static bool needsKeyAssignment (Node * node);
static bool isListElement (Node * node);
static bool isLastChild (Node * node);
static bool hasInlineComment (Node * node);
static bool needNewlineBeforeComment (Node * node);

int tomlWrite (KeySet * keys, Key * parent)
{
	prepareKeySet (keys, parent);

	elektraCursor it = 0;
	if (keyCmp (ksAtCursor (keys, it), parent) == 0)
	{
		it++;
	}

	Node * root = buildTree (NULL, parent, keys, &it);
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

static int writeTree (Node * node, Writer * writer)
{
	int result = 0;
	CommentList * comments = NULL;

	if (keyCmp (node->key, writer->rootKey) != 0)
	{
		result |= collectComments (&comments, node->key, writer);
		bool hasComments = comments != NULL;

		// Comments will be dropped if parent is an inline table
		if (hasComments && node->parent->type != NT_INLINE_TABLE)
		{
			bool needNewline = needNewlineBeforeComment (node);
			if (hasComments && needNewline)
			{
				result |= fputc ('\n', writer->f) == EOF;
			}
			result |= writePrecedingComments (comments, writer);
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

		// Inline comments in inline tables are illegal
		if (node->parent->type != NT_INLINE_TABLE)
		{
			result |= writeInlineComment (comments, true, writer);
		}
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
	ELEKTRA_ASSERT (key != NULL, "key was NULL");
	ELEKTRA_ASSERT (writer != NULL, "writer was NULL");

	if (keyIsBinary (key))
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (writer->rootKey,
							"Detected binary key '%s' in toml plugin. Please ensure the 'base64' plugin is "
							"mounted when using binary keys with TOML.",
							keyName (key));
		return -1;
	}

	int result = 0;

	const Key * origValue = keyGetMeta (key, "origvalue");
	const Key * type = keyGetMeta (key, "type");
	const Key * tomlTypeKey = keyGetMeta (key, "tomltype");
	const char * tomlType = tomlTypeKey == NULL ? "" : keyString (tomlTypeKey);
	const char * valueStr = keyString (key);

	if (origValue != NULL)
	{
		valueStr = keyString (origValue);
	}

	if (type != NULL && strcmp (keyString (type), "boolean") == 0)
	{
		if (strcmp (valueStr, "0") == 0)
		{
			result |= fputs ("false", writer->f) == EOF;
		}
		else if (strcmp (valueStr, "1") == 0)
		{
			result |= fputs ("true", writer->f) == EOF;
		}
		else
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (writer->rootKey, "Expected a boolean value of either 0 or 1, but got %s",
								 valueStr);
			result = 1;
		}
	}
	else if ((type == NULL || strcmp (keyString (type), "string") != 0) &&
		 (isFloat (writer->checker, valueStr) || isValidIntegerAnyBase (valueStr) || isDateTime (writer->checker, valueStr)))
	{
		result |= fputs (valueStr, writer->f) == EOF;
	}
	else
	{
		// handles values explicitly typed as string
		// as well as all untyped values
		StringType stringType;
		if (strcmp (tomlType, "string_basic") == 0)
		{
			stringType = STRING_BASIC;
		}
		else if (strcmp (tomlType, "string_ml_basic") == 0)
		{
			stringType = STRING_BASIC | STRING_MULTILINE;
		}
		else if (strcmp (tomlType, "string_literal") == 0)
		{
			stringType = STRING_LITERAL;
		}
		else if (strcmp (tomlType, "string_ml_literal") == 0)
		{
			stringType = STRING_LITERAL | STRING_MULTILINE;
		}
		else
		{
			stringType = STRING_BASIC;
			const char * firstNewline = strchr (valueStr, '\n');
			if (firstNewline != NULL && strchr (firstNewline + 1, '\n') != NULL)
			{
				// contains 2 newlines -> write as multiline string
				stringType |= STRING_MULTILINE;
			}
		}

		result |= writeString (key, stringType, valueStr, writer);
	}
	return result;
}

static const char * stringQuotes (StringType type)
{
	bool isLiteral = (type & STRING_LITERAL) != 0;
	bool isMultiline = (type & STRING_MULTILINE) != 0;

	if (isMultiline)
	{
		if (isLiteral)
		{
			return "'''";
		}
		else
		{
			return "\"\"\"";
		}
	}
	else
	{
		if (isLiteral)
		{
			return "'";
		}
		else
		{
			return "\"";
		}
	}
}

static const char * writeAscii (int * result, const char * value, Writer * writer)
{
	const char * asciiEnd = value;
	while (*asciiEnd >= 0x20 && *asciiEnd <= 0x7E)
	{
		++asciiEnd;
	}
	if (asciiEnd > value)
	{
		size_t n = asciiEnd - value;
		*result |= fwrite (value, 1, n, writer->f) < n;
	}
	return asciiEnd;
}

static const char * writeEscapeSequence (int * result, StringType type, const char * value, Writer * writer)
{
	switch (*value)
	{
	case '\b':
		*result |= fputs ("\\b", writer->f) == EOF;
		break;
	case '\t':
		// 2a. special case: if multiline, write tabs literally
		if (type & STRING_MULTILINE)
		{
			*result |= fputc ('\t', writer->f) == EOF;
		}
		else
		{
			*result |= fputs ("\\t", writer->f) == EOF;
		}
		break;
	case '\n':
		// 2b. special case: if multiline, write newlines literally
		if (type & STRING_MULTILINE)
		{
			*result |= fputc ('\n', writer->f) == EOF;
		}
		else
		{
			*result |= fputs ("\\n", writer->f) == EOF;
		}
		break;
	case '\f':
		*result |= fputs ("\\f", writer->f) == EOF;
		break;
	case '\r':
		*result |= fputs ("\\r", writer->f) == EOF;
		break;
	case '\"':
		*result |= fputs ("\\\"", writer->f) == EOF;
		break;
	case '\\':
		*result |= fputs ("\\\\", writer->f) == EOF;
		break;
	default:
		return value;
	}

	return value + 1;
}

static const char * writeUtf8Codepoint (int * result, bool * warnedUtf8, Key * key, const char * value, Writer * writer)
{
	size_t utf8Len = utf8LenFromHeadChar ((uint8_t) *value);
	if (utf8Len == 0 || !isValidUtf8 ((uint8_t *) value, utf8Len))
	{
		if (!*warnedUtf8)
		{
			ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (writer->rootKey,
								  "Key '%s' contained non-UTF-8 value. Invalid byte "
								  "sequences will be replaced with replacement "
								  "character U+FFFD. Mark the key as binary and use base64 "
								  "plugin to preserve non-UTF-8 values.",
								  keyName (key));
			*warnedUtf8 = true;
		}

		// write U+FFFD (replacment character), skip one byte and try again
		*result |= fputs ("\xEF\xBF\xBD", writer->f) == EOF;
	}
	else
	{
		// write valid UTF-8
		*result |= fwrite (value, 1, utf8Len, writer->f) < utf8Len;
	}

	return value + 1;
}

static bool checkLiteralAsciiControl (const char * value, bool allowNewline)
{
	const char * control;
	while ((control = strpbrk (value, ASCII_CONTROL)) != NULL)
	{
		if (*control == 0x09)
		{
			// tab is allowed
			continue;
		}

		if (allowNewline && *control == 0x0A)
		{
			// newline (LF) in multiline
			continue;
		}

		if (allowNewline && *control == 0x0D && *control == 0x0A)
		{
			// newline (CRLF) in multiline
			continue;
		}

		// found illegal control character
		return true;
	}

	return false;
}

static int writeString (Key * key, StringType type, const char * value, Writer * writer)
{
	int result = 0;
	result |= fputs (stringQuotes (type), writer->f) == EOF;

	if (type & STRING_LITERAL)
	{
		if ((type & STRING_MULTILINE) == 0 && strchr (value, '\n') != NULL)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				writer->rootKey, "Key with literal non-multiline cannot contain newline: %s", keyName (key));
			result |= -1;
		}
		else if (!isValidUtf8 ((uint8_t *) value, keyGetValueSize (key) - 1))
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (writer->rootKey, "Key with literal string must contain valid UTF-8: %s",
								 keyName (key));
			result |= -1;
		}
		else if (checkLiteralAsciiControl (value, type & STRING_MULTILINE))
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				writer->rootKey,
				"Key with literal string must not contain ASCII control characters except for tab (U+0009): %s",
				keyName (key));
			result |= -1;
		}
		else
		{
			result |= fputs (value, writer->f) == EOF;
		}
	}
	else
	{
		bool warnedUtf8 = false;
		while (*value != '\0')
		{
			// 1. write a block of ASCII print characters
			value = writeAscii (&result, value, writer);

			// 2. if byte has TOML escape sequence, write that
			value = writeEscapeSequence (&result, type, value, writer);

			if (*value == '\0')
			{
				// end of string reached
				break;
			}

			// 3. if valid UTF-8 follows, write that
			//    otherwise write replacement character
			value = writeUtf8Codepoint (&result, &warnedUtf8, key, value, writer);
		}
	}

	result |= fputs (stringQuotes (type), writer->f) == EOF;
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

	if (comment->space != NULL)
	{
		result |= fputs (comment->space, writer->f) == EOF;
	}

	if (comment->start != '\0')
	{
		result |= fputc (comment->start, writer->f) == EOF;
	}
	else if (comment->content != NULL)
	{
		result |= fputc ('#', writer->f) == EOF;
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

	CommentList * comments = NULL;
	result |= collectComments (&comments, parent, writer);
	if (comments != NULL)
	{
		result |= writePrecedingComments (comments, writer);
		freeComments (comments);
	}

	return result;
}

static int collectComments (CommentList ** comments, Key * key, Writer * writer)
{
	int result = 0;

	const Key * meta;
	CommentList * commentRoot = *comments;
	CommentList * commentBack = NULL;
	size_t currIndex = 0;

	KeySet * metaKeys = keyMeta (key);

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		meta = ksAtCursor (metaKeys, it);
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
							ELEKTRA_SET_OUT_OF_MEMORY_ERROR (writer->rootKey);
							*comments = NULL;
							return -1;
						}
						nextComment->space = NULL;
						nextComment->start = '\0';

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
						if (keyString (meta)[0] == '#')
						{
							commentBack->start = '#';
						}
						else
						{
							commentBack->start = '\0';
						}
					}
					else if (elektraStrCmp (fieldName, "space") == 0)
					{
						const char * whitespace = keyString (meta);
						if (whitespace[strspn (whitespace, " \t")] != '\0')
						{
							ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
								writer->rootKey,
								"The '%s' metakey of '%s' contains an invalid whitespace character. Only "
								"space and tab are allowed as whitespace characters in TOML files.",
								keyName (meta), keyName (key));
							result |= -1;
						}

						commentBack->space = whitespace;
					}
				}

				subDepth++;
				pos += elektraStrLen (pos);
			}
			// Accept valid metakeys, throw error for everything else
		}
		else if (elektraStrCmp (pos, "order") != 0 && elektraStrCmp (pos, "type") != 0 && elektraStrCmp (pos, "tomltype") != 0 &&
			 elektraStrCmp (pos, "origvalue") != 0 && elektraStrCmp (pos, "binary") != 0 && elektraStrCmp (pos, "array") != 0)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (key, "The Metakey %s is not supported by TOML", keyString (meta));
			return -1;
		}
	}
	*comments = commentRoot;
	return result;
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
