#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "write.h"

typedef enum
{
	KEY_TYPE_SCALAR,
	KEY_TYPE_ARRAY,
	KEY_TYPE_SIMPLE_TABLE,
	KEY_TYPE_INLINE_TABLE,
	KEY_TYPE_TABLE_ARRAY
} KeyType;

typedef struct KeyStack_
{
	Key * key;
	KeyType type;
	struct KeyStack_ * next;
} KeyStack;

typedef struct
{
	char * filename;
	FILE * f;
	Key * rootKey;
	KeyStack * keyStack;
} Writer;

static int writeKey (Key * key, Writer * writer);
static int writeScalar (Key * key, Writer * writer);
static int writeTableArray (Key * key, Writer * writer);
static int writeSimpleTable (Key * key, Writer * writer);
static int writeInlineTableOpening (Key * key, Writer * writer);
static int writeArrayOpening (Key * key, Writer * writer);
static int writeKeyName (Key * key, Writer * writer);
static int writeValue (Key * key, Writer * writer);
static int writeEquality (Writer * writer);
static int writeNewline (Writer * writer);
static int writeArraySeparator (Writer * writer);
static int writeKeyNameInSequence (Key * key, KeyType type, Writer * writer);
static int writeOpeningSequence (KeyType type, Writer * writer);
static int writeClosingSequence (KeyType type, Writer * writer);
static int handleClosingSequences (Key * key, Writer * writer);
static char * getKeyName (const Key * key, const Key * rootKey);
static KeyType getKeyType (Key * key);
static bool isArrayElement (Key * key);
static Key * findMetaKey (Key * key, const char * metakeyName);
static Writer * createWriter (Key * rootKey);
static void destroyWriter (Writer * writer);
static KeyStack * pushKey (KeyStack * root, Key * key, KeyType type);
static KeyStack * popKey (KeyStack * root);
static void freeKeyStack (KeyStack * root);


int tomlWrite (KeySet * keys, Key * rootKey)
{
	Writer * w = createWriter (rootKey);
	if (w == NULL)
	{
		return 1;
	}

	ksRewind (keys);
	Key * currKey = NULL;
	int result = 0;
	while ((currKey = ksNext (keys)) != NULL && result == 0)
	{
		if (keyCmp (currKey, rootKey) != 0)
		{
			result = writeKey (currKey, w);
		}
	}

	destroyWriter (w);
	return result;
}

static Writer * createWriter (Key * rootKey)
{
	Writer * writer = elektraCalloc (sizeof (Writer));
	writer->filename = elektraStrDup (keyString (rootKey));
	if (writer->filename == 0)
	{
		destroyWriter (writer);
		return NULL;
	}
	printf ("FILENAME = %s\n", writer->filename);
	writer->f = fopen (writer->filename, "w");
	if (writer->f == NULL)
	{
		destroyWriter (writer);
		return NULL;
	}
	writer->rootKey = rootKey;

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
		freeKeyStack (writer->keyStack);
	}
}


static int writeKey (Key * key, Writer * writer)
{
	int result = 0;
	printf (">> Write KEY = %s\n", keyName (key));
	if (handleClosingSequences (key, writer) != 0)
	{
		return 1;
	}

	KeyType type = getKeyType (key);
	switch (type)
	{
	case KEY_TYPE_SCALAR:
		result |= writeScalar (key, writer);
		break;
	case KEY_TYPE_ARRAY:
		result |= writeArrayOpening (key, writer);
		writer->keyStack = pushKey (writer->keyStack, key, type);
		break;
	case KEY_TYPE_SIMPLE_TABLE:
	case KEY_TYPE_INLINE_TABLE:
	case KEY_TYPE_TABLE_ARRAY:
	default:
		ELEKTRA_ASSERT (0, "Default should be unreachable, all KeyType variations must be handeled explicitly");
		break;
	}

	if (writer->keyStack == NULL)
	{
		return writeNewline (writer);
	}
	return 0;
}

static int handleClosingSequences (Key * key, Writer * writer)
{
	int result = 0;
	while (writer->keyStack != NULL && keyIsBelow (key, writer->keyStack->key) == 0)
	{
		result |= writeClosingSequence (writer->keyStack->type, writer);
		writer->keyStack = popKey (writer->keyStack);
	}
	return result;
}

static int writeScalar (Key * key, Writer * writer)
{
	int result = 0;
	if (!isArrayElement (key)) {
		result |= writeKeyName (key, writer);
		result |= writeEquality (writer);
		result |= writeValue (key, writer);
	} else {
		result |= writeValue (key, writer);
		result |= writeArraySeparator (writer);
	}

	return result;
}

static int writeArrayOpening (Key * key, Writer * writer)
{
	int result = 0;
	if (!isArrayElement (key))
	{
		result |= writeKeyName (key, writer);
		result |= writeEquality (writer);
	}
	result |= writeOpeningSequence (KEY_TYPE_ARRAY, writer);
	return result;
}

static int writeInlineTableOpening (Key * key, Writer * writer)
{
	int result = 0;
	if (!isArrayElement (key)) {
		result |= writeKeyName (key, writer);
		result |= writeEquality (writer);
	}
	result |= writeOpeningSequence (KEY_TYPE_INLINE_TABLE, writer);
	return result;
}

static int writeSimpleTable (Key * key, Writer * writer)
{
	ELEKTRA_ASSERT (writer->keyStack == NULL, "No simple table possible inside array / inline table, but key stack was not empty");
	return writeKeyNameInSequence (key, KEY_TYPE_SIMPLE_TABLE, writer);
}

static int writeTableArray (Key * key, Writer * writer)
{
	ELEKTRA_ASSERT (writer->keyStack == NULL, "No table array possible inside array / inline table, but key stack was not empty");
	return writeKeyNameInSequence (key, KEY_TYPE_TABLE_ARRAY, writer);
}

static int writeKeyNameInSequence (Key * key, KeyType type, Writer * writer)
{
	int result = 0;
	result |= writeOpeningSequence (type, writer);
	result |= writeKeyName (key, writer);
	result |= writeClosingSequence (type, writer);
	return result;
}

static int writeEquality (Writer * writer)
{
	return fputs (" = ", writer->f) == EOF;
}

static int writeNewline (Writer * writer)
{
	return fputc ('\n', writer->f) == EOF;
}

static int writeArraySeparator (Writer * writer)
{
	return fputs (", ", writer->f) == EOF;
}

static int writeOpeningSequence (KeyType type, Writer * writer)
{
	switch (type)
	{
	case KEY_TYPE_ARRAY:
	case KEY_TYPE_SIMPLE_TABLE:
		return fputc ('[', writer->f) == EOF;
	case KEY_TYPE_INLINE_TABLE:
		return fputc ('{', writer->f) == EOF;
	case KEY_TYPE_TABLE_ARRAY:
		return fputs ("[[", writer->f) == EOF;
	case KEY_TYPE_SCALAR:
		ELEKTRA_ASSERT (0, "KEY_TYPE_SCALAR has no opening sequence");
		return 0;
	default:
		ELEKTRA_ASSERT (0, "Unreachable default, all cases should have been handeled");
		return 0;
	}
}

static int writeClosingSequence (KeyType type, Writer * writer)
{
	switch (type)
	{
	case KEY_TYPE_ARRAY:
	case KEY_TYPE_SIMPLE_TABLE:
		return fputc (']', writer->f) == EOF;
	case KEY_TYPE_INLINE_TABLE:
		return fputc ('}', writer->f) == EOF;
	case KEY_TYPE_TABLE_ARRAY:
		return fputs ("]]", writer->f) == EOF;
	case KEY_TYPE_SCALAR:
		ELEKTRA_ASSERT (0, "KEY_TYPE_SCALAR has no closing sequence");
		break;
	default:
		ELEKTRA_ASSERT (0, "Unreachable default, all cases should have been handeled");
		break;
	}
}


static int writeKeyName (Key * key, Writer * writer)
{
	Key * ancestor = writer->keyStack != NULL ? writer->keyStack->key : writer->rootKey;
	char * name = getKeyName (key, ancestor);
	return fputs (name, writer->f) == EOF;
}

static int writeValue (Key * key, Writer * writer)
{
	keyRewindMeta (key);
	const char * valueStr = keyString (key);
	Key * origValue = findMetaKey (key, "origvalue");
	if (origValue != NULL)
	{
		valueStr = keyValue (origValue);
	}
	return fputs (valueStr, writer->f) == EOF;
}

static char * getKeyName (const Key * key, const Key * rootKey)
{
	if (keyCmp (key, rootKey) == 0)
	{
		return NULL;
	}
	size_t len = keyGetUnescapedNameSize (key) - keyGetUnescapedNameSize (rootKey);
	size_t pos = 0;
	char * name = elektraCalloc (sizeof (char) * len);
	const char * keyPart = keyUnescapedName (key) + keyGetUnescapedNameSize (rootKey);
	const char * keyStop = keyUnescapedName (key) + keyGetUnescapedNameSize (key);
	while (keyPart < keyStop)
	{
		strncat (name + pos, keyPart, len);
		pos += elektraStrLen (keyPart) - 1;
		name[pos++] = '.';
		keyPart += elektraStrLen (keyPart);
	}
	if (pos > 0)
	{
		name[pos - 1] = '\0';
	}

	return name;
}

static KeyType getKeyType (Key * key)
{
	Key * meta = findMetaKey (key, "type");
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
		else if (elektraStrCmp (keyString (meta), "inlinetable") == 0)
		{
			return KEY_TYPE_INLINE_TABLE;
		}
	}
	else if ((meta = findMetaKey (key, "array")) != NULL)
	{
		return KEY_TYPE_ARRAY;
	}
	else
	{
		return KEY_TYPE_SCALAR;
	}
}

static bool isArrayElement (Key * key)
{
	return keyName (key)[0] == '#';
}

static KeyStack * pushKey (KeyStack * root, Key * key, KeyType type)
{
	ELEKTRA_ASSERT (root->next != NULL, "root->next must be NULL");
	ELEKTRA_ASSERT (
		type == KEY_TYPE_ARRAY || type == KEY_TYPE_INLINE_TABLE,
		"Only Array and Inline Table keys belong on the stack (since nesting is possible), but wanted to push another KeyType");
	KeyStack * newRoot = elektraCalloc (sizeof (KeyStack));
	if (newRoot == NULL)
	{
		return NULL;
	}
	newRoot->key = key;
	newRoot->type = type;
	newRoot->next = root;
	return newRoot;
}

static KeyStack * popKey (KeyStack * root)
{
	if (root != NULL)
	{
		KeyStack * newRoot = root->next;
		elektraFree (root);
		return newRoot;
	}
	else
	{
		return NULL;
	}
}

static void freeKeyStack (KeyStack * root)
{
	while (root != NULL)
	{
		KeyStack * next = root->next;
		elektraFree (root);
		root = next;
	}
}

static Key * findMetaKey (Key * key, const char * metakeyName)
{
	keyRewindMeta (key);
	for (Key * meta = keyNextMeta (key); meta != NULL; meta = keyNextMeta (key))
	{
		if (elektraStrCmp (keyName (meta), metakeyName) == 0)
		{
			return meta;
		}
	}
	return NULL;
}
