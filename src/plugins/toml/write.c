#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

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

static Writer * createWriter (Key * rootKey, KeySet * keys);
static void destroyWriter (Writer * writer);
static int writeKeys (Writer * writer);
static int writeAssignment (Key * parent, Key * key, Writer * writer);
static int writeSimpleTable (Key * parent, Key * key, Writer * writer);
static int writeArrayBody (Key * parent, Key * key, Writer * writer);
static int writeArrayElements (Key * parent, Writer * writer);
static int writeScalar (Key * key, Writer * writer);
static int writeRelativeKeyName (Key * parent, Key * key, Writer * writer);
static int writeNewline (Writer * writer);
static int writeArraySeparator (Writer * writer);
static bool isArray (Key * key);
static bool isType (Key * key, const char * type);
static bool isTableArray (Key * key);
static char * getRelativeKeyName (const Key * parent, const Key * key);
static KeyType getKeyType (Key * key);
static const Key * findMetaKey (Key * key, const char * metakeyName);

int tomlWrite (KeySet * keys, Key * rootKey)
{
	Writer * w = createWriter (rootKey, keys);
	if (w == NULL)
	{
		return 1;
	}
	int result = 0;
	result |= writeKeys (w);

	destroyWriter (w);
	return result;
}

static Writer * createWriter (Key * rootKey, KeySet * keys)
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

static int writeKeys (Writer * writer)
{
	int result = 0;
	ksRewind (writer->keys);
	Key * key = ksNext (writer->keys);
	while (result == 0 && key != NULL)
	{
		if (keyCmp (key, writer->rootKey) == 0)
		{
			key = ksNext (writer->keys);
			continue;
		}
		printf("LOOP KEY = %s\n", keyName (key));
		switch (getKeyType (key))
		{
		case KEY_TYPE_ASSIGNMENT:
			result |= writeAssignment (writer->rootKey, key, writer);
			result |= writeNewline (writer);
			break;
		case KEY_TYPE_SIMPLE_TABLE:
			break;
		case KEY_TYPE_TABLE_ARRAY:
			break;
		}
		key = ksCurrent (writer->keys);
		printf("AFTER KEY = %s, type = %d, result = %d\n", keyName (key), getKeyType (key), result);
	}
}

static int writeAssignment (Key * parent, Key * key, Writer * writer)
{
	int result = 0;

	result |= writeRelativeKeyName (parent, key, writer);
	result |= fputs(" = ", writer->f) == EOF;
	if (isArray (key))
	{
		result |= writeArrayBody (parent, key, writer);
	}
	else
	{
	}
	return result;
}

static int writeSimpleTable (Key * parent, Key * key, Writer * writer)
{
	int result = 0;

	return result;
}

static int writeSimpleTableArray (Key * parent, Key * key, Writer * writer)
{
	int result = 0;

	return result;
}

static int writeArrayBody (Key * parent, Key * key, Writer * writer)
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
		printf("Write array element\n");
		result |= writeScalar (key, writer);
		result |= fputc (',', writer->f) == EOF;
		key = ksNext (writer->keys);
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
		printf("Write relative name: %s\n", relativeName);
		elektraFree (relativeName);
	}
	return result;
}

static int writeScalar (Key * key, Writer * writer)
{
	keyRewindMeta (key);
	const char * valueStr = keyString (key);
	const Key * origValue = findMetaKey (key, "origvalue");
	if (origValue != NULL)
	{
		valueStr = keyValue (origValue);
	}
	printf("Write scalar: %s\n", valueStr);
	return fputs (valueStr, writer->f) == EOF;
}

static int writeAssignementChar (Writer * writer)
{
	return fputs (" = ", writer->f) == EOF;
}

static int writeNewline (Writer * writer)
{
	return fputc ('\n', writer->f) == EOF;
}

static char * getRelativeKeyName (const Key * parent, const Key * key)
{
	if (keyIsBelow (parent, key) == 0)
	{
		return NULL;
	}
	size_t len = keyGetUnescapedNameSize (key) - keyGetUnescapedNameSize (parent);
	size_t pos = 0;
	char * name = elektraCalloc (sizeof (char) * len);
	const char * keyPart = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (parent);
	const char * keyStop = ((const char *) keyUnescapedName (key)) + keyGetUnescapedNameSize (key);
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
	const Key * meta = findMetaKey (key, "type");
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

static bool isArray (Key * key)
{
	return findMetaKey (key, "array") != NULL;
}

static bool isInlineArray (Key * key)
{
	return isType (key, "inlinearray");
}

static bool isTableArray (Key * key)
{
	return isType (key, "tablearray");
}

static bool isType (Key * key, const char * type)
{
	const Key * meta = findMetaKey (key, "type");
	if (meta == NULL)
	{
		return false;
	}
	return elektraStrCmp (keyString (meta), type) == 0;
}

static const Key * findMetaKey (Key * key, const char * metakeyName)
{
	keyRewindMeta (key);
	for (const Key * meta = keyNextMeta (key); meta != NULL; meta = keyNextMeta (key))
	{
		if (elektraStrCmp (keyName (meta), metakeyName) == 0)
		{
			return meta;
		}
	}
	return NULL;
}
