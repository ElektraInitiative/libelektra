#include <kdbhelper.h>
#include <stdio.h>
#include <string.h>
#include <kdbassert.h>

#include "write.h"

typedef struct KeyStack_
{
	Key * key;
	struct KeyStack_ * next;
} KeyStack;

typedef struct
{
	char * filename;
	FILE * f;
	Key * rootKey;
	KeyStack * inlineTableStack;
} Writer;

static int writeKey (Key * key, Writer * writer);
static int writeKeyName (Key * key, Writer * writer);
static int writeValue (Key * key, Writer * writer);
static int writeEquality (Writer * writer);
static char * getKeyName (const Key * key, const Key * rootKey);
static Key * findMetaKey (Key * key, const char * metakeyName);
static Writer * createWriter (Key * rootKey);
static void destroyWriter (Writer * writer);
static KeyStack * pushKey (KeyStack * root, Key * key);
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
	if (writer->filename == 0) {
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
		freeKeyStack (writer->inlineTableStack);
	}
}


static int writeKey (Key * key, Writer * writer)
{

	return 0;
}

static int writeEquality (Writer * writer)
{
	return fputs (" = ", writer->f) == EOF;
}

static int writeKeyName (Key * key, Writer * writer)
{
	char * name = getKeyName (key, writer->rootKey);
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

static KeyStack * pushKey (KeyStack * root, Key * key)
{
	ELEKTRA_ASSERT (root->next != NULL, "root->next must be NULL");
	KeyStack * newRoot = elektraCalloc (sizeof(KeyStack));
	if (newRoot == NULL) {
		return NULL;
	}
	newRoot->key = key;
	newRoot->next = root;
	return newRoot;
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
