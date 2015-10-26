/**
 * \file
 *
 * \brief A plugin for reading and writing ini files
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
# include "kdbconfig.h"
#endif

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <kdberrors.h>
#include <inih.h>
#include "ini.h"

int elektraIniOpen(Plugin *handle, Key *parentKey);
int elektraIniClose(Plugin *handle, Key *parentKey);

#include "contract.h"

typedef struct {
	const Key *parentKey;	/* the parent key of the result KeySet */
	KeySet *result;			/* the result KeySet */
	char *collectedComment;	/* buffer for collecting comments until a non comment key is reached */
} CallbackHandle;

typedef struct {
	short supportMultiline;	/* defines whether multiline keys are supported */
	short autoSections;		/* defines whether sections for keys 2 levels or more below the parentKey are created */
} IniPluginConfig;

static void flushCollectedComment (CallbackHandle *handle, Key *key)
{
	if (handle->collectedComment)
	{
		keySetMeta (key, "comment", handle->collectedComment);
		free (handle->collectedComment);
		handle->collectedComment = 0;
	}
}

// TODO defined privately in internal.c, API break possible.
// Might consider moving this to the public API as it might be used by more plugins
size_t elektraUnescapeKeyName(const char *source, char *dest);

// TODO: this is very similar to elektraKeyAppendMetaLine in keytometa
static int elektraKeyAppendLine (Key *target, const char *line)
{
	if (!target) return 0;
	if (!line) return 0;


	char *buffer = malloc (keyGetValueSize(target) + strlen (line) + 1);
	if (!buffer) return 0;

	keyGetString(target, buffer, keyGetValueSize(target));
	strcat (buffer, "\n");
	strncat (buffer, line, strlen (line));

	keySetString(target, buffer);
	free (buffer);
	return keyGetValueSize(target);
}

static int iniKeyToElektraKey (void *vhandle, const char *section, const char *name, const char *value, unsigned short lineContinuation)
{

	CallbackHandle *handle = (CallbackHandle *)vhandle;

	Key *appendKey = keyDup (handle->parentKey);

	if (section)
	{
		if (*section != '\0')
		{
			keyAddBaseName(appendKey, section);
		}
	}

	keyAddBaseName (appendKey, name);

	if(value == NULL)
		keySetMeta(appendKey, "ini/empty", "");
	if (!lineContinuation)
	{
		flushCollectedComment (handle, appendKey);
		keySetString (appendKey, value);
		ksAppendKey (handle->result, appendKey);
	}
	else
	{
		Key *existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
		keyDel (appendKey);

		/* something went wrong before because this key should exist */
		if (!existingKey) return -1;

		elektraKeyAppendLine(existingKey, value);
	}


	return 1;
}

static int iniSectionToElektraKey (void *vhandle, const char *section)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;

	Key *appendKey = keyDup (handle->parentKey);
	keySetBinary(appendKey, 0, 0);
	keyAddBaseName(appendKey, section);
	flushCollectedComment (handle, appendKey);
	ksAppendKey(handle->result, appendKey);

	return 1;
}

static int iniCommentToMeta (void *vhandle, const char *comment)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;

	size_t commentSize = strlen (comment) + 1;

	if (!handle->collectedComment)
	{
		handle->collectedComment = malloc (commentSize);

		if (!handle->collectedComment) return 0;

		strncpy (handle->collectedComment, comment, commentSize);
	}
	else
	{
		size_t newCommentSize = strlen (handle->collectedComment) + commentSize + 1;
		handle->collectedComment = realloc (handle->collectedComment, newCommentSize);

		if (!handle->collectedComment) return 0;

		strcat (handle->collectedComment, "\n");
		strncat (handle->collectedComment, comment, newCommentSize);
	}

	return 1;
}



int elektraIniOpen(Plugin *handle, Key *parentKey ELEKTRA_UNUSED)
{
	KeySet *config = elektraPluginGetConfig (handle);
	IniPluginConfig *pluginConfig = (IniPluginConfig *)elektraMalloc (sizeof (IniPluginConfig));
	Key *multilineKey = ksLookupByName (config, "/multiline", KDB_O_NONE);
	Key *autoSectionKey = ksLookupByName(config, "/autosections", KDB_O_NONE);
	pluginConfig->supportMultiline = multilineKey != 0;
	pluginConfig->autoSections = autoSectionKey != 0;
	elektraPluginSetData(handle, pluginConfig);

	return 0;
}


int elektraIniClose(Plugin *handle, Key *parentKey ELEKTRA_UNUSED)
{
	IniPluginConfig *pluginConfig = (IniPluginConfig *)elektraPluginGetData(handle);
	elektraFree(pluginConfig);
	elektraPluginSetData(handle, 0);
	return 0;
}

int elektraIniGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	int errnosave = errno;

	if (!strcmp (keyName (parentKey), "system/elektra/modules/ini"))
	{
		KeySet *info = getPluginContract();

		ksAppend (returned, info);
		ksDel (info);
		return 1;
	}

	FILE *fh = fopen (keyString (parentKey), "r");
	if (!fh)
	{
		ELEKTRA_SET_ERROR_GET(parentKey);
		errno = errnosave;
		return -1;
	}

	KeySet *append = ksNew (0, KS_END);

	CallbackHandle cbHandle;
	cbHandle.parentKey = parentKey;
	cbHandle.result = append;
	cbHandle.collectedComment = 0;
	ksAppendKey (cbHandle.result, keyDup(parentKey));



	struct IniConfig iniConfig;
	iniConfig.keyHandler=iniKeyToElektraKey;
	iniConfig.sectionHandler = iniSectionToElektraKey;
	iniConfig.commentHandler = iniCommentToMeta;

	IniPluginConfig *pluginConfig = elektraPluginGetData(handle);
	iniConfig.supportMultiline = pluginConfig->supportMultiline;

	int ret = ini_parse_file(fh, &iniConfig, &cbHandle);

	fclose (fh);
	errno = errnosave;

	if (ret == 0)
	{
		ksClear(returned);
		ksAppend(returned, cbHandle.result);
		ret = 1;
	}
	else
	{
		switch (ret)
		{
			case -1:
				ELEKTRA_SET_ERROR(9, parentKey, "Unable to open the ini file");
				break;
			case -2:
				ELEKTRA_SET_ERROR(87, parentKey, "Memory allocation error while reading the ini file");
				break;
			default:
				ELEKTRA_SET_ERRORF(98, parentKey, "Could not parse ini file %s. First error at line %d", keyString(parentKey), ret);
				break;
		}
		ret = -1;
	}

	ksDel(cbHandle.result);
	return ret; /* success */
}

// TODO: # and ; comments get mixed up, patch inih to differentiate and
// create comment keys instead of writing meta data. Writing the meta
// data can be done by keytometa then
void writeComments(Key* current, FILE* fh)
{
	const Key* commentMeta = keyGetMeta (current, "comment");
	if (commentMeta)
	{
		size_t commentSize = keyGetValueSize (commentMeta);
		char* comments = malloc (commentSize);
		keyGetString (commentMeta, comments, commentSize);
		char* savePtr = 0;
		char* currentComment = strtok_r (comments, "\n", &savePtr);
		while (currentComment)
		{
			fprintf (fh, ";%s\n", currentComment);
			currentComment = strtok_r (0, "\n", &savePtr);
		}

		free (comments);
	}
}

void writeMultilineKey(Key *key, const char *iniName, FILE *fh)
{
	size_t valueSize = keyGetValueSize(key);
	char *saveptr = 0;
	char *result = 0;
	char *value = malloc (valueSize);
	keyGetString(key, value, valueSize);
	result = strtok_r (value, "\n", &saveptr);

	fprintf (fh, "%s = %s\n", iniName, result);

	while ( (result = strtok_r (0, "\n", &saveptr)) != 0)
	{
		fprintf (fh, "\t%s\n", result);
	}

	free (value);
}

static short isSectionKey(Key *key)
{
	if (!key) return 0;

	return keyIsBinary(key) && !keyValue(key);
}

/**
 * Returns the name of the corresponding ini key based on
 * the structure and parentKey of the supplied key.
 *
 * The returned string has to be freed by the caller
 *
 */
static char *getIniName(KeySet *keys, Key *parent, Key *key)
{
	cursor_t currentCursor = ksGetCursor(keys);

	Key *temp = keyDup(key);
	Key *section;

	do
	{
		keySetBaseName(temp, 0);

		if (!keyCmp(temp, parent))
		{
			/* we reached the parent key, there won't be any more section keys */
			section = parent;
			break;
		}

		section = ksLookup(keys, temp, KDB_O_NONE);
	} while(!isSectionKey(section));

	ksSetCursor(keys, currentCursor);
	keyDel(temp);

	char *buffer = elektraMalloc(keyGetNameSize(key));
	elektraUnescapeKeyName(keyName(key) + keyGetNameSize(section), buffer);

	return buffer;

}

static Key *generateSectionKey(Key *key, Key *parentKey)
{
	Key *sectionKey = keyDup(key);
	while (!keyIsDirectBelow(parentKey, sectionKey) && keyIsBelow(parentKey, sectionKey))
	{
		keySetBaseName(sectionKey, 0);
	}

	keySetBinary(sectionKey, 0, 0);
	return sectionKey;
}

int elektraIniSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	int errnosave = errno;
	int ret = 1;

	FILE *fh = fopen(keyString(parentKey), "w");

	if (!fh)
	{
		ELEKTRA_SET_ERROR_SET(parentKey);
		errno = errnosave;
		return -1;
	}

	IniPluginConfig* pluginConfig = elektraPluginGetData(handle);

	ksRewind (returned);
	Key *current;
	while ((current = ksNext (returned)))
	{
		Key *sectionKey = NULL;
		if (pluginConfig->autoSections && !keyIsDirectBelow(parentKey, current))
		{
			Key *sectionKey = generateSectionKey(current, parentKey);

			cursor_t cursor = ksGetCursor(returned);
			if (!ksLookup(returned, sectionKey, KDB_O_NONE))
			{
				ksAppendKey(returned, sectionKey);
				current = sectionKey;
			}
			else
			{
				keyDel(sectionKey);
				ksSetCursor(returned, cursor);
			}
		}

		if (!strcmp (keyName(current), keyName(parentKey))) continue;
		if(keyIsDirectBelow(parentKey, current))
		{
			if(*(keyString(current)) == NULL)
			{
				sectionKey = keyDup(current);
				keySetBinary(sectionKey, 0, 0);
				current = sectionKey;
			}
		}
		writeComments (current, fh);

		/* find the section the current key belongs to */
		char *iniName;
		if(keyIsDirectBelow(parentKey, current))
			iniName = getIniName(returned, parentKey, current);
		else
			iniName = strdup(keyBaseName(current));
		/* keys with a NULL value are treated as sections */
		if (isSectionKey(current))
		{
			fprintf (fh, "[%s]\n", iniName);
		}
		else
		{
			/* if the key value is only single line, write a singleline INI key */
			if(keyGetMeta(current, "ini/empty"))
			{
				printf("writing empty key\n");
				fprintf(fh, "%s\n", iniName);

			}
			else if (strstr (keyString (current), "\n") == 0)
			{
				fprintf (fh, "%s = %s\n", iniName, keyString (current));
			}
			else
			{
				/* otherwise check that multiline support is enabled and write a multiline INI key */
				if (pluginConfig->supportMultiline)
				{
					writeMultilineKey (current, iniName, fh);
				}
				else
				{
					ELEKTRA_SET_ERROR(97, parentKey,
							"Encountered a multiline value but multiline support is not enabled. "
							"Have a look at kdb info ini for more details");
					ret = -1;
				}
			}
		}
		elektraFree(iniName);
		if (ret < 0) break;
		if(sectionKey)
			keyDel(sectionKey);
	}

	fclose (fh);

	errno = errnosave;
	return ret; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(ini)
{
	return elektraPluginExport("ini",
			ELEKTRA_PLUGIN_OPEN, &elektraIniOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraIniClose,
			ELEKTRA_PLUGIN_GET,	&elektraIniGet,
			ELEKTRA_PLUGIN_SET,	&elektraIniSet,
			ELEKTRA_PLUGIN_END);
}

