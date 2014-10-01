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
#include <string.h>
#include <stdlib.h>
#include <kdberrors.h>
#include <kdbextension.h>
#include <inih.h>
#include "ini.h"

#include "contract.h"

typedef struct {
	const Key *parentKey;	/* the parent key of the result KeySet */
	KeySet *result;			/* the result KeySet */
	char *collectedComment;	/* buffer for collecting comments until a non comment key is reached */
} CallbackHandle;

static void flushCollectedComment (CallbackHandle *handle, Key *key)
{
	if (handle->collectedComment)
	{
		keySetMeta (key, "comment", handle->collectedComment);
		free (handle->collectedComment);
		handle->collectedComment = 0;
	}
}

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

	if (!lineContinuation)
	{
		flushCollectedComment (handle, appendKey);
		keySetString (appendKey, value);
		ksAppendKey (handle->result, appendKey);
	}
	else
	{
		Key *existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE);

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
	keySetString(appendKey, 0);

	keyAddBaseName(appendKey, section);
	flushCollectedComment (handle, appendKey);
	keySetDir(appendKey);
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

int elektraIniGet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
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
		ELEKTRA_SET_ERROR(9, parentKey, strerror (errno));
		errno = errnosave;
		return -1;
	}

	KeySet *append = ksNew (0, KS_END);

	CallbackHandle cbHandle;
	cbHandle.parentKey = parentKey;
	cbHandle.result = append;
	cbHandle.collectedComment = 0;
	ksAppendKey (cbHandle.result, keyDup(parentKey));

	KeySet *config = elektraPluginGetConfig (handle);
	Key* multilineKey = ksLookupByName (config, "/multiline", 0);

	struct IniConfig iniConfig;
	iniConfig.keyHandler=iniKeyToElektraKey;
	iniConfig.sectionHandler = iniSectionToElektraKey;
	iniConfig.commentHandler = iniCommentToMeta;
	iniConfig.supportMultiline = multilineKey != 0;

	int ret = ini_parse_file(fh, &iniConfig, &cbHandle);

	fclose (fh);
	errno = errnosave;

	if (ret >= 0)
	{
		ksClear(returned);
		ksAppend(returned, cbHandle.result);
		ret = 1;
	}
	else
	{
		ELEKTRA_SET_ERROR(87, parentKey, "Unable to parse the ini file");
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
		char* savePtr;
		char* currentComment = strtok_r (comments, "\n", &savePtr);
		while (currentComment)
		{
			fprintf (fh, ";%s\n", currentComment);
			currentComment = strtok_r (0, "\n", &savePtr);
		}

		free (comments);
	}
}

void writeMultilineKey(Key *key, FILE *fh)
{
	size_t valueSize = keyGetValueSize(key);
	char *saveptr = 0;
	char *result = 0;
	char *value = malloc (valueSize);
	keyGetString(key, value, valueSize);
	result = strtok_r (value, "\n", &saveptr);

	fprintf (fh, "%s = %s\n", keyBaseName(key), result);

	while ( (result = strtok_r (0, "\n", &saveptr)) != 0)
	{
		fprintf (fh, "\t%s\n", result);
	}

	free (value);
}

int elektraIniSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	int errnosave = errno;
	int ret = 1;

	FILE *fh = fopen(keyString(parentKey), "w");

	if (!fh)
	{
		ELEKTRA_SET_ERROR(9, parentKey, strerror(errno));
		errno = errnosave;
		return -1;
	}

	KeySet *config = elektraPluginGetConfig (handle);
	Key* multilineKey = ksLookupByName (config, "/multiline", 0);

	ksRewind (returned);
	Key *current;
	while ((current = ksNext (returned)))
	{
		if (!strcmp (keyName(current), keyName(parentKey))) continue;

		writeComments (current, fh);

		if (keyIsDir(current))
		{
			fprintf (fh, "[%s]\n", keyBaseName(current));
		}
		else
		{
			if (strstr (keyString (current), "\n") == 0)
			{
				fprintf (fh, "%s = %s\n", keyBaseName(current), keyString(current));
			}
			else
			{
				if (multilineKey)
				{
					writeMultilineKey(current, fh);
				}
				else
				{
					ELEKTRA_SET_ERROR(97, parentKey, "Encountered a multiline value but multiline support is not enabled");
					ret = -1;
				}
			}
		}

		if (ret < 0) break;
	}

	fclose (fh);

	errno = errnosave;
	return ret; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(ini)
{
	return elektraPluginExport("ini",
		ELEKTRA_PLUGIN_GET,	&elektraIniGet,
		ELEKTRA_PLUGIN_SET,	&elektraIniSet,
		ELEKTRA_PLUGIN_END);
}

