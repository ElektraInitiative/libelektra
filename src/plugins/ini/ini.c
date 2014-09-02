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

#define ELEKTRA_SET_GENERAL_ERROR(id, parentKey, message) \
	do { \
		ELEKTRA_SET_ERROR(id, parentKey, message); \
		errno = errnosave; \
	} while (0)

#define ELEKTRA_SET_ERRNO_ERROR(id, parentKey) \
	ELEKTRA_SET_GENERAL_ERROR(id, parentKey, strerror(errno))


typedef struct {
	const Key *parentKey;
	KeySet *result;
	char *collectedComment;
} CallbackHandle;

static void writeCommentToMeta (CallbackHandle *handle, Key *key)
{
	if (handle->collectedComment)
	{
		keySetMeta (key, "comment", handle->collectedComment);
		free (handle->collectedComment);
		handle->collectedComment = 0;
	}
}

static int iniKeyToElektraKey (void *vhandle, const char *section, const char *name, const char *value)
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
	writeCommentToMeta (handle, appendKey);
	keySetString (appendKey, value);
	ksAppendKey (handle->result, appendKey);

	return 1;
}

static int iniSectionToElektraKey (void *vhandle, const char *section)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;

	Key *appendKey = keyDup (handle->parentKey);
	keySetString(appendKey, 0);

	keyAddBaseName(appendKey, section);
	writeCommentToMeta (handle, appendKey);
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
		ELEKTRA_SET_ERRNO_ERROR(9, parentKey);
		return -1;
	}

	KeySet *append = ksNew (0, KS_END);

	CallbackHandle cbHandle;
	cbHandle.parentKey = parentKey;
	cbHandle.result = append;
	cbHandle.collectedComment = 0;
	ksAppendKey (cbHandle.result, keyDup(parentKey));
	int ret = ini_parse_file(fh,iniKeyToElektraKey, iniSectionToElektraKey, iniCommentToMeta, &cbHandle);

	fclose (fh);

	if (ret < 0)
	{
		ELEKTRA_SET_GENERAL_ERROR(87, parentKey, "Unable to parse the ini file");
		return -1;
	}

	ksClear(returned);
	ksAppend(returned, cbHandle.result);
	ksDel(cbHandle.result);
	errno = errnosave;
	return 1; /* success */
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

int elektraIniSet(Plugin *handle ELEKTRA_UNUSED, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	int errnosave = errno;

	FILE *fh = fopen(keyString(parentKey), "w");

	if (!fh)
	{
		ELEKTRA_SET_ERRNO_ERROR(9, parentKey);
		return -1;
	}

	ksRewind (returned);
	Key *current;
	while ((current = ksNext (returned)))
	{
		if (!strcmp (keyName(current), keyName(parentKey))) continue;

		writeComments (current, fh);
		size_t baseNameSize = keyGetBaseNameSize(current);
		char *name = malloc (baseNameSize);
		keyGetBaseName(current, name, baseNameSize);

		if (keyIsDir(current))
		{
			fprintf (fh, "[%s]\n", name);
		}
		else
		{
			fprintf (fh, "%s = %s\n", name, keyString(current));
		}

		free (name);
	}

	fclose (fh);

	errno = errnosave;
	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(ini)
{
	return elektraPluginExport("ini",
		ELEKTRA_PLUGIN_GET,	&elektraIniGet,
		ELEKTRA_PLUGIN_SET,	&elektraIniSet,
		ELEKTRA_PLUGIN_END);
}

