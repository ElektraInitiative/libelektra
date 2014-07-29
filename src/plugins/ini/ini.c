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
#include "lib/inih.h"
#include "ini.h"

#include "contract.h"


#define ELEKTRA_SET_GENERAL_ERROR_IF(id, parentKey, message, condition) \
	do { \
		if (condition) \
		{ \
			ELEKTRA_SET_ERROR(id, parentKey, message); \
			errno = errnosave; \
			return -1; \
		} \
	} while (0)

#define ELEKTRA_SET_ERRNO_ERROR_IF(id, parentKey, condition) \
	ELEKTRA_SET_GENERAL_ERROR_IF(id, parentKey, strerror(errno), condition)


typedef struct {
	const Key *parentKey;
	KeySet *result;
	char *collectedComment;
} Configuration;

static void writeCommentToMeta (Configuration *config, Key *key)
{
	if (config->collectedComment)
	{
		keySetMeta (key, "comment", config->collectedComment);
		free (config->collectedComment);
		config->collectedComment = 0;
	}
}

static int iniKeyToElektraKey (void *vconfig, const char *section, const char *name, const char *value)
{

	Configuration *config = (Configuration *)vconfig;

	Key *appendKey = keyDup (config->parentKey);

	if (section)
	{
		keyAddBaseName(appendKey, section);
	}

	keyAddBaseName (appendKey, name);
	writeCommentToMeta (config, appendKey);
	keySetString (appendKey, value);
	ksAppendKey (config->result, appendKey);

	return 1;
}

static int iniSectionToElektraKey (void *vconfig, const char *section)
{
	Configuration *config = (Configuration *)vconfig;

	Key *appendKey = keyDup (config->parentKey);

	keyAddBaseName(appendKey, section);
	writeCommentToMeta (config, appendKey);
	keySetDir(appendKey);
	ksAppendKey(config->result, appendKey);

	return 1;
}

static int iniCommentToMeta (void *vconfig, const char *comment)
{
	Configuration *config = (Configuration *)vconfig;

	size_t commentSize = strlen (comment) + 1;

	if (!config->collectedComment)
	{
		config->collectedComment = malloc (commentSize);

		if (!config->collectedComment) return 0;

		strncpy (config->collectedComment, comment, commentSize);
	}
	else
	{
		size_t newCommentSize = strlen (config->collectedComment) + commentSize + 1;
		config->collectedComment = realloc (config->collectedComment, newCommentSize);

		if (!config->collectedComment) return 0;

		strcat (config->collectedComment, "\n");
		strncat (config->collectedComment, comment, newCommentSize);
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
	ELEKTRA_SET_ERRNO_ERROR_IF(9, parentKey, !fh);

	KeySet *append = ksNew (ksGetSize (returned) * 2, KS_END);

	Configuration config;
	config.parentKey = parentKey;
	config.result = append;
	config.collectedComment = 0;

	int ret = ini_parse_file(fh,iniKeyToElektraKey, iniSectionToElektraKey, iniCommentToMeta, &config);

	fclose (fh);
	ELEKTRA_SET_GENERAL_ERROR_IF(87, parentKey, "Unable to parse the ini file", ret < 0);

	ksClear(returned);
	ksAppend(returned, config.result);
	ksDel(config.result);
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

	ELEKTRA_SET_ERRNO_ERROR_IF(9, parentKey, !fh);

	ksRewind (returned);
	Key *current;
	while ((current = ksNext (returned)))
	{
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

