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
	Key *parentKey;	/* the parent key of the result KeySet */
	KeySet *result;			/* the result KeySet */
	char *collectedComment;	/* buffer for collecting comments until a non comment key is reached */
} CallbackHandle;

typedef struct {
	short supportMultiline;	/* defines whether multiline keys are supported */
	short autoSections;		/* defines whether sections for keys 2 levels or more below the parentKey are created */
	short keyToMeta;
	short preserveOrder;
	short nestedSections;
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

static Key *createUnescapedKey(Key *key, KeySet *ks, const char *name)
{
	char *localString = strdup(name);
	char *newBaseName = strtok(localString, "/");
	if(newBaseName != NULL)
	{
		keyAddBaseName(key, newBaseName);
		if(!ksLookup(ks, key, KDB_O_NONE))
		{
			ksAppendKey(ks, keyDup(key));
		}
	}
	while(newBaseName != NULL)
	{
		newBaseName = strtok(NULL, "/");
		if(newBaseName != NULL)
		{
			keyAddBaseName(key, newBaseName);
			if(!ksLookup(ks, key, KDB_O_NONE))
			{
				ksAppendKey(ks, keyDup(key));
			}
		}
	}
	free(localString);
	return key;
}

static int iniKeyToElektraKey (void *vhandle, const char *section, const char *name, const char *value, unsigned short toMeta, unsigned short lineContinuation)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;
	Key *appendKey = keyDup (handle->parentKey);
	keySetString(appendKey, NULL);
	keySetMeta(appendKey, "ini/lastSection", 0);
	Key *sectionKey = NULL;

	if (section)
	{
		if (*section != '\0')
		{
			keySetBinary(appendKey, 0, 0);
			keySetMeta(appendKey, "ini/section", 0);
			appendKey = createUnescapedKey(appendKey, handle->result, section);
		}
		sectionKey = ksLookup(handle->result, appendKey, KDB_O_NONE);
		if(sectionKey)
		{
			if(!(keyGetMeta(sectionKey, "ini/section")))
			{
				int lastSectionIndex = atoi(keyString(keyGetMeta(handle->parentKey, "ini/lastSection")));
				++lastSectionIndex;
				char buf[16];
				snprintf(buf, sizeof(buf), "%d", lastSectionIndex);
				keySetMeta(handle->parentKey, "ini/lastSection", buf);
				keySetMeta(appendKey, "ini/section", buf);
				keySetMeta(sectionKey, "ini/section", buf);
				keySetMeta(sectionKey, "ini/lastSection", 0);
			}
		}
	}

	if(toMeta)
	{
		sectionKey = ksLookup(handle->result, appendKey, KDB_O_NONE);
		if(!sectionKey)
			sectionKey = appendKey;
		keySetMeta(sectionKey, name, value);
		ksAppendKey(handle->result, sectionKey);
		keyDel(appendKey);
		return 1;
	}
	sectionKey = ksLookup(handle->result, appendKey, KDB_O_NONE);
	appendKey = createUnescapedKey(appendKey, handle->result, name);
	keySetMeta(appendKey, "ini/section", keyString(keyGetMeta(sectionKey, "ini/section")));
	char buf[16];
	unsigned int lastIndex;
	if(sectionKey)
	{
		lastIndex = atoi(keyString(keyGetMeta(sectionKey, "ini/lastKey")));
		++lastIndex;
		snprintf(buf, sizeof(buf), "%u", lastIndex);
		keySetMeta(sectionKey, "ini/lastKey", buf);
		keySetMeta(appendKey, "order", buf);
		keySetMeta(appendKey, "ini/lastKey", NULL);
	}
	else
	{
		lastIndex = atoi(keyString(keyGetMeta(handle->parentKey, "ini/lastKey")));
		++lastIndex;
		snprintf(buf, sizeof(buf), "%u", lastIndex);
		keySetMeta(handle->parentKey, "ini/lastKey", buf);
		keySetMeta(appendKey, "order", buf);
		keySetMeta(appendKey, "ini/lastKey", NULL);

	}
	keySetMeta(appendKey, "binary", 0);
	if(*value == '\0')
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
	int lastSectionIndex = atoi(keyString(keyGetMeta(handle->parentKey, "ini/lastSection")));
	++lastSectionIndex;
	//TODO: use a constant for the size
	char newSectionIndex[16];
	snprintf(newSectionIndex, sizeof(newSectionIndex), "%d", lastSectionIndex);
	Key *appendKey = keyDup (handle->parentKey);
	keySetString(appendKey, NULL);
	keySetBinary(appendKey, 0, 0);
	keySetMeta(appendKey, "ini/lastKey", "0");
	keySetMeta(appendKey, "ini/lastSection", 0);
	keySetMeta(appendKey, "ini/section", 0);	
	appendKey = createUnescapedKey(appendKey, handle->result, section);
	keySetMeta(appendKey, "ini/section", newSectionIndex);
	keySetMeta(handle->parentKey, "ini/lastSection", newSectionIndex);
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
	Key *toMetaKey = ksLookupByName(config, "/meta", KDB_O_NONE);
	Key *preserveKey = ksLookupByName(config, "/preserveorder", KDB_O_NONE);
	Key *nestedSectionsKey = ksLookupByName(config, "/nestedsections", KDB_O_NONE);
	pluginConfig->nestedSections = nestedSectionsKey != 0;
	pluginConfig->preserveOrder = preserveKey != 0;	
	pluginConfig->supportMultiline = multilineKey != 0;
	pluginConfig->autoSections = autoSectionKey != 0;
	pluginConfig->keyToMeta = toMetaKey != 0;
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
	keySetMeta(parentKey, "ini/section", "0");
	keySetMeta(parentKey, "ini/lastSection", "0");
	keySetMeta(parentKey, "ini/lastKey", "0");
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

	iniConfig.keyToMeta = pluginConfig->keyToMeta;

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

	return keyGetMeta(key, "ini/section");
}

/**
 * Returns the name of the corresponding ini key based on
 * the structure and parentKey of the supplied key.
 *
 * The returned string has to be freed by the caller
 *
 */
static char *getIniName(Key *section, Key *key)
{
	if(!strcmp(keyName(section), keyName(key)))
		return strdup(keyBaseName(key));
	char *buffer = malloc(strlen(keyName(key)) - strlen(keyName(section)));
	char *dest = buffer;
	for(char *ptr = (char *)keyName(key)+strlen(keyName(section))+1; *ptr; ++ptr)
	{
		if(*ptr != '\\')
		{
			*dest = *ptr;
			++dest;
		}
	}
	*dest = 0;
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

	unsigned int lastSectionIndex = atoi(keyString(keyGetMeta(parentKey, "ini/section")));
	++lastSectionIndex;
	char buf[16];
	snprintf(buf, sizeof(buf), "%u", lastSectionIndex);
	keySetMeta(parentKey, "ini/section", buf);
	keySetMeta(sectionKey, "ini/section", buf);

	return sectionKey;
}

/**
 * Loops through all metakeys belonging to the (section)key.
 * If the metakey doesn't match any of the reserved keywords (order, ini/empty, binary):
 * write it to the file.
 */

static void writeMeta(Key *key, FILE *fh)
{
	keyRewindMeta(key);
	while(keyNextMeta(key) != NULL)
	{
		const Key *meta = keyCurrentMeta(key);
		if(strcmp(keyName(meta), "ini/empty") && strcmp(keyName(meta), "binary") && strcmp(keyName(meta), "order"))
		{
			fprintf(fh, "%s = %s\n", keyName(meta), keyString(meta));
		}
	}
}

static Key *nextKeyBySectionIndex(KeySet *returned, int *index)
{
	ksRewind(returned);
	Key *cur;
	while((cur = ksNext(returned)) != NULL)
	{
		if(atoi(keyString(keyGetMeta(cur, "ini/section"))) == *index)
		{
			++(*index);
			return cur;
		}
	}
	return NULL;
}
static Key *nextKeyByOrderNumber(KeySet *returned, Key *sectionKey, int *index)
{
	ksRewind(returned);
	Key *cur;
	int sectionIndex = atoi(keyString(keyGetMeta(sectionKey, "ini/section")));
	while((cur = ksNext(returned)) != NULL)
	{
		if(!keyIsBelow(sectionKey, cur))
			continue;
		if((atoi((char *)(keyString(keyGetMeta(cur, "ini/section"))))) != sectionIndex)
			continue;
		if((atoi(keyString(keyGetMeta(cur, "order")))) == ((int)(*index)))
		{
			++(*index);
			return cur;
		}
	}
	return NULL;
}
void printKeyTree(FILE *fp, Key *parentKey, KeySet *returned, IniPluginConfig *pluginConfig)
{
	int order = 1;
	int sectionIndex = 0;
	int endSectionIndex = atoi(keyString(keyGetMeta(parentKey, "ini/lastSection")));
	KeySet *workingKS = ksDup(returned);
	Key *sectionKey;
	while(ksGetSize(workingKS) > 0)
	{
		sectionKey = nextKeyBySectionIndex(workingKS, &sectionIndex);
		if(sectionKey == NULL)
			break;
		KeySet *cutKS = ksCut(workingKS, sectionKey);
		order = 1;
		if(!strcmp(keyName(sectionKey), keyName(parentKey)))
		{
			Key *key;
			while((key = nextKeyByOrderNumber(cutKS, sectionKey, &order)) != NULL)
			{
				char *iniName = getIniName(sectionKey, key);
				fprintf(fp, "%s = %s\n", iniName, keyString(key));
				keyDel(ksLookup(cutKS, key, KDB_O_POP));
				free(iniName);
			}
		}
		else
		{
			char *iniName = getIniName(parentKey, sectionKey);
			fprintf(fp, "[%s]\n", iniName);
			free(iniName);
			Key *key;
			while((key = nextKeyByOrderNumber(cutKS, sectionKey, &order)) != NULL)
			{
				iniName = getIniName(sectionKey, key);
				fprintf(fp, "%s = %s\n", iniName, keyString(key));
				keyDel(ksLookup(cutKS, key, KDB_O_POP));
				free(iniName);
			}
		}
		keyDel(ksLookup(cutKS, sectionKey, KDB_O_POP));
		ksAppend(workingKS, cutKS);
	}
}
static void outputDebug(KeySet *ks)
{
	Key *cur;
	while((cur = ksNext(ks)) != NULL)
	{
		fprintf(stderr, "%s:(%s)\t", keyName(cur), keyString(cur));
		keyRewindMeta(cur);
		Key *meta;
		while((meta = keyNextMeta(cur)) != NULL)
		{
			fprintf(stderr, ", %s: %s", keyName(meta), keyValue(meta));
		}
		fprintf(stderr, "\n");
	}
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
	ksRewind(returned);
	outputDebug(returned);	
	ksRewind (returned);
	printKeyTree(fh, parentKey, ksDup(returned), pluginConfig);
	
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

