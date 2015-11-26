/**
 * @file
 *
 * @brief A plugin for reading and writing ini files
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

#define INTERNAL_ROOT_SECTION "GLOBALROOT"

typedef struct {
	const Key *parentKey;	/* the parent key of the result KeySet */
	KeySet *result;			/* the result KeySet */
	char *collectedComment;	/* buffer for collecting comments until a non comment key is reached */
} CallbackHandle;

typedef struct {
	short supportMultiline;	/* defines whether multiline keys are supported */
	short autoSections;		/* defines whether sections for keys 2 levels or more below the parentKey are created */
	short keyToMeta;
	short preserverOrder;
} IniPluginConfig;

static void flushCollectedComment (CallbackHandle *handle, Key *key)
{
	if (handle->collectedComment)
	{
		keySetMeta (key, "comment", handle->collectedComment);
		elektraFree (handle->collectedComment);
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


	char *buffer = elektraMalloc (keyGetValueSize(target) + strlen (line) + 1);
	if (!buffer) return 0;

	keyGetString(target, buffer, keyGetValueSize(target));
	strcat (buffer, "\n");
	strncat (buffer, line, strlen (line));

	keySetString(target, buffer);
	elektraFree (buffer);
	return keyGetValueSize(target);
}

static Key *createUnescapedKey(Key *key, const char *name)
{
	char *localString = strdup(name);
	char *newBaseName = strtok(localString, "/");
	if (newBaseName != NULL)
		keyAddBaseName(key, newBaseName);
	while (newBaseName != NULL)
	{
		newBaseName = strtok(NULL, "/");
		if (newBaseName != NULL)
		{
			keyAddBaseName(key, newBaseName);
		}
	}
	elektraFree (localString);
	return key;
}
static void setSectionNumber(Key *parentKey, Key *key, KeySet *ks)
{
	if(!strcmp(keyBaseName(key), INTERNAL_ROOT_SECTION))
	{
		Key *tmpKey = keyDup(key);
		keySetMeta(tmpKey, "ini/section", "0");
		keySetMeta(key, "ini/section", "0");
		keySetString(tmpKey, 0);
		ksAppendKey(ks, tmpKey);
		keyDel(tmpKey);
		return;
	}

	Key *lookupKey = keyDup(key);
	Key *lastKey = keyDup(lookupKey);

	while(1)
	{
		if(!strcmp(keyName(lookupKey), keyName(parentKey)))
		{
			if(keyGetMeta(parentKey, "ini/lastSection"))
			{
				long previousSection = atol(keyString(keyGetMeta(parentKey, "ini/lastSection")));
				++previousSection;
				char buffer[21]; //20 digits + \0
				snprintf(buffer, sizeof(buffer), "%ld", previousSection);
				keySetMeta(parentKey, "ini/lastSection", buffer);
				keySetMeta(key, "ini/section", buffer);
			}
			else
			{
				keySetMeta(parentKey, "ini/lastSection", "1");
				keySetMeta(parentKey, "ini/section", "0");
				keySetMeta(key, "ini/section", "1");
			}
			keySetMeta(lastKey, "ini/section", keyString(keyGetMeta(key, "ini/section")));
			ksAppendKey(ks, lastKey);
			break;
		}
		if(keyGetMeta(ksLookup(ks, lookupKey, KDB_O_NONE), "ini/section"))
		{
			keySetMeta(key, "ini/section", keyString(keyGetMeta(ksLookup(ks, lookupKey, KDB_O_NONE), "ini/section")));
			break;
		}
		keySetName(lastKey, keyName(lookupKey));
		keyAddName(lookupKey, "..");
	}
	keyDel(lookupKey);
	keyDel(lastKey);
}

static void setOrderNumber(Key *parentKey, Key *key)
{
	long order = atol(keyString(keyGetMeta(parentKey, "order")));
	++order;
	char buffer[21];
	snprintf(buffer, sizeof(buffer), "%09d", order);
	keySetMeta(key, "order", buffer);
	keySetMeta(parentKey, "order", buffer);
}

static void insertNewKeyIntoExistendOrder(Key *key, KeySet *ks)
{
	if(keyGetMeta(ksLookup(ks, key, KDB_O_NONE), "order"))
		return;
	ksRewind(ks);	
	Key *curKey;
	Key *prevKey;
	while((curKey = ksNext(ks)) != NULL)
	{
		if(!strcmp(keyName(curKey), keyName(key)))
		{
			const char *oldOrder = "1";
			if(keyGetMeta(prevKey, "order"))
				oldOrder = keyString(keyGetMeta(prevKey, "order"));
			char *newOrder = elektraMalloc(elektraStrLen(oldOrder)+2);
			sprintf(newOrder, "%s/1", oldOrder);
			keySetMeta(key, "order", newOrder);
			elektraFree(newOrder);
		}	
		prevKey = curKey;
	}
}
static int iniKeyToElektraKey (void *vhandle, const char *section, const char *name, const char *value, unsigned short lineContinuation)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;
	Key *appendKey = keyDup (handle->parentKey);
	keySetMeta(appendKey, "ini/lastSection", 0);
	Key *sectionKey;
	if(!section || *section == '\0')
	{
		section = INTERNAL_ROOT_SECTION;
	}
	sectionKey = createUnescapedKey(appendKey, section);
	setSectionNumber(handle->parentKey, appendKey, handle->result);
	keySetMeta(appendKey, "ini/section", 0);
	appendKey = createUnescapedKey(appendKey, name);
	setSectionNumber(handle->parentKey, appendKey, handle->result);
	setOrderNumber(handle->parentKey, appendKey);
	if (!lineContinuation)
	{
		flushCollectedComment (handle, appendKey);
		keySetString (appendKey, value);
		keySetMeta(appendKey, "ini/key", "");
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

static short isIniKey(Key *key)
{
	if(!key) return 0;
	if(keyGetMeta(key, "ini/key"))
		return 1;
	else
		return 0;
}

static short isSectionKey(Key *key)
{
	if (!key) return 0;
	if(keyIsBinary(key))
		return 1;
	else 
		return 0;
}

static int iniSectionToElektraKey (void *vhandle, const char *section)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;
	Key *appendKey = keyDup (handle->parentKey);
	keySetMeta(appendKey, "ini/lastSection", 0);
	createUnescapedKey(appendKey, section);
	setSectionNumber(handle->parentKey, appendKey, handle->result);
	setOrderNumber(handle->parentKey, appendKey);
	keySetBinary(appendKey, 0, 0);	
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
		handle->collectedComment = elektraMalloc (commentSize);

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

static void outputDebug(KeySet *ks)
{
	Key *cur;
	while((cur = ksNext(ks)) != NULL)
	{
		fprintf(stderr, "%s:(%s)\t", keyName(cur), keyString(cur));
		fprintf(stderr, " sync: %d", keyNeedSync(cur));
		keyRewindMeta(cur);
		Key *meta;
		while((meta = keyNextMeta(cur)) != NULL)
		{
			fprintf(stderr, ", %s: %s", keyName(meta), keyValue(meta));
		}
		fprintf(stderr, "\n");
	}
}
static char *findParent(Key *parentKey, Key *searchkey, KeySet *ks)
{
	Key *key = keyDup(searchkey);
	Key *lookedUp;
	while(strcmp(keyName(key), keyName(parentKey)))
	{
		if(!strcmp(keyName(key), keyName(searchkey)))
		{
			keyAddName(key, "..");
			continue;
		}
		lookedUp = ksLookup(ks, key, KDB_O_NONE);
		if(lookedUp)
		{
			if(isSectionKey(lookedUp))
				break;
		}
		keyAddName(key, "..");
	}
	lookedUp = ksLookup(ks, key, KDB_O_NONE);
	keyDel(key);
	ksDel(ks);
	return keyName(lookedUp);
}
static void setParents(KeySet *ks, Key *parentKey)
{
	Key *cur;
	ksRewind(ks);
	while((cur = ksNext(ks)) != NULL)
	{
		const char *parentName = findParent(parentKey, cur, ksDup(ks));
		keySetMeta(cur, "parent", parentName);
	}
}
int elektraIniGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	int errnosave = errno;
	keySetMeta(parentKey, "ini/section", "0");
	keySetMeta(parentKey, "ini/lastSection", "0");

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
	setParents(cbHandle.result, cbHandle.parentKey);

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
	ksRewind(returned);
	outputDebug(returned);
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
		char* comments = elektraMalloc (commentSize);
		keyGetString (commentMeta, comments, commentSize);
		char* savePtr = 0;
		char* currentComment = strtok_r (comments, "\n", &savePtr);
		while (currentComment)
		{
			fprintf (fh, ";%s\n", currentComment);
			currentComment = strtok_r (0, "\n", &savePtr);
		}

		elektraFree (comments);
	}
}

void writeMultilineKey(Key *key, const char *iniName, FILE *fh)
{
	size_t valueSize = keyGetValueSize(key);
	char *saveptr = 0;
	char *result = 0;
	char *value = elektraMalloc (valueSize);
	keyGetString(key, value, valueSize);
	result = strtok_r (value, "\n", &saveptr);

	fprintf (fh, "%s = %s\n", iniName, result);

	while ( (result = strtok_r (0, "\n", &saveptr)) != 0)
	{
		fprintf (fh, "\t%s\n", result);
	}

	elektraFree (value);
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
	if (!strcmp(keyName(section), keyName(key)))
		return strdup(keyBaseName(key));
	char *buffer = elektraMalloc(strlen(keyName(key)) - strlen(keyName(section)));
	char *dest = buffer;
	for (char *ptr = (char *)keyName(key)+strlen(keyName(section))+1; *ptr; ++ptr)
	{
		if (*ptr != '\\')
		{
			*dest = *ptr;
			++dest;
		}
	}
	*dest = 0;
	return buffer;
}

/**
 * Loops through all metakeys belonging to the (section)key.
 * If the metakey doesn't match any of the reserved keywords (order, ini/empty, binary):
 * write it to the file.
 */

static void writeMeta(Key *key, FILE *fh)
{
	keyRewindMeta(key);
	while (keyNextMeta(key) != NULL)
	{
		const Key *meta = keyCurrentMeta(key);
		if (strcmp(keyName(meta), "ini/empty") && strcmp(keyName(meta), "binary") && strcmp(keyName(meta), "order") && strcmp(keyName(meta), "ini/noautosection"))
		{
			fprintf(fh, "%s = %s\n", keyName(meta), keyString(meta));
		}
	}
}
void insertIntoKS(Key *parentKey, Key *cur, KeySet *newKS)
{
		
	if(keyIsBinary(cur))
	{
		// create new section here
		const char *sectionName = keyName(cur)+strlen(keyName(parentKey))+1;
		const char *oldSectionNumber = strdup(keyString(keyGetMeta(parentKey, "ini/lastSection")));
		Key *appendKey = keyDup(parentKey);
		keySetMeta(appendKey, "ini/lastSection", 0);
		keySetString(appendKey, 0);
		keySetMeta(appendKey, "order", 0);
		keySetMeta(appendKey, "binary", 0);
		createUnescapedKey(appendKey, sectionName);
		setSectionNumber(parentKey, appendKey, newKS);
		keySetBinary(appendKey, 0, 0);
		ksAppendKey(newKS, appendKey);
		if(atoi(oldSectionNumber) < atoi(keyString(keyGetMeta(appendKey, "ini/section"))))
			setOrderNumber(parentKey, appendKey);
		else
		{
			int lastOrderNumber = -1;
			int sectionNumber = atoi(keyString(keyGetMeta(appendKey, "ini/section")));
			KeySet *searchKS = ksDup(newKS);
			ksRewind(searchKS);
			Key *looking;
			while((looking = ksNext(searchKS)) != NULL)
			{
				if(atoi(keyString(keyGetMeta(looking, "ini/section"))) == sectionNumber)
					break;
			}
			KeySet *cutKS = ksCut(searchKS, looking);
			ksRewind(cutKS);
			while((looking = ksNext(cutKS)) != NULL)
			{
				if(atoi(keyString(keyGetMeta(looking, "order"))) > lastOrderNumber)
					lastOrderNumber = atoi(keyString(keyGetMeta(looking, "order")));
			}
			char buffer[21];
			snprintf(buffer, sizeof(buffer), "%09d/1", lastOrderNumber);
			keySetMeta(appendKey, "order", buffer);
			ksDel(cutKS);
			ksDel(searchKS);
		}
		keyDel(cur);
		free(oldSectionNumber);

	}
	else if(keyIsDirectBelow(parentKey, cur))
	{
		// create global key here
		const char *name = keyName(cur)+strlen(keyName(parentKey))+1;
		const char *sectionName = INTERNAL_ROOT_SECTION;
		Key *appendKey = keyDup(parentKey);
		keySetMeta(appendKey, "ini/lastSection", 0);
		keySetString(appendKey, 0);
		keySetMeta(appendKey, "order", 0);
		keySetMeta(appendKey, "binary", 0);
		createUnescapedKey(appendKey, sectionName);
		setSectionNumber(parentKey, appendKey, newKS);
		createUnescapedKey(appendKey, name);
		keySetMeta(appendKey, "ini/key", "");
		ksAppendKey(newKS, appendKey);
		insertNewKeyIntoExistendOrder(appendKey, newKS);
		keyDel(cur);
	}
	else
	{
		Key *sectionKey = keyDup(cur);
		Key *appendKey = keyDup(parentKey);
		keySetMeta(appendKey, "ini/lastSection", 0);
		keySetMeta(appendKey, "order", 0);
		keySetString(appendKey, 0);
		keyAddName(sectionKey, "..");
		const char *sectionName = keyName(sectionKey)+strlen(keyName(parentKey))+1;
		appendKey = createUnescapedKey(appendKey, sectionName);
		const char *oldLastSection = strdup(keyString(keyGetMeta(parentKey, "ini/lastSection")));
		setSectionNumber(parentKey, appendKey, newKS);
		if(atoi(keyString(keyGetMeta(appendKey, "ini/section"))) > atoi(oldLastSection))
		{
			setOrderNumber(parentKey, appendKey);
			keySetBinary(appendKey, 0, 0);
			ksAppendKey(newKS, keyDup(appendKey));
		}
		else
		{
			int lastOrderNumber = -1;
			int sectionNumber = atoi(keyString(keyGetMeta(appendKey, "ini/section")));
			KeySet *searchKS = ksDup(newKS);
			ksRewind(searchKS);
			Key *looking;
			while((looking = ksNext(searchKS)) != NULL)
			{
				if(atoi(keyString(keyGetMeta(looking, "ini/section"))) == sectionNumber)
					break;
			}
			KeySet *cutKS = ksCut(searchKS, looking);
			ksRewind(cutKS);
			while((looking = ksNext(cutKS)) != NULL)
			{
				if(atoi(keyString(keyGetMeta(looking, "order"))) > lastOrderNumber)
					lastOrderNumber = atoi(keyString(keyGetMeta(looking, "order")));
			}
			char buffer[21];
			snprintf(buffer, sizeof(buffer), "%09d/1", lastOrderNumber);
			keySetMeta(appendKey, "order", buffer);
			ksDel(cutKS);
			ksDel(searchKS);
		}
		keySetBinary(appendKey, 0, 0);
		if(!ksLookup(newKS, appendKey, KDB_O_NONE))
		{
			ksAppendKey(newKS, keyDup(appendKey));
		}
		free(oldLastSection);
		keySetMeta(appendKey, "order", 0);
		keySetMeta(appendKey, "ini/section", 0);
		keySetMeta(appendKey, "binary", 0);
		appendKey = createUnescapedKey(appendKey, keyBaseName(cur));
		setSectionNumber(parentKey, appendKey, newKS);
		keySetMeta(appendKey, "ini/key", "");
		ksAppendKey(newKS, appendKey);
		insertNewKeyIntoExistendOrder(appendKey, newKS);
		if(keyString(cur))
		{
			keySetString(appendKey, keyString(cur));
		}
		keyDel(sectionKey);
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
	unsigned short toMeta = pluginConfig->keyToMeta;
	ksRewind (returned);
	outputDebug(returned);
	ksRewind(returned);
	Key *cur;
	KeySet *newKS = ksNew(0, KS_END);
	keySetMeta(parentKey, "order", "0");
	while((cur = ksNext(returned)) != NULL)
	{
		if(keyGetMeta(cur, "order"))
		{
			if(atoi(keyString(keyGetMeta(parentKey, "order"))) < atoi(keyString(keyGetMeta(cur, "order"))))
				keySetMeta(parentKey, "order", keyString(keyGetMeta(cur, "order")));
			ksAppendKey(newKS, cur);
			keyDel(ksLookup(returned, cur, KDB_O_POP));
		}

	}
	ksAppendKey(newKS, parentKey);
	ksRewind(returned);
	outputDebug(returned);
	ksRewind(newKS);
	outputDebug(newKS);
	ksRewind(returned);
	while((cur = ksNext(returned)) != NULL)
	{
		if(!strcmp(keyName(cur), keyName(parentKey)))
			continue;
		if(!strcmp(keyBaseName(cur), INTERNAL_ROOT_SECTION))
			continue;
		insertIntoKS(parentKey, cur, newKS);
		keyDel(ksLookup(returned, cur, KDB_O_POP));
	}
	fprintf(stderr, "=========== NEW KS =========\n");
	ksRewind(newKS);
	ksClear(returned);
	ksAppend(returned, newKS);
	setParents(returned, parentKey);	
	outputDebug(newKS);
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

