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
#include <kdbproposal.h> //elektraKsToMemArray
#include <kdbprivate.h> //elektraReadArrayNumber
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbos.h>
#include <inih.h>
#include <ctype.h>
#include "ini.h"


char *keyNameGetOneLevel(const char *, size_t *);

int elektraIniOpen(Plugin *handle, Key *parentKey);
int elektraIniClose(Plugin *handle, Key *parentKey);

#include "contract.h"

#define INTERNAL_ROOT_SECTION "GLOBALROOT"

typedef enum{NONE, BINARY, ALWAYS}SectionHandling;

typedef struct {
	short supportMultiline;	/* defines whether multiline keys are supported */
	short preserverOrder;
	SectionHandling sectionHandling;
	short array;
	short mergeSections;
	short BOM;
	short toMeta;
	char *continuationString;
} IniPluginConfig;

typedef struct {
	Key *parentKey;	/* the parent key of the result KeySet */
	KeySet *result;			/* the result KeySet */
	char *collectedComment;	/* buffer for collecting comments until a non comment key is reached */
	short array;
	short mergeSections;
	short toMeta;
	IniPluginConfig *pluginConfig;
} CallbackHandle;


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


static void keyAddUnescapedBasePath (Key * key, const char * path)
{
	size_t size=0;
	char *p=keyNameGetOneLevel(path+size,&size);
	if (*p && path[0] == '/')
	{
		keyAddBaseName(key, path);
		return;
	}
	while (*p)
	{
		char *buffer = elektraMalloc(size+1);
		strncpy(buffer, p, size);
		buffer[size] = 0;
		int ret = keyAddName(key, buffer);
		if (ret == -1)
		{
			char *tmp = elektraMalloc(keyGetFullNameSize(key) + strlen(buffer));
			keyGetFullName(key, tmp, keyGetFullNameSize(key));
			strcat(tmp, "/");
			strcat(tmp, buffer);
			keySetName(key, tmp);
			elektraFree(tmp);
		}
		elektraFree(buffer);
		p=keyNameGetOneLevel(p+size,&size);
	}
}

static Key *createUnescapedKey(Key *key, const char *name)
{
	char *dupName = strdup(name);
	keyAddUnescapedBasePath(key, dupName);
	free(dupName);
	return key;
}
static void setSectionNumber(Key *parentKey, Key *key, KeySet *ks)
{
	if (!strcmp(keyBaseName(key), INTERNAL_ROOT_SECTION))
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

	while (1)
	{
		if (!strcmp(keyName(lookupKey), keyName(parentKey)))
		{
			if (keyGetMeta(parentKey, "ini/lastSection"))
			{
				long previousSection = atol(keyString(keyGetMeta(parentKey, "ini/lastSection")));
				++previousSection;
				char buffer[21]; //20 digits (long) + \0
				snprintf(buffer, sizeof (buffer), "%ld", previousSection);
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
		if (keyGetMeta(ksLookup(ks, lookupKey, KDB_O_NONE), "ini/section"))
		{
			keySetMeta(key, "ini/section", keyString(keyGetMeta(ksLookup(ks, lookupKey, KDB_O_NONE), "ini/section")));
			break;
		}
		keySetName(lastKey, keyName(lookupKey));
		if (keyAddName(lookupKey, "..") <= 0)
			break;
	}
	keyDel(lookupKey);
	keyDel(lastKey);
}

static void setOrderNumber(Key *parentKey, Key *key)
{
	kdb_long_long_t order = 0;
	const Key *orderKey = keyGetMeta(parentKey, "order");
	if (orderKey != NULL)
	{
		char *ptr = (char *)keyString(orderKey);
		++ptr; //skip #
		while (*ptr == '_')
		{
			++ptr;
		}
		elektraReadArrayNumber(ptr, &order);
	}
	++order;
	char buffer[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber(buffer, order);
	keySetMeta(key, "order", buffer);
	keySetMeta(parentKey, "order", buffer);
}
static void setSubOrderNumber(Key *key, const char *oldOrder)
{
	char *lastIndexPtr = NULL;
	char *newOrder = elektraMalloc(elektraStrLen(oldOrder)+ELEKTRA_MAX_ARRAY_SIZE);
	if ((lastIndexPtr = strrchr(oldOrder, '/')))
	{
		kdb_long_long_t subIndex = 0;
		char *ptr = lastIndexPtr;
		++ptr; //skip /
		++ptr; //skip #
		while (*ptr == '_')
		{
			++ptr;
		}
		elektraReadArrayNumber(ptr, &subIndex);
		++subIndex;
		int len = (lastIndexPtr+1) - oldOrder;
		char buffer[ELEKTRA_MAX_ARRAY_SIZE];
		elektraWriteArrayNumber(buffer, subIndex);
		sprintf(newOrder, "%.*s%s", len, oldOrder, buffer);
	}
	else
	{
		sprintf(newOrder, "%s/#1", oldOrder);
	}
	keySetMeta(key, "order", newOrder);
	elektraFree(newOrder);
}

static void insertNewKeyIntoExistendOrder(Key *key, KeySet *ks)
{
	if (keyGetMeta(ksLookup(ks, key, KDB_O_NONE), "order"))
		return;
	ksRewind(ks);	
	Key *curKey;
	if (keyGetMeta(key, "parent"))
	{
		Key *cutKey = ksLookupByName(ks, keyString(keyGetMeta(key, "parent")), KDB_O_NONE);
		KeySet *cutKS = ksCut(ks, cutKey);
		const char *oldOrder = NULL;
		if (keyGetMeta(cutKey, "order"))
		{
			oldOrder=keyString(keyGetMeta(cutKey, "order"));
		}
		else
		{
			oldOrder = "#1";
		}

		while ((curKey = ksNext(cutKS)) != NULL)
		{
			if (keyIsDirectBelow(cutKey, curKey) && !strcmp(keyString(keyGetMeta(curKey, "parent")), keyName(cutKey)) && !keyIsBinary(curKey))
			{
				if (keyGetMeta(curKey, "order"))
				{
					if (strcmp(keyString(keyGetMeta(curKey, "order")), oldOrder) > 0)
						oldOrder = keyString(keyGetMeta(curKey, "order"));
				}
			}
		}
		setSubOrderNumber(key, oldOrder);
		ksAppend(ks, cutKS);
		ksDel(cutKS);
	}
	else
	{

		Key *prevKey = NULL;
		while ((curKey = ksNext(ks)) != NULL)
		{
			if (!strcmp(keyName(curKey), keyName(key)))
			{
				const char *oldOrder = "#1";
				if (keyGetMeta(prevKey, "order"))
				{
					oldOrder = keyString(keyGetMeta(prevKey, "order"));
				}
				setSubOrderNumber(key, oldOrder);
			}	
			prevKey = curKey;
		}
	}
}
static void iniBomHandler(void *vhandle, short BOM)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;
	IniPluginConfig *pluginConfig = (IniPluginConfig *)handle->pluginConfig;
	if (BOM)
	{
		pluginConfig->BOM = 1;
	}
	else
	{
		pluginConfig->BOM = 0;
	}
}

static int iniKeyToElektraArray(CallbackHandle *handle, Key *existingKey, Key *appendKey, const char *value)
{

	keySetMeta(appendKey, "ini/section", 0);
	if (keyGetMeta(existingKey, "ini/array"))
	{
		//array already exists, appending new key
		const char *lastIndex = keyString(keyGetMeta(existingKey, "ini/array"));
		keyAddBaseName(appendKey, lastIndex);
		keySetMeta(appendKey, "ini/array", 0);
		keySetMeta(appendKey, "order", 0);
		keySetMeta(appendKey, "parent", 0);
		if (elektraArrayIncName(appendKey) == 1)
		{
			return -1;
		}
		keySetString(appendKey, value);
		keySetMeta(appendKey, "ini/key", 0);
		keySetMeta(appendKey, "ini/arrayMember", "");
		ksAppendKey(handle->result, appendKey);
		keySetMeta(existingKey, "ini/array", keyBaseName(appendKey));
		ksAppendKey(handle->result, existingKey);
	}
	else
	{
		//creating a new array
		Key *sectionKey = keyDup(appendKey);
		keyAddName(sectionKey, "..");
		char *origVal = strdup(keyString(existingKey));
		keySetString(appendKey, "");
		keySetMeta(appendKey, "ini/array", "#1");
		setSectionNumber(handle->parentKey, appendKey, handle->result);
		setOrderNumber(handle->parentKey, appendKey);
		keySetMeta(appendKey, "ini/key", "");
		keySetMeta(appendKey, "parent", 0);
		ksAppendKey(handle->result, keyDup(appendKey));
		keySetMeta(appendKey, "ini/arrayMember", "");
		keySetMeta(appendKey, "ini/key", 0);
		keySetMeta(appendKey, "ini/array", 0);
		keySetMeta(appendKey, "parent", 0);
		keySetMeta(appendKey, "ini/section", 0);
		keyAddName(appendKey, "#");
		keySetMeta(appendKey, "order", 0);
		if (elektraArrayIncName(appendKey) == -1)
		{
			free(origVal);
			return -1;
		}
		keySetString(appendKey, origVal);
		ksAppendKey(handle->result, keyDup(appendKey));
		free(origVal);
		if (elektraArrayIncName(appendKey) == -1)
		{
			return -1;
		}
		keySetMeta(appendKey, "parent", 0);
		keySetString(appendKey, value);
		ksAppendKey(handle->result, keyDup(appendKey));
		keyDel(appendKey);
		keyDel(sectionKey);
	}
	return 1;
}

static int iniKeyToElektraKey (void *vhandle, const char *section, const char *name, const char *value, unsigned short lineContinuation)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;
	if ((!section || *section == '\0') && (!name || *name == '\0'))
	{
		Key *rootKey = keyDup(handle->parentKey);	
		keySetString(rootKey, value);
		keySetMeta(rootKey, "ini/key", "");
		ksAppendKey(handle->result, rootKey);
		return 1;
	}
	Key *appendKey = keyDup (handle->parentKey);
	keySetMeta(appendKey, "ini/lastSection", 0);
	if (!section || *section == '\0')
	{
		section = INTERNAL_ROOT_SECTION;
	}
	appendKey = createUnescapedKey(appendKey, section);
	short mergeSections = 0;
	Key *existingKey = NULL;
	if ((existingKey = ksLookup(handle->result, appendKey, KDB_O_NONE)))
	{
		if (keyGetMeta(existingKey, "ini/duplicate"))
		{
			mergeSections = 1;
		}
	}
	if (handle->toMeta)
	{
		if (!existingKey)
			existingKey = appendKey;
		if (lineContinuation)
		{
			const Key *meta = keyGetMeta(existingKey, name);
			Key *newMeta = keyDup(meta);
			elektraKeyAppendLine(newMeta, value);
			keySetMeta(existingKey, name, keyString(newMeta));
			keyDel(newMeta);
		}
		else
		{
			keySetMeta(existingKey, name, value);
			ksAppendKey(handle->result, existingKey);
		}
		keyDel(appendKey);
		return 1;
	}
	setSectionNumber(handle->parentKey, appendKey, handle->result);
	appendKey = createUnescapedKey(appendKey, name);
	existingKey = ksLookup(handle->result, appendKey, KDB_O_NONE);
	if (existingKey)
	{
		//a key with the same name already exists
		if (handle->array)
		{
			//array support is turned on
			return iniKeyToElektraArray(handle, existingKey, appendKey, value); 
		}
		else if (!lineContinuation)
		{
			keyDel(appendKey);
			ELEKTRA_SET_ERRORF(141, handle->parentKey, "Key: %s\n", keyName(existingKey));
			return -1;
		}
	}

	setSectionNumber(handle->parentKey, appendKey, handle->result);
	if (value == NULL)
		keySetMeta(appendKey, "ini/empty", "");
	if (!lineContinuation)
	{
		flushCollectedComment (handle, appendKey);
		keySetString (appendKey, value);
		keySetMeta(appendKey, "ini/key", "");
		ksAppendKey (handle->result, appendKey);
		if (mergeSections)
		{
			keySetMeta(appendKey, "order", 0);
			insertNewKeyIntoExistendOrder(appendKey, handle->result);
		}
		else
		{
			setOrderNumber(handle->parentKey, appendKey);
		}
	}
	else
	{
		existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
		keyDel (appendKey);
		/* something went wrong before because this key should exist */
		if (!existingKey) return -1;

		elektraKeyAppendLine(existingKey, value);
	}


	return 1;
}

static short isIniKey(Key *key)
{
	if (!key) return 0;
	if (keyGetMeta(key, "ini/key"))
		return 1;
	else
		return 0;
}

static short isSectionKey(Key *key)
{
	if (!key) return 0;
	if (keyIsBinary(key))
		return 1;
	else 
		return 0;
}

static int iniSectionToElektraKey (void *vhandle, const char *section)
{
	CallbackHandle *handle = (CallbackHandle *)vhandle;
	Key *appendKey = keyDup (handle->parentKey);
	keySetString(appendKey, 0);
	keySetMeta(appendKey, "ini/lastSection", 0);
	createUnescapedKey(appendKey, section);
	Key *existingKey = NULL;
	if ((existingKey = ksLookup(handle->result, appendKey, KDB_O_NONE)))
	{
		keyDel(appendKey);
		if (!handle->mergeSections)
		{
			ELEKTRA_SET_ERRORF(140, handle->parentKey, "Section name: %s\n", section);
			return 0;
		}
		keySetMeta(existingKey, "ini/duplicate", "");
		return 1;
	}
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
	pluginConfig->BOM = 0;
	Key *multilineKey = ksLookupByName (config, "/multiline", KDB_O_NONE);
	Key *sectionHandlingKey = ksLookupByName(config, "/section", KDB_O_NONE);
	Key *arrayKey = ksLookupByName(config, "/array", KDB_O_NONE);
	Key *mergeSectionsKey = ksLookupByName(config, "/mergesections", KDB_O_NONE);
	Key *toMetaKey = ksLookupByName(config, "/meta", KDB_O_NONE);
	Key *contStringKey = ksLookupByName(config, "/linecont", KDB_O_NONE);
	if (!contStringKey)
	{
		pluginConfig->continuationString = strdup("\\");
	}
	else
	{
		pluginConfig->continuationString = strdup(keyString(contStringKey));
	}
	pluginConfig->toMeta = toMetaKey != 0;
	pluginConfig->mergeSections = mergeSectionsKey != 0;
	pluginConfig->array = arrayKey != 0;
	if (!multilineKey)
	{
		pluginConfig->supportMultiline = 1;
	}
	else
	{
		if (!strcmp(keyString(multilineKey), "0"))
		{
			pluginConfig->supportMultiline = 0;
		}
		else
		{
			pluginConfig->supportMultiline = 1;
		}
	}
	if (!sectionHandlingKey)
	{
		pluginConfig->sectionHandling = ALWAYS;
	}
	else
	{
		if (!strcasecmp(keyString(sectionHandlingKey), "NONE"))
		{
			pluginConfig->sectionHandling = NONE;
		}
		else if (!strcasecmp(keyString(sectionHandlingKey), "NULL"))
		{
			pluginConfig->sectionHandling = BINARY;
		}
		else if (!strcasecmp(keyString(sectionHandlingKey), "ALWAYS"))
		{
			pluginConfig->sectionHandling = ALWAYS;
		}
	}
	elektraPluginSetData(handle, pluginConfig);

	return 0;
}


int elektraIniClose(Plugin *handle, Key *parentKey ELEKTRA_UNUSED)
{
	IniPluginConfig *pluginConfig = (IniPluginConfig *)elektraPluginGetData(handle);
	elektraFree(pluginConfig->continuationString);
	elektraFree(pluginConfig);
	elektraPluginSetData(handle, 0);
	return 0;
}

#if DEBUG && VERBOSE
static void outputDebug() __attribute__ ((unused));

static void outputDebug(KeySet *ks)
{
	Key *cur;
	ksRewind(ks);
	while ((cur = ksNext(ks)) != NULL)
	{
		fprintf(stderr, "%s:(%.20s:_%.5x_)\t", keyName(cur), keyString(cur), *keyString(cur));
		fprintf(stderr, " sync: %d", keyNeedSync(cur));
		keyRewindMeta(cur);
		const Key *meta;
		while ((meta = keyNextMeta(cur)) != NULL)
		{
			fprintf(stderr, ", %s: %s", keyName(meta), (char *)keyValue(meta));
		}
		fprintf(stderr, "\n");
	}
}
#endif

static const char *findParent(Key *parentKey, Key *searchkey, KeySet *ks)
{
	if (keyGetMeta(searchkey, "parent"))
	{
		ksDel(ks);
		return NULL;
	}	
	size_t offset = 0;
	if (keyName(parentKey)[0] == '/' && keyName(searchkey)[0] != '/')
	{
		offset = strchr(keyName(searchkey)+1, '/')-keyName(searchkey);
	}
	Key *key = keyDup(searchkey);
	Key *lookedUp;
	while (strcmp(keyName(key)+offset, keyName(parentKey)))
	{
		if (!strcmp(keyName(key), keyName(searchkey)))
		{
			keyAddName(key, "..");
			continue;
		}
		lookedUp = ksLookup(ks, key, KDB_O_NONE);
		if (lookedUp)
		{
			if (isSectionKey(lookedUp))
				break;
		}
		
		if (keyAddName(key, "..") <= 0)
			break;
	}
	lookedUp = ksLookup(ks, key, KDB_O_NONE);
	if (!lookedUp)
		lookedUp = parentKey;
	keyDel(key);
	ksDel(ks);
	return keyName(lookedUp);
}
static void setParents(KeySet *ks, Key *parentKey)
{
	Key *cur;
	ksRewind(ks);
	while ((cur = ksNext(ks)) != NULL)
	{
		const char *parentName = findParent(parentKey, cur, ksDup(ks));
		if (parentName)
			keySetMeta(cur, "parent", parentName);
	}
}
static void stripInternalData(Key *parentKey, KeySet *);

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

	//ksAppendKey (cbHandle.result, keyDup(parentKey));

	struct IniConfig iniConfig;
	iniConfig.keyHandler=iniKeyToElektraKey;
	iniConfig.sectionHandler = iniSectionToElektraKey;
	iniConfig.commentHandler = iniCommentToMeta;
	iniConfig.bomHandler = iniBomHandler;
	IniPluginConfig *pluginConfig = elektraPluginGetData(handle);
	iniConfig.continuationString = pluginConfig->continuationString; 
	iniConfig.supportMultiline = pluginConfig->supportMultiline;
	pluginConfig->BOM = 0;
	cbHandle.array = pluginConfig->array;
	cbHandle.mergeSections = pluginConfig->mergeSections;
	cbHandle.pluginConfig = pluginConfig;
	cbHandle.toMeta = pluginConfig->toMeta;
	int ret = ini_parse_file(fh, &iniConfig, &cbHandle);
	setParents(cbHandle.result, cbHandle.parentKey);
	stripInternalData(cbHandle.parentKey, cbHandle.result);
	fclose (fh);
	errno = errnosave;
	ksRewind(cbHandle.result);
	if (ret == 0)
	{
		ksClear(returned);
		ksAppend(returned, cbHandle.result);
		//ksAppendKey(returned, keyDup(parentKey));
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
		char* comments = elektraMalloc (commentSize);
		keyGetString (commentMeta, comments, commentSize);
		char *ptr = comments;
		while (*ptr)
		{
			if ((*ptr) == '\n')
			{
				fprintf(fh, "\n");
				++ptr;
			}
			else if (*ptr != ' ')
			{
				fprintf(fh, ";");
				while (*ptr && ((*ptr) != '\n'))
				{
					fprintf(fh, "%c", *ptr);
					++ptr;
				}
			}
			else
			{
				++ptr;
			}
		}
		if (!(*ptr))
		{
			fprintf(fh, "\n");
		}
		elektraFree (comments);
	}
}
static int containsSpecialCharacter(const char *);

void writeMultilineKey(Key *key, const char *iniName, FILE *fh, IniPluginConfig *config)
{
	size_t valueSize = keyGetValueSize(key);
	char *saveptr = 0;
	char *result = 0;
	char *value = elektraMalloc (valueSize);
	keyGetString(key, value, valueSize);
	result = strtok_r (value, "\n", &saveptr);
	if (containsSpecialCharacter(iniName))
	{
		fprintf(fh, "\"%s\" = ", iniName);
	}
	else
	{
		fprintf(fh, "%s = ", iniName);
	}
	if (result == NULL)
		fprintf(fh, "\"\n%s\"", config->continuationString);
	else
	{
		if (containsSpecialCharacter(result))
			fprintf (fh, "\"%s\"\n", result);
		else
			fprintf (fh, "%s\n",result);
	}
	while ( (result = strtok_r (0, "\n", &saveptr)) != 0)
	{
		if (containsSpecialCharacter(result))
			fprintf (fh, "%s\"%s\"\n", config->continuationString, result);
		else
			fprintf (fh, "%s%s\n", config->continuationString, result);
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
	if (keyName(section)[0] == '/')
	{
		if (!strcmp(keyName(section), strchr(keyName(key)+1, '/')))
			return strdup(keyBaseName(key));
	}
	int slashCount = 0;
	char *slashCounter = (char *)keyName(key);
	while (*slashCounter)
	{
		if (*slashCounter == '/')
			++slashCount;
		++slashCounter;
	}
	int len = 0;
	if (strcmp(keyName(section), "/"))
		len = strlen(keyName(section));
	char *buffer = elektraCalloc((strlen(keyName(key)) - len)+slashCount+1);
	char *ptr = NULL;
	if(!strcmp(keyName(section), "/"))
	{
		ptr = (char *)keyName(key);
	}
	else if (keyName(section)[0] == '/' && keyName(key)[0] != '/')
	{
		size_t offset = strchr(keyName(key)+1, '/')-keyName(key);	
		ptr = (char *)keyName(key)+strlen(keyName(section))+offset+1;
	}
	else
	{
		ptr	= (char *)keyName(key)+strlen(keyName(section))+1;
	}

	size_t size = 0;
	char *tmp = strdup(ptr);
	char *p = keyNameGetOneLevel(tmp+size, &size);
	while (*p)
	{
		char *name = elektraMalloc(size+1);
		strncpy(name, p, size);
		name[size] = 0;
		strcat(buffer, name);
		strcat(buffer, "/");
		elektraFree(name);
		p = keyNameGetOneLevel(p+size, &size);

	}
	free(tmp);
	buffer[strlen(buffer)-1] = '\0';
	return buffer;
}

static void insertSectionIntoExistingOrder(Key *parentKey, Key *appendKey, KeySet *newKS)
{
	char *lastOrderNumber = NULL;
	int sectionNumber = atoi(keyString(keyGetMeta(appendKey, "ini/section")));
	KeySet *searchKS = ksDup(newKS);
	ksRewind(searchKS);
	Key *looking;
	while ((looking = ksNext(searchKS)) != NULL)
	{
		int curSectionNumber = atoi(keyString(keyGetMeta(looking, "ini/section")));
		if (curSectionNumber == sectionNumber)
			break;
	}
	Key *meta = (Key *)keyGetMeta(looking, "order");
	if (meta)
		lastOrderNumber = (char *)keyString(meta);
	KeySet *cutKS = ksCut(searchKS, looking);
	ksRewind(cutKS);
	while ((looking = ksNext(cutKS)) != NULL)
	{
		meta = (Key *)keyGetMeta(looking, "order");
		if (!meta)
			continue;
		if (!strcmp(keyName(looking), keyName(appendKey)))
			continue;
		if (!lastOrderNumber || strcmp(keyString(meta), lastOrderNumber) > 0)
		{
			lastOrderNumber = (char *)keyString(meta);
		}
	}

	if(lastOrderNumber)
		setSubOrderNumber(appendKey, lastOrderNumber);
	else
		setOrderNumber(parentKey, appendKey);

	ksDel(cutKS);
	ksDel(searchKS);
}

static void insertNewSectionIntoExistendOrder(Key *parentKey, Key *appendKey, KeySet *newKS)
{
	KeySet *searchKS = ksDup(newKS);
	ksRewind(searchKS);
	Key *looking;
	while ((looking = ksNext(searchKS)) != NULL)
	{	
		if (!strcmp(keyName(looking), keyName(appendKey)))
			break;
	}
	int found = 0;
	while ((looking = ksPrev(searchKS)) != NULL)
	{
		if (keyIsBinary(looking))
		{
			found = 1;
			break;
		}
	}
	if (!found)
	{
		setOrderNumber(parentKey, appendKey);
		ksDel(searchKS);
		return;
	}
	char *lastOrderNumber = "#1";
	Key *meta = (Key *)keyGetMeta(looking, "order");
	if (meta)
		lastOrderNumber = (char *)keyString(meta);
	long sectionNumber = atol(keyString(keyGetMeta(looking, "ini/section")));
	ksRewind(searchKS);
	while (( looking = ksNext(searchKS)) != NULL)
	{
		if (atol(keyString(keyGetMeta(looking, "ini/section"))) == sectionNumber)
		{
			if (strcmp(keyString(keyGetMeta(looking, "order")), lastOrderNumber) > 0)
				lastOrderNumber = (char *)keyString(keyGetMeta(looking, "order"));
		}
	}
	if (!lastOrderNumber)
		setOrderNumber(parentKey, appendKey);
	else
		setSubOrderNumber(appendKey, lastOrderNumber);

	ksDel(searchKS);
}

void insertIntoKS(Key *parentKey, Key *cur, KeySet *newKS, IniPluginConfig *pluginConfig)
{
	Key *appendKey = keyDup(parentKey);
	keyCopyAllMeta(appendKey, cur);
	keySetMeta(appendKey, "ini/lastSection", 0);
	keySetString(appendKey, 0);
	keySetMeta(appendKey, "order", 0);
	keySetMeta(appendKey, "binary", 0);
	keySetMeta(appendKey, "ini/section", 0);
	keySetMeta(appendKey, "ini/key", 0);
	char *oldSectionNumber = strdup(keyString(keyGetMeta(parentKey, "ini/lastSection")));
	if (keyIsBinary(cur))
	{
		// create new section here
		char *sectionName = NULL;
		if (!strcmp(keyName(parentKey), "/"))
		{
			keySetName(appendKey, keyName(cur));
		}
		else if (keyName(parentKey)[0] == '/' && keyName(cur)[0] != '/')
		{
			sectionName = (char *)keyName(cur)+strlen(strchr(keyName(parentKey)+1, '/'))+1;
			createUnescapedKey(appendKey, sectionName);
		}
		else
		{
			sectionName = (char *)keyName(cur)+strlen(keyName(parentKey))+1;
			createUnescapedKey(appendKey, sectionName);
		}
		setSectionNumber(parentKey, appendKey, newKS);
		keySetBinary(appendKey, 0, 0);
		ksAppendKey(newKS, appendKey);
		if (atoi(oldSectionNumber) < atoi(keyString(keyGetMeta(appendKey, "ini/section"))))
		{
			insertNewSectionIntoExistendOrder(parentKey, appendKey, newKS);
		}
		else
		{
			insertSectionIntoExistingOrder(parentKey, appendKey, newKS);
		}
	}
	else if (keyIsDirectBelow(parentKey, cur))
	{
		// create global key here
		char *name = NULL;
		if (keyName(parentKey)[0] == '/' && keyName(cur)[0] != '/')
		{
			name = (char *)keyName(cur)+strlen(strchr(keyName(parentKey)+1, '/'))+1;
		}
		else
		{
			name = (char *)keyName(cur)+strlen(keyName(parentKey))+1;
		}
		const char *sectionName = INTERNAL_ROOT_SECTION;
		createUnescapedKey(appendKey, sectionName);
		if (!ksLookup(newKS, cur, KDB_O_NONE))
		{
			keySetMeta(appendKey, "order", "#1");
		}
		setSectionNumber(parentKey, appendKey, newKS);
		createUnescapedKey(appendKey, name);
		keySetMeta(appendKey, "ini/key", "");
		ksAppendKey(newKS, appendKey);
		insertNewKeyIntoExistendOrder(appendKey, newKS);
		keySetString(appendKey, keyString(cur));
		if (keyGetMeta(cur, "ini/empty"))
		{
			keySetMeta(appendKey, "ini/empty", "");
		}
	}
	else
	{
		Key *sectionKey = keyDup(cur);
		keyAddName(sectionKey, "..");
		char *sectionName = NULL;
		if (!strcmp(keyName(parentKey), "/"))
		{
			keySetName(appendKey, keyName(sectionKey));
		}
		else if (keyName(parentKey)[0] == '/' && keyName(cur)[0] != '/')
		{
			sectionName = (char *)keyName(sectionKey)+strlen(strchr(keyName(parentKey)+1, '/'))+1;
			appendKey = createUnescapedKey(appendKey, sectionName);
		}
		else
		{
			sectionName = (char *)keyName(sectionKey)+strlen(keyName(parentKey))+1;
			appendKey = createUnescapedKey(appendKey, sectionName);
		}
		if (pluginConfig->sectionHandling == ALWAYS)
		{
			setSectionNumber(parentKey, appendKey, newKS);
			if (atoi(keyString(keyGetMeta(appendKey, "ini/section"))) > atoi(oldSectionNumber))
			{
				keySetBinary(appendKey, 0, 0);
				ksAppendKey(newKS, appendKey);
				insertNewSectionIntoExistendOrder(parentKey, appendKey, newKS);
				appendKey = keyDup(appendKey);
			}
			else
			{
				if (!ksLookup(newKS, appendKey, KDB_O_NONE))
				{
					keySetBinary(appendKey, 0, 0);
					ksAppendKey(newKS, appendKey);
					insertSectionIntoExistingOrder(parentKey, appendKey, newKS);
					appendKey = keyDup(appendKey);
				}
			}
			keySetMeta(appendKey, "order", 0);
			keySetMeta(appendKey, "ini/section", 0);
			keySetMeta(appendKey, "binary", 0);
		}
		appendKey = createUnescapedKey(appendKey, keyBaseName(cur));
		if ((elektraArrayValidateName(appendKey) == 1) && pluginConfig->array)
		{
			Key *arrayParentLookup = keyDup(appendKey);
			keySetBaseName(arrayParentLookup, 0);
			Key *arrayParent = ksLookup(newKS, arrayParentLookup, KDB_O_NONE);
			keyDel(arrayParentLookup);
			const Key *arrayMeta = keyGetMeta(arrayParent, "ini/array");
			if (arrayMeta)
			{
				if (strcmp(keyString(arrayMeta), keyBaseName(cur)) < 0)
				{
					keySetMeta(arrayParent, "ini/array", keyBaseName(cur));
				}
			}
			else
			{
				const char *oldVal = keyString(arrayParent);
				keySetMeta(arrayParent, "ini/array", keyBaseName(cur));
				keySetMeta(arrayParent, "ini/key", "");
				keySetMeta(arrayParent, "binary", 0);

				if (oldVal && strlen(oldVal))
				{
					Key *arrayInitKey = keyDup(arrayParent);
					keyAddBaseName(arrayInitKey, "#0");
					keySetString(arrayInitKey, oldVal);
					ksAppendKey(newKS, arrayInitKey);
				}
				ksAppendKey(newKS, appendKey);
			}
		}
		setSectionNumber(parentKey, appendKey, newKS);
		keySetMeta(appendKey, "ini/key", "");
		if (atoi(keyString(keyGetMeta(appendKey, "ini/section"))) > atoi(oldSectionNumber))
		{
			ksAppendKey(newKS, appendKey);
			if (pluginConfig->sectionHandling == ALWAYS)
			{
				insertNewKeyIntoExistendOrder(appendKey, newKS);
			}
			else
			{
				keySetMeta(appendKey, "order", "#0");
			}
		}
		else
		{
			const char *parent = findParent(parentKey, appendKey, ksDup(newKS));
			if (parent)
			{
				keySetMeta(appendKey, "parent", parent);
			}
			ksAppendKey(newKS, appendKey);
			insertNewKeyIntoExistendOrder(appendKey, newKS);
		}
		if (keyString(cur))
		{
			keySetString(appendKey, keyString(cur));
		}
		if (keyGetMeta(cur, "ini/empty"))
		{
			keySetMeta(appendKey, "ini/empty", "");
		}
		keyDel(sectionKey);
	}
	if (keyGetMeta(cur, "comment"))
	{
		keySetMeta(appendKey, "comment", keyString(keyGetMeta(cur, "comment")));
	}
	free(oldSectionNumber);
}

static int iniCmpOrder(const void *a, const void *b)
{
	const Key *ka = (*(const Key **)a);
	const Key *kb = (*(const Key **)b);

	if (!ka && !kb) return 0;
	if (ka && !kb) return 1;
	if (!ka && kb) return -1;

	const Key *kam = keyGetMeta(ka, "order");
	const Key *kbm = keyGetMeta(kb, "order");

	return strcmp(keyString(kam), keyString(kbm));
}

static int containsSpecialCharacter(const char *str)
{
	char *ptr = (char *)str;
	if (isspace(*ptr) || (isspace(*(ptr+strlen(str)-1))))
		return 1;
	if (*ptr == '#' || *ptr == ';')
		return 1;
	if (*ptr == '[')
		return 1;
	while (*ptr)
	{
		if (*ptr == '"' || *ptr == '=' || *ptr == ':')
		{
			return 1;
		}
		++ptr;
	}
	return 0;
}

static void iniWriteMeta(FILE *fh, Key *parentKey, Key *key, IniPluginConfig *config)
{
	uint8_t first = 1;
	keyRewindMeta(key);
	while (keyNextMeta(key) != NULL)
	{
		Key *meta = (Key *)keyCurrentMeta(key);
		const char *name = keyName(meta);
		if (strncmp(name, "ini/", 4) && strcmp(name, "order") && strcmp(name, "parent") && strcmp(name, "binary"))
		{
			if (first)
			{
				char *iniName = getIniName(parentKey, key);
				fprintf(fh, "[%s]\n", iniName);
				elektraFree(iniName);
				first = 0;
			}
			const char *string = keyString(meta);
			if (strstr(string, "\n") == 0)
			{
				if (containsSpecialCharacter(name))
					fprintf(fh, "\"%s\" = ", name);
				else
					fprintf(fh, "%s = ", name);
				if (strlen(string) && (containsSpecialCharacter(string)))
					fprintf(fh, "\"%s\"\n", string);
				else if (strlen(string))
					fprintf(fh, "%s\n", string);
			}
			else
			{
				writeMultilineKey(meta, name, fh, config);
			}
		}
	}
}


static int iniWriteKeySet(FILE *fh, Key *parentKey, KeySet *returned, IniPluginConfig *config)
{
	ksRewind(returned);
	Key **keyArray;
	ssize_t arraySize = ksGetSize(returned);
	keyArray = elektraCalloc(arraySize * sizeof (Key*));
	elektraKsToMemArray(returned, keyArray);
	qsort(keyArray, arraySize, sizeof (Key *), iniCmpOrder);
	Key *cur = NULL;
	Key *sectionKey = parentKey;
	int ret = 1;
	
	if (config->toMeta)
	{
		for (ssize_t i = 0; i < arraySize; ++i)
		{
			cur = keyArray[i];
			if (!strcmp(keyName(parentKey), keyName(cur)))
				continue;
			if (keyGetValueSize(cur) <= 1)
				continue;
			char *name = getIniName(parentKey, cur);
			const char *string = keyString(cur);
			if (strstr(string, "\n") == 0)
			{
				if (containsSpecialCharacter(name))
					fprintf(fh, "\"%s\" = ", name);
				else
					fprintf(fh, "%s = ", name);
				if (strlen(string) && (containsSpecialCharacter(string)))
					fprintf(fh, "\"%s\"\n", string);
				else if (strlen(string))
					fprintf(fh, "%s\n", string);
			}
			else
			{
				writeMultilineKey(cur, name, fh, config);
			}
			elektraFree(name);
		}
	}
	
	int removeSectionKey = 0;
	
	for (ssize_t i = 0; i < arraySize; ++i)
	{
		cur = keyArray[i];
		
		if (!strcmp(keyName(parentKey), keyName(cur)))
		{
			continue;
		}
		if (keyName(parentKey)[0] == '/')
		{
			if (!strcmp(keyName(parentKey), strchr(keyName(cur)+1, '/')))
				continue;
		}


		if (isSectionKey(cur))
		{
			if (removeSectionKey)
			{
				keyDel(sectionKey);
				sectionKey = parentKey;
				removeSectionKey = 0;
			}
			sectionKey = cur;
		}
		writeComments(cur, fh);
		if (config->toMeta)
		{
			iniWriteMeta(fh, parentKey, cur, config);
			continue;
		}
		else if (config->sectionHandling == NONE)
		{
			char *name = getIniName(parentKey, cur);
			if (isIniKey(cur))
			{
				const char *string = keyString(cur);
				if (containsSpecialCharacter(name))
					fprintf(fh, "\"%s\" = ", name);
				else
					fprintf(fh, "%s = ", name);
				if (strlen(string) && (containsSpecialCharacter(string)))
					fprintf(fh, "\"%s\"\n", string);
				else
					fprintf(fh, "%s\n", string);
			}
			free(name);
		}
		else
		{
			if (isSectionKey(cur))
			{
				char *iniName = getIniName(parentKey, cur);
				fprintf(fh, "[%s]\n", iniName);
				free(iniName);
			}
			else if (isIniKey(cur))
			{
				if (config->sectionHandling != NONE)
				{
					if (keyGetMeta(cur, "parent") && strcmp(keyString(keyGetMeta(cur, "parent")), keyName(sectionKey)) && strcmp(keyName(cur), keyString(keyGetMeta(cur, "parent"))))
					{
						Key *oldSectionKey = sectionKey;
						sectionKey = keyNew(keyString(keyGetMeta(cur, "parent")), KEY_END);
						if (!keyIsBelow(oldSectionKey, sectionKey))
						{
							removeSectionKey = 1;
							if (keyIsBelow(parentKey, sectionKey))
							{
								char *name = getIniName(parentKey, sectionKey);
								fprintf(fh, "[%s]\n", name);
								elektraFree(name);
							}
							else if (!strcmp(keyName(parentKey), "/") && !strcmp(keyString(keyGetMeta(cur, "parent")), "/"))
							{
								fprintf(fh, "[]\n");
							}
							else if (!strcmp(keyName(parentKey), "/"))
							{
								fprintf(fh, "[%s]\n", keyString(keyGetMeta(cur, "parent")));
							}
						}
						else
						{
							keyDel(sectionKey);
							sectionKey = oldSectionKey;
						}
					}
				}
				if (keyGetMeta(cur, "ini/array") && config->array)
				{
					int lastArrayIndex = atoi(keyString(keyGetMeta(cur, "ini/array"))+1);
					char *name = getIniName(sectionKey, cur);
					++i;
					for (int j = i; j <= i+lastArrayIndex; ++j)
					{
						cur = keyArray[j];
						const char *string = keyString(cur);
						if (containsSpecialCharacter(name))
							fprintf(fh, "\"%s\" = ", name);
						else
							fprintf(fh, "%s = ", name);
						if (strlen(string) && (containsSpecialCharacter(string)))
							fprintf(fh, "\"%s\"\n", string);
						else
							fprintf(fh, "%s\n", string);
					}
					free(name);
					i += lastArrayIndex;
				}
				else
				{
					char *name;
					if (keyIsBelow(sectionKey, cur))
					{
						name = getIniName(sectionKey, cur);
					}
					else 
					{
						name = getIniName(parentKey, cur);
					}

					if (keyGetMeta(cur, "ini/empty"))
					{
						if (containsSpecialCharacter(name))
							fprintf(fh, "\"%s\"\n", name);
						else
							fprintf(fh, "%s\n",name);
					}
					else if (strstr(keyString(cur), "\n") == 0)
					{
						const char *string = keyString(cur);
						if (containsSpecialCharacter(name))
							fprintf(fh, "\"%s\" = ", name);
						else
							fprintf(fh, "%s = ", name);
						if (strlen(string) && (containsSpecialCharacter(string)))
							fprintf(fh, "\"%s\"\n", string);
						else if (strlen(string))
							fprintf(fh, "%s\n", string);
						else
							fprintf(fh, "\n");

					}
					else
					{
						if (config->supportMultiline)
						{
							writeMultilineKey(cur, name, fh, config);
						}
						else
						{
							ELEKTRA_SET_ERROR(97, parentKey,
									"Encountered a multiline value but multiline support is not enabled. "
									"Have a look at kdb info ini for more details");
							ret = -1;
						}
					}
					free(name);
				}
			}
		}
	}
	if (removeSectionKey)
		keyDel(sectionKey);

	elektraFree(keyArray);
	return ret;
}
static void stripInternalData(Key *parentKey ELEKTRA_UNUSED, KeySet *ks)
{
	ksRewind(ks);
	Key *cur;
	KeySet *newKS = ksNew(ksGetSize(ks), KS_END);
	while ((cur = ksNext(ks)) != NULL)
	{
		if (strstr(keyName(cur), INTERNAL_ROOT_SECTION))
		{
			Key *newKey = keyDup(cur);
			char *oldName = strdup(keyName(cur));
			char *newName = elektraCalloc(elektraStrLen(keyName(cur)));
			char *token = NULL;
			token = strtok(oldName, "/");
			strcat(newName, token);
			while (token != NULL)
			{
				token = strtok(NULL, "/");
				if (token == NULL)
					break;
				if (!strcmp(token, INTERNAL_ROOT_SECTION))
					continue;
				strcat(newName, "/");
				strcat(newName, token);
			}
			strcat(newName, "/");
			keySetName(newKey, newName);
			if (strcmp(keyName(parentKey), keyName(newKey)))
				ksAppendKey(newKS, keyDup(newKey));
			keyDel(newKey);
			elektraFree(oldName);
			elektraFree(newName);
		}
		else
		{
			if (keyGetMeta(cur, "ini/key") || keyGetMeta(cur, "ini/arrayMember") || keyIsBinary(cur))
			{
				if (!keyGetMeta(ksLookup(newKS, cur, KDB_O_NONE), "ini/key"))
				{
					ksAppendKey(newKS, cur);
				}			
			}
		}
	}
	ksClear(ks);
	ksAppend(ks, newKS);
	ksDel(newKS);
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
	long lastSection = 0;
	if (keyGetMeta(parentKey, "ini/lastSection"))
	{
		lastSection = atol(keyString(keyGetMeta(parentKey, "ini/lastSection")));
	}
	Key *root = keyDup(ksLookup(returned, parentKey, KDB_O_NONE));
	Key *head = keyDup(ksHead(returned));
	IniPluginConfig* pluginConfig = elektraPluginGetData(handle);
	ksRewind(returned);
	Key *cur;
	KeySet *newKS = ksNew(0, KS_END);
	keySetMeta(parentKey, "order", "#0");
	while ((cur = ksNext(returned)) != NULL)
	{
		if (keyGetMeta(cur, "order"))
		{
			if (strcmp(keyString(keyGetMeta(parentKey, "order")),keyString(keyGetMeta(cur, "order"))) < 0)												 
				keySetMeta(parentKey, "order", keyString(keyGetMeta(cur, "order")));
			if (atol(keyString(keyGetMeta(cur, "ini/section"))) > lastSection)
			{
				lastSection = atol(keyString(keyGetMeta(cur, "ini/section")));
			}
			if (keyGetValueSize(cur) > 1)
			{
				keySetMeta(cur, "ini/key", "");
			}
			else if (keyGetMeta(cur, "ini/empty"))
			{
				keySetMeta(cur, "ini/key", "");
			}
			else if (!keyIsBinary(cur))
			{
				keySetMeta(cur, "ini/key", "");
			}

			ksAppendKey(newKS, cur);
			keyDel(ksLookup(returned, cur, KDB_O_POP));
		}

	}
	char buffer[21];
	snprintf(buffer, sizeof (buffer), "%lu", lastSection);
	keySetMeta(parentKey, "ini/lastSection", buffer);

	ksRewind(returned);

	while ((cur = ksNext(returned)) != NULL)
	{
		if (!strcmp(keyName(cur), keyName(parentKey)))
			continue;
		if (!strcmp(keyBaseName(cur), INTERNAL_ROOT_SECTION))
			continue;
		insertIntoKS(parentKey, cur, newKS, pluginConfig);
		keyDel(ksLookup(returned, cur, KDB_O_POP));
	}
	ksClear(returned);
	ksAppend(returned, newKS);
	ksDel(newKS);
	setParents(returned, parentKey);
	stripInternalData(parentKey, returned);
	
	if (pluginConfig->BOM == 1)
	{
		fprintf(fh, "\xEF\xBB\xBF");
	}
	if (keyNeedSync(parentKey) && root)
	{
		if (strncmp(keyString(parentKey), keyString(root), strlen(keyString(root))))
			if (keyString(root) && strlen(keyString(root)))
				fprintf(fh, "= %s\n", keyString(root));
	}
	keyDel(root);
	keyDel(head);
	ret = iniWriteKeySet(fh, parentKey, returned, pluginConfig);
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

