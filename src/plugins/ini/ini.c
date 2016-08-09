/**
 * @file
 *
 * @brief A plugin for reading and writing ini files
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "ini.h"
#include <ctype.h>
#include <errno.h>
#include <inih.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdbmeta.h>
#include <kdbos.h>
#include <kdbprivate.h>  //elektraReadArrayNumber
#include <kdbproposal.h> //elektraKsToMemArray
#include <stdlib.h>
#include <string.h>


char * keyNameGetOneLevel (const char *, size_t *);

int elektraIniOpen (Plugin * handle, Key * parentKey);
int elektraIniClose (Plugin * handle, Key * parentKey);
static char * findParent (Key *, Key *, KeySet *);
#include "contract.h"
static int iniCmpOrder (const void * a, const void * b);

#define INTERNAL_ROOT_SECTION "GLOBALROOT"
#define DEFAULT_DELIMITER '='

typedef enum { NONE, BINARY, ALWAYS } SectionHandling;

typedef struct
{
	short supportMultiline; /* defines whether multiline keys are supported */
	short preserverOrder;
	SectionHandling sectionHandling;
	short array;
	short mergeSections;
	short BOM;
	short toMeta;
	char * continuationString;
	char delim;
	char * lastOrder;
	KeySet * oldKS;
} IniPluginConfig;

typedef struct
{
	Key * parentKey;	/* the parent key of the result KeySet */
	KeySet * result;	/* the result KeySet */
	Key * collectedComment; /* buffer for collecting comments until a non comment key is reached */
	short array;
	short mergeSections;
	short toMeta;
	IniPluginConfig * pluginConfig;
} CallbackHandle;


static void flushCollectedComment (CallbackHandle * handle, Key * key)
{
	if (handle->collectedComment)
	{
		KeySet * comments = elektraMetaArrayToKS (handle->collectedComment, "comments");
		Key * cur;
		while ((cur = ksNext (comments)) != NULL)
		{
			keySetMeta (key, keyName (cur), keyString (cur));
		}
		ksDel (comments);
		keyDel (handle->collectedComment);
		handle->collectedComment = NULL;
	}
}

// TODO defined privately in internal.c, API break possible.
// Might consider moving this to the public API as it might be used by more plugins
size_t elektraUnescapeKeyName (const char * source, char * dest);

// TODO: this is very similar to elektraKeyAppendMetaLine in keytometa
static int elektraKeyAppendLine (Key * target, const char * line)
{
	if (!target) return 0;
	if (!line) return 0;


	char * buffer = elektraMalloc (keyGetValueSize (target) + strlen (line) + 1);
	if (!buffer) return 0;

	keyGetString (target, buffer, keyGetValueSize (target));
	strcat (buffer, "\n");
	strncat (buffer, line, strlen (line));

	keySetString (target, buffer);
	elektraFree (buffer);
	return keyGetValueSize (target);
}


static void keyAddUnescapedBasePath (Key * key, const char * path)
{
	size_t size = 0;
	char * p = keyNameGetOneLevel (path + size, &size);
	if (*p && path[0] == '/')
	{
		keyAddBaseName (key, path);
		return;
	}
	while (*p)
	{
		char * buffer = elektraMalloc (size + 1);
		strncpy (buffer, p, size);
		buffer[size] = 0;
		int ret = keyAddName (key, buffer);
		if (ret == -1)
		{
			char * tmp = elektraMalloc (keyGetFullNameSize (key) + strlen (buffer));
			keyGetFullName (key, tmp, keyGetFullNameSize (key));
			strcat (tmp, "/");
			strcat (tmp, buffer);
			keySetName (key, tmp);
			elektraFree (tmp);
		}
		elektraFree (buffer);
		p = keyNameGetOneLevel (p + size, &size);
	}
}

static Key * createUnescapedKey (Key * key, const char * name)
{
	char * dupName = strdup (name);
	keyAddUnescapedBasePath (key, dupName);
	free (dupName);
	return key;
}

static void setOrderNumber (Key * parentKey, Key * key)
{
	kdb_long_long_t order = 1;
	const Key * orderKey = keyGetMeta (parentKey, "order");
	if (orderKey != NULL)
	{
		char * ptr = (char *)keyString (orderKey);
		++ptr; // skip #
		while (*ptr == '_')
		{
			++ptr;
		}
		elektraReadArrayNumber (ptr, &order);
	}
	++order;
	char buffer[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (buffer, order);
	keySetMeta (key, "order", buffer);
	keySetMeta (parentKey, "order", buffer);
}
static void setSubOrderNumber (Key * key, const char * oldOrder)
{
	char * lastIndexPtr = NULL;
	char * newOrder = elektraMalloc (elektraStrLen (oldOrder) + ELEKTRA_MAX_ARRAY_SIZE);
	if ((lastIndexPtr = strrchr (oldOrder, '/')))
	{
		kdb_long_long_t subIndex = 0;
		char * ptr = lastIndexPtr;
		++ptr; // skip /
		++ptr; // skip #
		while (*ptr == '_')
		{
			++ptr;
		}
		elektraReadArrayNumber (ptr, &subIndex);
		++subIndex;
		int len = (lastIndexPtr + 1) - oldOrder;
		char buffer[ELEKTRA_MAX_ARRAY_SIZE];
		elektraWriteArrayNumber (buffer, subIndex);
		sprintf (newOrder, "%.*s%s", len, oldOrder, buffer);
	}
	else
	{
		sprintf (newOrder, "%s/#1", oldOrder);
	}
	keySetMeta (key, "order", newOrder);
	elektraFree (newOrder);
}


static void iniBomHandler (void * vhandle, short BOM)
{
	CallbackHandle * handle = (CallbackHandle *)vhandle;
	IniPluginConfig * pluginConfig = (IniPluginConfig *)handle->pluginConfig;
	if (BOM)
	{
		pluginConfig->BOM = 1;
	}
	else
	{
		pluginConfig->BOM = 0;
	}
}

static void setKeyOrderNumber (Key * sectionKey, Key * key)
{
	const Key * childMeta = keyGetMeta (sectionKey, "ini/key/last");
	keySetMeta (key, "ini/key/number", keyString (childMeta));
	Key * newChild = keyDup (childMeta);
	keyAddName (newChild, keyString (newChild));
	elektraArrayIncName (newChild);
	keySetMeta (sectionKey, "ini/key/last", keyBaseName (newChild));
	keyDel (newChild);
	keySetMeta (key, "order", keyString (keyGetMeta (sectionKey, "order")));
}

static int iniKeyToElektraArray (CallbackHandle * handle, Key * existingKey, Key * appendKey, const char * value)
{

	if (keyGetMeta (existingKey, "ini/array"))
	{
		// array already exists, appending new key
		const char * lastIndex = keyString (keyGetMeta (existingKey, "ini/array"));
		keyAddBaseName (appendKey, lastIndex);
		keySetMeta (appendKey, "ini/array", 0);
		keySetMeta (appendKey, "order", 0);
		keySetMeta (appendKey, "parent", 0);
		if (elektraArrayIncName (appendKey) == 1)
		{
			return -1;
		}
		keySetString (appendKey, value);
		keySetMeta (appendKey, "ini/arrayMember", "");
		keySetMeta (appendKey, "order", keyString (keyGetMeta (existingKey, "order")));
		ksAppendKey (handle->result, appendKey);
		keySetMeta (existingKey, "ini/array", keyBaseName (appendKey));
		ksAppendKey (handle->result, existingKey);
	}
	else
	{
		// creating a new array
		Key * sectionKey = keyDup (appendKey);
		keyAddName (sectionKey, "..");
		char * origVal = strdup (keyString (existingKey));
		keySetString (appendKey, "");
		keySetMeta (appendKey, "ini/array", "#1");
		setOrderNumber (handle->parentKey, appendKey);
		keySetMeta (appendKey, "parent", 0);
		ksAppendKey (handle->result, keyDup (appendKey));
		keySetMeta (appendKey, "ini/arrayMember", "");
		keySetMeta (appendKey, "ini/array", 0);
		keySetMeta (appendKey, "parent", 0);
		keyAddName (appendKey, "#");
		if (elektraArrayIncName (appendKey) == -1)
		{
			free (origVal);
			return -1;
		}
		keySetString (appendKey, origVal);
		ksAppendKey (handle->result, keyDup (appendKey));
		free (origVal);
		if (elektraArrayIncName (appendKey) == -1)
		{
			return -1;
		}
		keySetMeta (appendKey, "parent", 0);
		keySetString (appendKey, value);
		ksAppendKey (handle->result, keyDup (appendKey));
		keyDel (appendKey);
		keyDel (sectionKey);
	}
	return 1;
}


static void insertKeyIntoKeySet (Key * parentKey, Key * key, KeySet * ks)
{
	cursor_t savedCursor = ksGetCursor (ks);
	char * parent = findParent (parentKey, key, ksDup (ks));
	keySetMeta (key, "parent", parent);
	if (keyIsBinary (key))
	{
		keySetMeta (key, "ini/key/last", "#0");
		Key * cutKey = keyNew (parent, KEY_END);
		KeySet * cutKS = ksCut (ks, cutKey);
		Key * cur;
		Key * prevKey = NULL;
		while ((cur = ksNext (cutKS)) != NULL)
		{
			if (!keyIsBinary (cur)) continue;
			if (strcmp (keyName (cur), keyName (key)) < 0)
				prevKey = cur;
			else
				break;
		}
		if (prevKey)
		{
			const Key * orderMeta = keyGetMeta (prevKey, "order");
			const char * oldOrder = keyString (orderMeta);
			setSubOrderNumber (key, oldOrder);
		}
		else
		{
			setOrderNumber (parentKey, key);
		}
		ksAppend (ks, cutKS);
		ksDel (cutKS);
		keyDel (cutKey);
	}
	else
	{
		Key * sectionKey = ksLookupByName (ks, parent, KDB_O_NONE);
		if (!sectionKey)
		{
			keySetMeta (key, "order", "#0");
		}
		else
		{
			setKeyOrderNumber (sectionKey, key);
		}
	}
	elektraFree (parent);
	ksSetCursor (ks, savedCursor);
}

static int iniKeyToElektraKey (void * vhandle, const char * section, const char * name, const char * value, unsigned short lineContinuation)
{
	CallbackHandle * handle = (CallbackHandle *)vhandle;
	if ((!section || *section == '\0') && (!name || *name == '\0'))
	{
		Key * rootKey = keyDup (handle->parentKey);
		keySetString (rootKey, value);
		ksAppendKey (handle->result, rootKey);
		return 1;
	}
	Key * appendKey = keyDup (handle->parentKey);
	Key * sectionKey;
	if (!section || *section == '\0')
	{
		if (!handle->toMeta)
		{
			section = INTERNAL_ROOT_SECTION;
		}
		else
		{
			Key * rootKey = ksLookup (handle->result, handle->parentKey, KDB_O_NONE);
			if (!rootKey)
			{
				rootKey = keyDup (handle->parentKey);
			}
			keySetMeta (rootKey, name, value);
			ksAppendKey (handle->result, rootKey);
		}
	}
	appendKey = createUnescapedKey (appendKey, section);
	if (!strcmp (keyBaseName (appendKey), INTERNAL_ROOT_SECTION))
	{
		sectionKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
		if (!sectionKey)
		{
			keySetMeta (appendKey, "order", "#0");
			keySetMeta (appendKey, "ini/key/last", "#0");
			keySetBinary (appendKey, 0, 0);
			ksAppendKey (handle->result, keyDup (appendKey));
			keySetMeta (appendKey, "order", "#0");
			keySetMeta (appendKey, "ini/key/last", "#0");
			keySetMeta (appendKey, "binary", 0);
			sectionKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
		}
	}
	else
	{
		sectionKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
	}
	short mergeSections = 0;
	Key * existingKey = NULL;
	if ((existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE)))
	{
		if (keyGetMeta (existingKey, "ini/duplicate"))
		{
			mergeSections = 1;
		}
	}
	if (handle->toMeta)
	{
		if (!existingKey) existingKey = appendKey;
		if (lineContinuation)
		{
			const Key * meta = keyGetMeta (existingKey, name);
			Key * newMeta = keyDup (meta);
			elektraKeyAppendLine (newMeta, value);
			keySetMeta (existingKey, name, keyString (newMeta));
			keyDel (newMeta);
		}
		else
		{
			keySetMeta (existingKey, name, value);
			ksAppendKey (handle->result, existingKey);
		}
		keyDel (appendKey);
		return 1;
	}
	appendKey = createUnescapedKey (appendKey, name);
	existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
	if (existingKey)
	{
		// a key with the same name already exists
		if (handle->array)
		{
			// array support is turned on
			return iniKeyToElektraArray (handle, existingKey, appendKey, value);
		}
		else if (!lineContinuation)
		{
			keyDel (appendKey);
			ELEKTRA_SET_ERRORF (141, handle->parentKey, "Key: %s\n", keyName (existingKey));
			return -1;
		}
	}

	if (value == NULL) keySetMeta (appendKey, "ini/empty", "");
	if (!lineContinuation)
	{
		flushCollectedComment (handle, appendKey);
		keySetString (appendKey, value);
		ksAppendKey (handle->result, appendKey);
		if (mergeSections)
		{
			keySetMeta (appendKey, "order", 0);
			insertKeyIntoKeySet (handle->parentKey, appendKey, handle->result);
		}
		else
		{
			setKeyOrderNumber (sectionKey, appendKey);
		}
	}
	else
	{
		existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
		keyDel (appendKey);
		/* something went wrong before because this key should exist */
		if (!existingKey) return -1;

		elektraKeyAppendLine (existingKey, value);
	}


	return 1;
}

static short isSectionKey (Key * key)
{
	if (!key) return 0;
	if (keyIsBinary (key))
		return 1;
	else
		return 0;
}

static int iniSectionToElektraKey (void * vhandle, const char * section)
{
	CallbackHandle * handle = (CallbackHandle *)vhandle;
	Key * appendKey = keyDup (handle->parentKey);
	keySetString (appendKey, 0);
	createUnescapedKey (appendKey, section);
	Key * existingKey = NULL;
	if ((existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE)))
	{
		if (handle->mergeSections) keySetMeta (existingKey, "ini/duplicate", "");
		keyDel (appendKey);
		return 1;
	}
	setOrderNumber (handle->parentKey, appendKey);
	keySetMeta (appendKey, "ini/key/last", "#0");
	keySetBinary (appendKey, 0, 0);
	flushCollectedComment (handle, appendKey);
	ksAppendKey (handle->result, appendKey);

	return 1;
}

static int iniCommentToMeta (void * vhandle, const char * comment)
{
	CallbackHandle * handle = (CallbackHandle *)vhandle;
	if (!handle->collectedComment) handle->collectedComment = keyNew ("/comments", KEY_CASCADING_NAME, KEY_END);
	elektraMetaArrayAdd (handle->collectedComment, "comments", comment);
	return 1;
}


int elektraIniOpen (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * config = elektraPluginGetConfig (handle);
	IniPluginConfig * pluginConfig = elektraMalloc (sizeof (IniPluginConfig));
	pluginConfig->lastOrder = NULL;
	pluginConfig->BOM = 0;
	Key * multilineKey = ksLookupByName (config, "/multiline", KDB_O_NONE);
	Key * sectionHandlingKey = ksLookupByName (config, "/section", KDB_O_NONE);
	Key * arrayKey = ksLookupByName (config, "/array", KDB_O_NONE);
	Key * mergeSectionsKey = ksLookupByName (config, "/mergesections", KDB_O_NONE);
	Key * toMetaKey = ksLookupByName (config, "/meta", KDB_O_NONE);
	Key * contStringKey = ksLookupByName (config, "/linecont", KDB_O_NONE);
	Key * delimiterKey = ksLookupByName (config, "/delimiter", KDB_O_NONE);

	if (!contStringKey)
	{
		pluginConfig->continuationString = strdup ("\t");
	}
	else
	{
		pluginConfig->continuationString = strdup (keyString (contStringKey));
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
		if (!strcmp (keyString (multilineKey), "0"))
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
		if (!strcasecmp (keyString (sectionHandlingKey), "NONE"))
		{
			pluginConfig->sectionHandling = NONE;
		}
		else if (!strcasecmp (keyString (sectionHandlingKey), "NULL"))
		{
			pluginConfig->sectionHandling = BINARY;
		}
		else if (!strcasecmp (keyString (sectionHandlingKey), "ALWAYS"))
		{
			pluginConfig->sectionHandling = ALWAYS;
		}
	}
	if (delimiterKey)
	{
		pluginConfig->delim = keyString (delimiterKey)[0];
	}
	else
	{
		pluginConfig->delim = DEFAULT_DELIMITER;
	}
	pluginConfig->oldKS = NULL;
	elektraPluginSetData (handle, pluginConfig);

	return 0;
}


int elektraIniClose (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	IniPluginConfig * pluginConfig = (IniPluginConfig *)elektraPluginGetData (handle);
	if (pluginConfig->oldKS) ksDel (pluginConfig->oldKS);
	if (pluginConfig->lastOrder) elektraFree (pluginConfig->lastOrder);
	elektraFree (pluginConfig->continuationString);
	elektraFree (pluginConfig);
	elektraPluginSetData (handle, 0);
	return 0;
}

#if DEBUG && VERBOSE
static void outputDebug () __attribute__ ((unused));

static void outputDebug (KeySet * ks)
{
	Key * cur;
	ksRewind (ks);
	while ((cur = ksNext (ks)) != NULL)
	{
		fprintf (stderr, "%s:(%.20s:_%.5x_)\t", keyName (cur), keyString (cur), *keyString (cur));
		fprintf (stderr, " sync: %d", keyNeedSync (cur));
		keyRewindMeta (cur);
		const Key * meta;
		while ((meta = keyNextMeta (cur)) != NULL)
		{
			fprintf (stderr, ", %s: %s", keyName (meta), (char *)keyValue (meta));
		}
		fprintf (stderr, "\n");
	}
}
#endif

static char * findParent (Key * parentKey, Key * searchkey, KeySet * ks)
{
	ksRewind (ks);
	size_t offset = 0;
	if (keyName (parentKey)[0] == '/' && keyName (searchkey)[0] != '/')
	{
		const char * ptr = strchr (keyName (searchkey) + 1, '/');
		if (ptr) offset = (ptr - keyName (searchkey)) + 1;
	}
	Key * key = keyDup (searchkey);
	Key * lookedUp;
	while (strcmp (keyName (key) + offset, keyName (parentKey)))
	{
		if (!strcmp (keyName (key), keyName (searchkey)))
		{
			if (keyAddName (key, "..") <= 0)
				break;
			else
				continue;
		}
		lookedUp = ksLookup (ks, key, KDB_O_NONE);
		if (lookedUp)
		{
			if (isSectionKey (lookedUp)) break;
		}

		if (keyAddName (key, "..") <= 0) break;
	}
	lookedUp = ksLookup (ks, key, KDB_O_NONE);
	if (!lookedUp) lookedUp = parentKey;
	char * parentName = strdup (keyName (lookedUp));
	keyDel (key);
	ksDel (ks);
	return parentName;
}
static void setParents (KeySet * ks, Key * parentKey)
{
	Key * cur;
	ksRewind (ks);
	while ((cur = ksNext (ks)) != NULL)
	{
		char * parentName = findParent (parentKey, cur, ksDup (ks));
		if (parentName)
		{
			keySetMeta (cur, "parent", parentName);
		}
		elektraFree (parentName);
	}
}
static void stripInternalData (Key * parentKey, KeySet *);

int elektraIniGet (Plugin * handle, KeySet * returned, Key * parentKey)
{

	/* get all keys */
	int errnosave = errno;

	if (!strcmp (keyName (parentKey), "system/elektra/modules/ini"))
	{
		KeySet * info = getPluginContract ();

		ksAppend (returned, info);
		ksDel (info);
		return 1;
	}


	FILE * fh = fopen (keyString (parentKey), "r");
	if (!fh)
	{
		ELEKTRA_SET_ERROR_GET (parentKey);
		errno = errnosave;
		return -1;
	}
	KeySet * append = ksNew (0, KS_END);
	CallbackHandle cbHandle;
	cbHandle.parentKey = parentKey;
	cbHandle.result = append;
	cbHandle.collectedComment = NULL;

	// ksAppendKey (cbHandle.result, keyDup(parentKey));

	struct IniConfig iniConfig;
	iniConfig.keyHandler = iniKeyToElektraKey;
	iniConfig.sectionHandler = iniSectionToElektraKey;
	iniConfig.commentHandler = iniCommentToMeta;
	iniConfig.bomHandler = iniBomHandler;
	IniPluginConfig * pluginConfig = elektraPluginGetData (handle);
	iniConfig.continuationString = pluginConfig->continuationString;
	iniConfig.supportMultiline = pluginConfig->supportMultiline;
	iniConfig.delim = pluginConfig->delim;
	pluginConfig->BOM = 0;
	if (pluginConfig->lastOrder && !keyGetMeta (parentKey, "order"))
		keySetMeta (parentKey, "order", pluginConfig->lastOrder);
	else
		keySetMeta (parentKey, "order", "#1");
	cbHandle.array = pluginConfig->array;
	cbHandle.mergeSections = pluginConfig->mergeSections;
	cbHandle.pluginConfig = pluginConfig;
	cbHandle.toMeta = pluginConfig->toMeta;
	int ret = ini_parse_file (fh, &iniConfig, &cbHandle);
	ksRewind (cbHandle.result);
	stripInternalData (cbHandle.parentKey, cbHandle.result);
	setParents (cbHandle.result, cbHandle.parentKey);
	fclose (fh);
	errno = errnosave;
	if (ret == 0)
	{
		ksClear (returned);
		ksAppend (returned, cbHandle.result);
		if (pluginConfig->sectionHandling == ALWAYS)
		{
			if (pluginConfig->oldKS) ksDel (pluginConfig->oldKS);
			pluginConfig->oldKS = ksDup (returned);
		}
		elektraPluginSetData (handle, pluginConfig);
		ret = 1;
	}
	else
	{
		switch (ret)
		{
		case -1:
			ELEKTRA_SET_ERROR (9, parentKey, "Unable to open the ini file");
			break;
		case -2:
			ELEKTRA_SET_ERROR (87, parentKey, "Memory allocation error while reading the ini file");
			break;
		default:
			ELEKTRA_SET_ERRORF (98, parentKey, "Could not parse ini file %s. First error at line %d", keyString (parentKey),
					    ret);
			break;
		}
		ret = -1;
	}
	ksDel (cbHandle.result);
	if (pluginConfig->lastOrder) elektraFree (pluginConfig->lastOrder);
	pluginConfig->lastOrder = strdup (keyString (keyGetMeta (parentKey, "order")));
	elektraPluginSetData (handle, pluginConfig);
	return ret; /* success */
}

// TODO: # and ; comments get mixed up, patch inih to differentiate and
// create comment keys instead of writing meta data. Wiriting the meta
// data can be done by keytometa then
void writeComments (Key * current, FILE * fh)
{
	char * toWrite = elektraMetaArrayToString (current, "comments", "\n");
	if (toWrite == NULL) return;
	fprintf (fh, "%s\n", toWrite);
	elektraFree (toWrite);
}
static int containsSpecialCharacter (const char *);

void writeMultilineKey (Key * key, const char * iniName, FILE * fh, IniPluginConfig * config)
{
	size_t valueSize = keyGetValueSize (key);
	char * saveptr = 0;
	char * result = 0;
	char * value = elektraMalloc (valueSize);
	keyGetString (key, value, valueSize);
	result = strtok_r (value, "\n", &saveptr);
	if (containsSpecialCharacter (iniName))
	{
		fprintf (fh, "\"%s\" %c ", iniName, config->delim);
	}
	else
	{
		fprintf (fh, "%s %c ", iniName, config->delim);
	}
	if (result == NULL)
		fprintf (fh, "\"\n%s\"", config->continuationString);
	else
	{
		if (containsSpecialCharacter (result))
			fprintf (fh, "\"%s\"\n", result);
		else
			fprintf (fh, "%s\n", result);
	}
	while ((result = strtok_r (0, "\n", &saveptr)) != 0)
	{
		if (containsSpecialCharacter (result))
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
static char * getIniName (Key * section, Key * key)
{
	if (!strcmp (keyName (section), keyName (key))) return strdup (keyBaseName (key));
	if (keyName (section)[0] == '/')
	{
		if (!strcmp (keyName (section), strchr (keyName (key) + 1, '/'))) return strdup (keyBaseName (key));
	}
	int slashCount = 0;
	char * slashCounter = (char *)keyName (key);
	while (*slashCounter)
	{
		if (*slashCounter == '/') ++slashCount;
		++slashCounter;
	}
	int len = 0;
	if (strcmp (keyName (section), "/")) len = strlen (keyName (section));
	char * buffer = elektraCalloc ((strlen (keyName (key)) - len) + slashCount + 1);
	char * ptr = NULL;
	if (!strcmp (keyName (section), "/"))
	{
		ptr = (char *)keyName (key);
	}
	else if (keyName (section)[0] == '/' && keyName (key)[0] != '/')
	{
		size_t offset = strchr (keyName (key) + 1, '/') - keyName (key);
		ptr = (char *)keyName (key) + strlen (keyName (section)) + offset + 1;
	}
	else
	{
		ptr = (char *)keyName (key) + strlen (keyName (section)) + 1;
	}

	size_t size = 0;
	char * tmp = strdup (ptr);
	char * p = keyNameGetOneLevel (tmp + size, &size);
	while (*p)
	{
		char * name = elektraMalloc (size + 1);
		strncpy (name, p, size);
		name[size] = 0;
		strcat (buffer, name);
		strcat (buffer, "/");
		elektraFree (name);
		p = keyNameGetOneLevel (p + size, &size);
	}
	free (tmp);
	buffer[strlen (buffer) - 1] = '\0';
	return buffer;
}

Key * getGlobalRoot (Key * parentKey, KeySet * ks)
{
	Key * lookup = keyDup (parentKey);
	keyAddName (lookup, INTERNAL_ROOT_SECTION);
	Key * found = ksLookup (ks, lookup, KDB_O_NONE);
	keyDel (lookup);
	return found;
}

int hasGlobalRoot (Key * parentKey, KeySet * ks)
{
	if (getGlobalRoot (parentKey, ks))
		return 1;
	else
		return 0;
}

void createGlobalRoot (Key * parentKey, KeySet * ks)
{
	Key * appendKey = keyDup (parentKey);
	keySetString (appendKey, 0);
	keySetMeta (appendKey, "order", 0);
	keySetMeta (appendKey, "binary", 0);
	keyAddName (appendKey, INTERNAL_ROOT_SECTION);
	keySetMeta (appendKey, "order", "#0");
	keySetMeta (appendKey, "ini/key/last", "#0");
	ksAppendKey (ks, appendKey);
}

void arrayHandler (Key * parentKey, Key * newKey, Key * cur, Key * sectionKey, KeySet * newKS)
{
	Key * arrayParentLookup = keyDup (newKey);
	keySetBaseName (arrayParentLookup, 0);
	Key * arrayParent = ksLookup (newKS, arrayParentLookup, KDB_O_NONE);
	keyDel (arrayParentLookup);
	const Key * arrayMeta = keyGetMeta (arrayParent, "ini/array");
	if (arrayMeta)
	{
		if (strcmp (keyString (arrayMeta), keyBaseName (cur)) < 0)
		{
			keySetMeta (arrayParent, "ini/array", keyBaseName (newKey));
			keySetMeta (newKey, "ini/arrayMember", "");
			keySetMeta (newKey, "order", keyString (keyGetMeta (arrayParent, "order")));
			keySetMeta (newKey, "ini/key/number", 0); // keyBaseName (newKey));
			ksAppendKey (newKS, newKey);
		}
	}
	else if (arrayParent && !keyIsBinary (arrayParent))
	{
		const char * oldVal = keyString (arrayParent);
		keySetMeta (arrayParent, "ini/array", keyBaseName (cur));
		keySetMeta (arrayParent, "ini/key/number", 0);
		if (oldVal && strlen (oldVal))
		{
			Key * arrayInitKey = keyDup (arrayParent);
			keyAddBaseName (arrayInitKey, "#0");
			keySetString (arrayInitKey, oldVal);
			keySetMeta (arrayInitKey, "ini/array", 0);
			keySetMeta (arrayInitKey, "ini/arrayMember", "");
			ksAppendKey (newKS, arrayInitKey);
			keySetMeta (arrayInitKey, "order", keyString (keyGetMeta (arrayParent, "order")));
			keySetMeta (arrayInitKey, "ini/key/number", 0); // keyBaseName(arrayInitKey));
		}
		ksAppendKey (newKS, newKey);
		keySetMeta (newKey, "order", keyString (keyGetMeta (arrayParent, "order")));
		keySetMeta (newKey, "ini/key/number", 0); // keyBaseName(newKey));
		keySetMeta (newKey, "ini/arrayMember", "");
	}
	else if (arrayParent && (keyBaseName (newKey)[1] == '0'))
	{
		Key * newParent = keyDup (arrayParent);
		keyAddName (newParent, "..");
		arrayParent = ksLookup (newKS, newParent, KDB_O_NONE);
		if (arrayParent)
		{
			keyDel (newParent);
		}
		else
		{
			arrayParent = newParent;
			keySetBinary (arrayParent, 0, 0);
			keySetMeta (arrayParent, "ini/array", keyBaseName (cur));
			ksAppendKey (newKS, arrayParent);
			insertKeyIntoKeySet (parentKey, arrayParent, newKS);
			keySetMeta (arrayParent, "ini/key/last", 0);
			keySetMeta (arrayParent, "ini/key/number", 0);
		}
		keySetBinary (newKey, 0, 0);
		keySetMeta (arrayParent, "ini/array", keyBaseName (cur));
		keyAddName (newKey, "..");
		ksAppendKey (newKS, newKey);
		insertKeyIntoKeySet (parentKey, newKey, newKS);
		keySetMeta (newKey, "binary", 0);
		keySetString (newKey, keyString (cur));
		keySetMeta (newKey, "ini/key/last", 0);
		keySetMeta (newKey, "ini/key/number", 0);
	}
	else if (keyIsDirectBelow (parentKey, newKey))
	{
		if (!hasGlobalRoot (parentKey, newKS))
		{
			createGlobalRoot (parentKey, newKS);
			keyAddName (sectionKey, INTERNAL_ROOT_SECTION);
		}
		else
		{
			keyDel (sectionKey);
			sectionKey = getGlobalRoot (parentKey, newKS);
		}
		keyDel (newKey);
		newKey = keyDup (sectionKey);
		keyAddBaseName (newKey, keyBaseName (cur));
	}
}

void insertIntoKS (Key * parentKey, Key * cur, KeySet * newKS, IniPluginConfig * pluginConfig)
{
	Key * appendKey = keyDup (parentKey);
	keyCopyAllMeta (appendKey, cur);
	keySetString (appendKey, 0);
	keySetMeta (appendKey, "order", 0);
	keySetMeta (appendKey, "binary", 0);
	Key * sectionKey = keyDup (appendKey);
	Key * newKey = NULL;
	keySetName (sectionKey, keyName (cur));
	keySetBinary (sectionKey, 0, 0);
	if (!keyIsBinary (cur))
	{
		keyAddName (sectionKey, "..");
		if (keyIsDirectBelow (parentKey, cur))
		{
			if (!hasGlobalRoot (parentKey, newKS))
			{
				createGlobalRoot (parentKey, newKS);
				keyAddName (sectionKey, INTERNAL_ROOT_SECTION);
			}
			else
			{
				keyDel (sectionKey);
				sectionKey = getGlobalRoot (parentKey, newKS);
			}
		}
		newKey = keyDup (sectionKey);
		keyAddBaseName (newKey, keyBaseName (cur));
		keySetMeta (newKey, "binary", 0);
		keySetString (newKey, keyString (cur));
	}
	if (pluginConfig->sectionHandling == ALWAYS || keyIsBinary (cur))
	{
		if (!ksLookup (newKS, sectionKey, KDB_O_NONE))
		{
			keySetBinary (sectionKey, 0, 0);
			ksAppendKey (newKS, sectionKey);
			insertKeyIntoKeySet (parentKey, sectionKey, newKS);
		}
		else
		{
			keyDel (sectionKey);
		}
	}
	else
	{
		keyDel (sectionKey);
	}
	if (newKey)
	{
		if ((elektraArrayValidateName (newKey) == 1) && pluginConfig->array)
		{
			arrayHandler (parentKey, newKey, cur, sectionKey, newKS);
		}
		else
		{
			ksAppendKey (newKS, newKey);
			insertKeyIntoKeySet (parentKey, newKey, newKS);
		}
	}
	keyDel (appendKey);
}

static int iniCmpOrder (const void * a, const void * b)
{
	const Key * ka = (*(const Key **)a);
	const Key * kb = (*(const Key **)b);

	if (!ka && !kb) return 0;
	if (ka && !kb) return 1;
	if (!ka && kb) return -1;

	const Key * kaom = keyGetMeta (ka, "order");
	const Key * kbom = keyGetMeta (kb, "order");
	const Key * kakm = keyGetMeta (ka, "ini/key/number");
	const Key * kbkm = keyGetMeta (kb, "ini/key/number");

	int ret = keyGetNamespace (ka) - keyGetNamespace (kb);
	if (!ret)
	{
		if (!kaom && !kbom) return 0;
		if (kaom && !kbom) return 1;
		if (!kaom && kbom) return -1;
		ret = strcmp (keyString (kaom), keyString (kbom));
	}
	if (!ret)
	{
		if (!kakm && kbkm) return -1;
		if (!kakm && !kbkm) return strcmp (keyName (ka), keyName (kb));
		if (kakm && !kbkm) return 1;
		if (kakm && kbkm) ret = strcmp (keyString (kakm), keyString (kbkm));
	}

	return ret;
}

static int containsSpecialCharacter (const char * str)
{
	char * ptr = (char *)str;
	if (isspace (*ptr) || (isspace (*(ptr + strlen (str) - 1)))) return 1;
	if (*ptr == '#' || *ptr == ';') return 1;
	if (*ptr == '[') return 1;
	while (*ptr)
	{
		if (*ptr == '"' || *ptr == '=')
		{
			return 1;
		}
		++ptr;
	}
	return 0;
}

static void iniWriteMeta (FILE * fh, Key * parentKey, Key * key, IniPluginConfig * config)
{
	uint8_t first = 1;
	keyRewindMeta (key);
	while (keyNextMeta (key) != NULL)
	{
		Key * meta = (Key *)keyCurrentMeta (key);
		const char * name = keyName (meta);
		if (strncmp (name, "ini/", 4) && strcmp (name, "order") && strcmp (name, "parent") && strcmp (name, "binary"))
		{
			if (first)
			{
				char * iniName = getIniName (parentKey, key);
				fprintf (fh, "[%s]\n", iniName);
				elektraFree (iniName);
				first = 0;
			}
			const char * string = keyString (meta);
			if (strstr (string, "\n") == 0)
			{
				if (containsSpecialCharacter (name))
					fprintf (fh, "\"%s\" = ", name);
				else
					fprintf (fh, "%s %c ", name, config->delim);
				if (strlen (string) && (containsSpecialCharacter (string)))
					fprintf (fh, "\"%s\"\n", string);
				else if (strlen (string))
					fprintf (fh, "%s\n", string);
			}
			else
			{
				writeMultilineKey (meta, name, fh, config);
			}
		}
	}
}


static int iniWriteKeySet (FILE * fh, Key * parentKey, KeySet * returned, IniPluginConfig * config)
{
	ksRewind (returned);
	Key ** keyArray;
	ssize_t arraySize = ksGetSize (returned);
	keyArray = elektraCalloc (arraySize * sizeof (Key *));
	elektraKsToMemArray (returned, keyArray);
	qsort (keyArray, arraySize, sizeof (Key *), iniCmpOrder);
	Key * cur = NULL;
	Key * sectionKey = parentKey;
	int ret = 1;
	const char delim = config->delim;
	if (config->toMeta)
	{
		for (ssize_t i = 0; i < arraySize; ++i)
		{
			cur = keyArray[i];
			if (!strcmp (keyName (parentKey), keyName (cur))) continue;
			if (keyGetValueSize (cur) <= 1) continue;
			char * name = getIniName (parentKey, cur);
			const char * string = keyString (cur);
			if (strstr (string, "\n") == 0)
			{
				if (containsSpecialCharacter (name))
					fprintf (fh, "\"%s\" %c ", name, delim);
				else
					fprintf (fh, "%s %c ", name, delim);
				if (strlen (string) && (containsSpecialCharacter (string)))
					fprintf (fh, "\"%s\"\n", string);
				else if (strlen (string))
					fprintf (fh, "%s\n", string);
			}
			else
			{
				writeMultilineKey (cur, name, fh, config);
			}
			elektraFree (name);
		}
	}

	int removeSectionKey = 0;

	for (ssize_t i = 0; i < arraySize; ++i)
	{
		cur = keyArray[i];

		if (!strcmp (keyName (parentKey), keyName (cur)))
		{
			continue;
		}
		if (keyName (parentKey)[0] == '/')
		{
			if (!strchr (keyName (cur) + 1, '/')) continue;
			if (!strcmp (keyName (parentKey), strchr (keyName (cur) + 1, '/'))) continue;
		}


		if (isSectionKey (cur))
		{
			if (removeSectionKey)
			{
				keyDel (sectionKey);
				sectionKey = parentKey;
				removeSectionKey = 0;
			}
			sectionKey = cur;
		}
		writeComments (cur, fh);
		if (config->toMeta)
		{
			iniWriteMeta (fh, parentKey, cur, config);
			continue;
		}
		else if (config->sectionHandling == NONE)
		{
			char * name = getIniName (parentKey, cur);
			if (!keyIsBinary (cur))
			{
				const char * string = keyString (cur);
				if (containsSpecialCharacter (name))
					fprintf (fh, "\"%s\" %c ", name, delim);
				else
					fprintf (fh, "%s %c ", name, delim);
				if (strlen (string) && (containsSpecialCharacter (string)))
					fprintf (fh, "\"%s\"\n", string);
				else
					fprintf (fh, "%s\n", string);
			}
			free (name);
		}
		else
		{
			if (isSectionKey (cur))
			{
				if (keyIsBelow (parentKey, cur))
				{
					char * iniName = getIniName (parentKey, cur);
					fprintf (fh, "[%s]\n", iniName);
					free (iniName);
				}
				else
				{
					sectionKey = parentKey;
					fprintf (fh, "[]\n");
				}
			}
			else if (!keyIsBinary (cur))
			{
				if (config->sectionHandling != NONE)
				{
					// handle possible section conflicts
					const Key * parentMeta = keyGetMeta (cur, "parent");
					if (parentMeta && !keyIsBelow (sectionKey, cur))
					{
						Key * oldSectionKey = sectionKey;
						sectionKey = keyNew (keyString (parentMeta), KEY_END);
						if (!keyIsBelow (oldSectionKey, sectionKey))
						{
							removeSectionKey = 1;
							if (keyIsBelow (parentKey, sectionKey))
							{
								char * name = getIniName (parentKey, sectionKey);
								fprintf (fh, "[%s]\n", name);
								elektraFree (name);
							}
							else if (!strcmp (keyName (parentKey), "/") &&
								 !strcmp (keyString (parentMeta), "/"))
							{
								fprintf (fh, "[]\n");
							}
							else if (!strcmp (keyName (parentKey), "/"))
							{
								fprintf (fh, "[%s]\n", keyString (parentMeta));
							}
							else if (!strcmp (keyName (sectionKey), keyName (parentKey)))
							{
								fprintf (fh, "[]\n");
							}
							else if (keyGetNamespace (sectionKey) != keyGetNamespace (oldSectionKey))
							{
								if (keyIsBelow (parentKey, sectionKey))
								{
									char * name = getIniName (parentKey, sectionKey);
									fprintf (fh, "[%s]\n", name);
									elektraFree (name);
								}
								else
									fprintf (fh, "[]\n");
							}
						}
						else
						{
							keyDel (sectionKey);
							sectionKey = oldSectionKey;
						}
					}
				}
				if (keyGetMeta (cur, "ini/array") && config->array)
				{
					int lastArrayIndex = atoi (keyString (keyGetMeta (cur, "ini/array")) + 1);
					keySetBaseName (cur, 0);

					char * name = getIniName (sectionKey, cur);
					++i;
					for (int j = i; j <= i + lastArrayIndex; ++j)
					{
						cur = keyArray[j];
						const char * string = keyString (cur);
						if (containsSpecialCharacter (name))
							fprintf (fh, "\"%s\" %c ", name, delim);
						else
							fprintf (fh, "%s %c ", name, delim);
						if (strlen (string) && (containsSpecialCharacter (string)))
							fprintf (fh, "\"%s\"\n", string);
						else
							fprintf (fh, "%s\n", string);
						if (lastArrayIndex == atoi (keyBaseName (cur) + 1))
						{
							lastArrayIndex = j - i;
							break;
						}
					}
					free (name);
					i += lastArrayIndex;
				}
				else
				{
					char * name;
					if (keyIsBelow (sectionKey, cur) && keyIsBelow (parentKey, cur))
					{
						if (keyIsBelow (parentKey, sectionKey))
						{
							name = getIniName (sectionKey, cur);
						}
						else
						{
							name = getIniName (parentKey, cur);
						}
					}
					else if (keyIsBelow (sectionKey, cur))
					{
						name = getIniName (sectionKey, cur);
					}
					else
					{
						name = getIniName (parentKey, cur);
					}

					if (keyGetMeta (cur, "ini/empty"))
					{
						if (containsSpecialCharacter (name))
							fprintf (fh, "\"%s\"\n", name);
						else
							fprintf (fh, "%s\n", name);
					}
					else if (strstr (keyString (cur), "\n") == 0)
					{
						const char * string = keyString (cur);
						if (containsSpecialCharacter (name))
							fprintf (fh, "\"%s\" %c ", name, delim);
						else
							fprintf (fh, "%s %c ", name, delim);
						if (strlen (string) && (containsSpecialCharacter (string)))
							fprintf (fh, "\"%s\"\n", string);
						else if (strlen (string))
							fprintf (fh, "%s\n", string);
						else
							fprintf (fh, "\n");
					}
					else
					{
						if (config->supportMultiline)
						{
							writeMultilineKey (cur, name, fh, config);
						}
						else
						{
							ELEKTRA_SET_ERROR (
								97, parentKey,
								"Encountered a multiline value but multiline support is not enabled. "
								"Have a look at kdb info ini for more details");
							ret = -1;
						}
					}
					free (name);
				}
			}
		}
	}
	if (removeSectionKey) keyDel (sectionKey);

	elektraFree (keyArray);
	return ret;
}
static void stripInternalData (Key * parentKey ELEKTRA_UNUSED, KeySet * ks)
{
	ksRewind (ks);
	Key * cur;
	KeySet * newKS = ksNew (ksGetSize (ks), KS_END);
	while ((cur = ksNext (ks)) != NULL)
	{
		if (!strcmp (keyBaseName (cur), INTERNAL_ROOT_SECTION))
		{
			keyDel (cur);
			continue;
		}
		if (strstr (keyName (cur), INTERNAL_ROOT_SECTION))
		{
			Key * newKey = keyDup (cur);
			char * oldName = strdup (keyName (cur));
			char * newName = elektraCalloc (elektraStrLen (keyName (cur)));
			char * token = NULL;
			token = strtok (oldName, "/");
			strcat (newName, token);
			while (token != NULL)
			{
				token = strtok (NULL, "/");
				if (token == NULL) break;
				if (!strcmp (token, INTERNAL_ROOT_SECTION)) continue;
				strcat (newName, "/");
				strcat (newName, token);
			}
			strcat (newName, "/");
			keySetName (newKey, newName);
			char * parent = findParent (parentKey, newKey, ksDup (newKS));
			keySetMeta (newKey, "parent", parent);
			elektraFree (parent);
			if (strcmp (keyName (parentKey), keyName (newKey))) ksAppendKey (newKS, keyDup (newKey));
			keyDel (newKey);
			elektraFree (oldName);
			elektraFree (newName);
		}
		else
		{
			if (!ksLookup (newKS, cur, KDB_O_NONE)) ksAppendKey (newKS, cur);
		}
	}
	ksClear (ks);
	ksAppend (ks, newKS);
	ksDel (newKS);
}

static void removeLeftoverSections (KeySet * ks, KeySet * oldKS, Key * parentKey)
{
	Key * cur;
	KeySet * removedKeys = ksNew (0, KS_END);
	ksRewind (oldKS);
	while ((cur = ksNext (oldKS)) != NULL)
	{
		Key * lookup = ksLookup (ks, cur, KDB_O_NONE);
		if (!lookup)
		{
			ksAppendKey (removedKeys, cur);
		}
	}
	ksRewind (removedKeys);
	while ((cur = ksPop (removedKeys)) != NULL)
	{
		const Key * parentMeta = keyGetMeta (cur, "parent");
		if (parentMeta && strcmp (keyName (parentKey), keyString (parentMeta)))
		{
			Key * sectionKey = ksLookupByName (ks, keyString (parentMeta), KDB_O_NONE);
			if (sectionKey)
			{
				KeySet * cutKS = ksCut (ks, sectionKey);
				Key * k;
				int empty = 1;
				ksRewind (cutKS);
				while (((k = ksNext (cutKS)) != NULL) && empty)
				{
					const Key * meta = keyGetMeta (k, "parent");
					if (meta && !strcmp (keyString (meta), keyName (sectionKey)))
					{
						empty = 0;
					}
				}
				if (empty)
				{
					keyDel (ksLookup (cutKS, sectionKey, KDB_O_POP));
				}
				ksAppend (ks, cutKS);
				ksDel (cutKS);
			}
		}
		keyDel (cur);
	}
	ksDel (removedKeys);
}

int elektraIniSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* set all keys */
	int errnosave = errno;
	int ret = 1;
	FILE * fh = fopen (keyString (parentKey), "w");

	if (!fh)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		errno = errnosave;
		return -1;
	}
	Key * root = keyDup (ksLookup (returned, parentKey, KDB_O_NONE));
	Key * head = keyDup (ksHead (returned));
	IniPluginConfig * pluginConfig = elektraPluginGetData (handle);
	if (pluginConfig->lastOrder && !keyGetMeta (parentKey, "order"))
		keySetMeta (parentKey, "order", pluginConfig->lastOrder);
	else
		keySetMeta (parentKey, "order", "#1");
	Key * cur;
	KeySet * newKS = ksNew (0, KS_END);
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyGetMeta (cur, "order"))
		{
			ksAppendKey (newKS, cur);
			keyDel (ksLookup (returned, cur, KDB_O_POP));
		}
	}
	ksRewind (returned);

	while ((cur = ksNext (returned)) != NULL)
	{
		if (!strcmp (keyName (cur), keyName (parentKey))) continue;
		if (!strcmp (keyBaseName (cur), INTERNAL_ROOT_SECTION)) continue;
		insertIntoKS (parentKey, cur, newKS, pluginConfig);
		keyDel (ksLookup (returned, cur, KDB_O_POP));
	}
	ksClear (returned);
	ksAppend (returned, newKS);
	ksDel (newKS);
	setParents (returned, parentKey);
	stripInternalData (parentKey, returned);
	if (pluginConfig->BOM == 1)
	{
		fprintf (fh, "\xEF\xBB\xBF");
	}
	if (keyNeedSync (parentKey) && root)
	{
		if (strncmp (keyString (parentKey), keyString (root), strlen (keyString (root))))
			if (keyString (root) && strlen (keyString (root))) fprintf (fh, "= %s\n", keyString (root));
	}
	keyDel (root);
	keyDel (head);

	KeySet * oldKS = pluginConfig->oldKS;
	if ((pluginConfig->sectionHandling == ALWAYS) && (ksGetSize (returned) < ksGetSize (oldKS)))
	{
		removeLeftoverSections (returned, oldKS, parentKey);
		ksDel (pluginConfig->oldKS);
		pluginConfig->oldKS = NULL;
		elektraPluginSetData (handle, pluginConfig);
	}
	ret = iniWriteKeySet (fh, parentKey, returned, pluginConfig);

	fclose (fh);
	errno = errnosave;
	if (pluginConfig->lastOrder) elektraFree (pluginConfig->lastOrder);
	pluginConfig->lastOrder = strdup (keyString (keyGetMeta (parentKey, "order")));
	elektraPluginSetData (handle, pluginConfig);


	return ret; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (ini)
{
	// clang-format off
	return elektraPluginExport ("ini",
			ELEKTRA_PLUGIN_OPEN, &elektraIniOpen,
			ELEKTRA_PLUGIN_CLOSE, &elektraIniClose,
			ELEKTRA_PLUGIN_GET, &elektraIniGet,
			ELEKTRA_PLUGIN_SET, &elektraIniSet, 
			ELEKTRA_PLUGIN_END);
}
