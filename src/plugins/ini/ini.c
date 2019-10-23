/**
 * @file
 *
 * @brief A plugin for reading and writing ini files
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
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
#define DEFAULT_COMMENT_CHAR '#'

char const * const ININAME_DELIM_SPECIAL_MASK = "\"%s\"%c";
char const * const ININAME_DELIM_MASK = "%s%c";

typedef enum
{
	NONE,
	BINARY,
	ALWAYS
} SectionHandling;

typedef struct
{
	short supportMultiline; /* defines whether multiline keys are supported */
	short preserverOrder;
	SectionHandling sectionHandling;
	short array;
	short mergeSections;
	short BOM;
	char * continuationString;
	char delim;
	char * lastOrder;
	char commentChar;
	KeySet * oldKS;
	Key * lastComments;
} IniPluginConfig;

typedef struct
{
	Key * parentKey;	/* the parent key of the result KeySet */
	KeySet * result;	/* the result KeySet */
	Key * collectedComment; /* buffer for collecting comments until a non comment key is reached */
	short array;
	short mergeSections;
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
		keyRewindMeta (handle->collectedComment);
		while ((cur = (Key *) keyNextMeta (handle->collectedComment)) != NULL)
		{
			if (!strncmp (keyName (cur), "meta/", 5)) keySetMeta (key, keyName (cur) + 5, keyString (cur));
		}
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
	strncat (buffer, line, elektraStrLen (line));

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
			char * tmp = elektraMalloc (keyGetFullNameSize (key) + strlen (buffer) + 2);
			keyGetFullName (key, tmp, keyGetFullNameSize (key));
			strcat (tmp, "/");
			strcat (tmp, buffer);
			ssize_t rc = keySetName (key, tmp);
			if (rc == -1 && tmp[strlen (tmp) - 1] == '\\')
			{
				tmp[strlen (tmp) - 1] = '\0';
				keySetName (key, tmp);
			}
			elektraFree (tmp);
		}
		elektraFree (buffer);
		p = keyNameGetOneLevel (p + size, &size);
	}
}

static Key * createUnescapedKey (Key * key, const char * name)
{
	char * dupName = elektraStrDup (name);
	keyAddUnescapedBasePath (key, dupName);
	free (dupName);
	return key;
}

static void setOrderNumber (Key * parentKey, Key * key)
{
	kdb_long_long_t order = 1;
	const Key * orderKey = keyGetMeta (parentKey, "internal/ini/order");
	if (orderKey != NULL)
	{
		char * ptr = (char *) keyString (orderKey);
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
	keySetMeta (key, "internal/ini/order", buffer);
	keySetMeta (parentKey, "internal/ini/order", buffer);
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
	keySetMeta (key, "internal/ini/order", newOrder);
	elektraFree (newOrder);
}


static void iniBomHandler (void * vhandle, short BOM)
{
	CallbackHandle * handle = (CallbackHandle *) vhandle;
	IniPluginConfig * pluginConfig = (IniPluginConfig *) handle->pluginConfig;
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
	const Key * childMeta = keyGetMeta (sectionKey, "internal/ini/key/last");
	keySetMeta (key, "internal/ini/key/number", keyString (childMeta));
	Key * newChild = keyDup (childMeta);
	keyAddName (newChild, keyString (newChild));
	elektraArrayIncName (newChild);
	keySetMeta (sectionKey, "internal/ini/key/last", keyBaseName (newChild));
	keyDel (newChild);
	keySetMeta (key, "internal/ini/order", keyString (keyGetMeta (sectionKey, "internal/ini/order")));
}

static int iniKeyToElektraArray (CallbackHandle * handle, Key * existingKey, Key * appendKey, const char * value)
{

	if (keyGetMeta (existingKey, "internal/ini/array"))
	{
		// array already exists, appending new key
		const char * lastIndex = keyString (keyGetMeta (existingKey, "internal/ini/array"));
		keyAddBaseName (appendKey, lastIndex);
		keySetMeta (appendKey, "internal/ini/array", 0);
		keySetMeta (appendKey, "internal/ini/order", 0);
		keySetMeta (appendKey, "internal/ini/parent", 0);
		if (elektraArrayIncName (appendKey) == 1)
		{
			return -1;
		}
		keySetString (appendKey, value);
		keySetMeta (appendKey, "internal/ini/arrayMember", "");
		keySetMeta (appendKey, "internal/ini/order", keyString (keyGetMeta (existingKey, "internal/ini/order")));
		ksAppendKey (handle->result, appendKey);
		keySetMeta (existingKey, "internal/ini/array", keyBaseName (appendKey));
		ksAppendKey (handle->result, existingKey);
	}
	else
	{
		// creating a new array
		Key * sectionKey = keyDup (appendKey);
		keyAddName (sectionKey, "..");
		char * origVal = elektraStrDup (keyString (existingKey));
		keySetString (appendKey, "");
		keySetMeta (appendKey, "internal/ini/array", "#1");
		setOrderNumber (handle->parentKey, appendKey);
		keySetMeta (appendKey, "internal/ini/parent", 0);
		ksAppendKey (handle->result, keyDup (appendKey));
		keySetMeta (appendKey, "internal/ini/arrayMember", "");
		keySetMeta (appendKey, "internal/ini/array", 0);
		keySetMeta (appendKey, "internal/ini/parent", 0);
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
		keySetMeta (appendKey, "internal/ini/parent", 0);
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
	keySetMeta (key, "internal/ini/parent", parent);
	if (keyGetMeta (key, "internal/ini/section"))
	{
		keySetMeta (key, "internal/ini/key/last", "#0");
		Key * cutKey = keyNew (parent, KEY_END);
		KeySet * cutKS = ksCut (ks, cutKey);
		Key * cur;
		Key * prevKey = NULL;
		while ((cur = ksNext (cutKS)) != NULL)
		{
			if (!keyGetMeta (cur, "internal/ini/section")) continue;
			if (strcmp (keyName (cur), keyName (key)) < 0)
				prevKey = cur;
			else
				break;
		}
		if (prevKey)
		{
			const Key * orderMeta = keyGetMeta (prevKey, "internal/ini/order");
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
			keySetMeta (key, "internal/ini/order", "#0");
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
	CallbackHandle * handle = (CallbackHandle *) vhandle;
	if ((!section || *section == '\0') && (!name || *name == '\0'))
	{
		Key * rootKey = keyNew (keyName (handle->parentKey), KEY_END);
		keySetString (rootKey, value);
		flushCollectedComment (handle, rootKey);
		ksAppendKey (handle->result, rootKey);
		return 1;
	}
	Key * appendKey = keyNew (keyName (handle->parentKey), KEY_END);
	Key * sectionKey;
	if (!section || *section == '\0')
	{
		section = INTERNAL_ROOT_SECTION;
	}
	appendKey = createUnescapedKey (appendKey, section);
	if (!strcmp (keyBaseName (appendKey), INTERNAL_ROOT_SECTION))
	{
		sectionKey = ksLookup (handle->result, appendKey, KDB_O_NONE);
		if (!sectionKey)
		{
			keySetMeta (appendKey, "internal/ini/order", "#0");
			keySetMeta (appendKey, "internal/ini/key/last", "#0");
			keySetMeta (appendKey, "internal/ini/section", "");
			ksAppendKey (handle->result, keyDup (appendKey));
			keySetMeta (appendKey, "internal/ini/order", "#0");
			keySetMeta (appendKey, "internal/ini/key/last", "#0");
			keySetMeta (appendKey, "internal/ini/section", 0);
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
		if (keyGetMeta (existingKey, "internal/ini/duplicate"))
		{
			mergeSections = 1;
		}
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
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (handle->parentKey,
								 "We found the key %s a second time in the INI file in section %s\n",
								 keyName (existingKey), section);
			return -1;
		}
	}

	if (value == NULL) keySetMeta (appendKey, "internal/ini/empty", "");
	if (!lineContinuation)
	{
		flushCollectedComment (handle, appendKey);
		keySetString (appendKey, value);
		ksAppendKey (handle->result, appendKey);
		if (mergeSections)
		{
			keySetMeta (appendKey, "internal/ini/order", 0);
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
	return key && keyGetMeta (key, "internal/ini/section");
}

static int iniSectionToElektraKey (void * vhandle, const char * section)
{
	CallbackHandle * handle = (CallbackHandle *) vhandle;
	Key * appendKey = keyNew (keyName (handle->parentKey), KEY_END);
	createUnescapedKey (appendKey, section);
	Key * existingKey = NULL;
	if ((existingKey = ksLookup (handle->result, appendKey, KDB_O_NONE)))
	{
		if (handle->mergeSections) keySetMeta (existingKey, "internal/ini/duplicate", "");
		keyDel (appendKey);
		return 1;
	}
	setOrderNumber (handle->parentKey, appendKey);
	keySetMeta (appendKey, "internal/ini/key/last", "#0");
	keySetMeta (appendKey, "internal/ini/section", "");
	flushCollectedComment (handle, appendKey);
	ksAppendKey (handle->result, appendKey);

	return 1;
}

static int iniCommentToMeta (void * vhandle, const char * comment)
{
	CallbackHandle * handle = (CallbackHandle *) vhandle;
	if (!handle->collectedComment) handle->collectedComment = keyNew ("/comments", KEY_CASCADING_NAME, KEY_END);
	if (strncmp (comment, "#@META ", 7))
		elektraMetaArrayAdd (handle->collectedComment, "comments", comment);
	else
	{
		// comment is an Elektra metakey

		// strip "#@META " from comment
		char * localCopy = elektraStrDup (comment + 7);
		size_t len = strlen (localCopy);
		char * ptr = localCopy;
		char * name = ptr;
		// skip keynames leading whitespace
		while (isspace (*name))
			++name;

		// locate key/value delimiter "="
		ptr = strstr (localCopy, "=");
		if (ptr)
		{

			// skip keynames trailing whitespace starting left of delimiter
			// and add nullbyte as string delimiter
			char * nameEnd = ptr - 1;
			while (isspace (*nameEnd))
				--nameEnd;
			*(nameEnd + 1) = '\0';
			if (*ptr)
			{

				*ptr = '\0';
				// skip leading whitespace and drop trailing whitespace
				char * value = ptr + 1;
				while (isspace (*value))
					++value;
				char * valueEnd = &localCopy[len - 1];
				while (isspace (*valueEnd))
					--valueEnd;
				if (*valueEnd) *(valueEnd + 1) = '\0';
				char buffer[strlen (name) + sizeof ("meta/") + 1];
				snprintf (buffer, sizeof (buffer), "meta/%s", name);
				keySetMeta (handle->collectedComment, buffer, value);
			}
		}
		elektraFree (localCopy);
	}
	return 1;
}


int elektraIniOpen (Plugin * handle, Key * parentKey ELEKTRA_UNUSED)
{
	KeySet * config = elektraPluginGetConfig (handle);
	IniPluginConfig * pluginConfig = elektraMalloc (sizeof (IniPluginConfig));
	pluginConfig->lastComments = NULL;
	pluginConfig->lastOrder = NULL;
	pluginConfig->BOM = 0;
	Key * multilineKey = ksLookupByName (config, "/multiline", KDB_O_NONE);
	Key * sectionHandlingKey = ksLookupByName (config, "/section", KDB_O_NONE);
	Key * arrayKey = ksLookupByName (config, "/array", KDB_O_NONE);
	Key * mergeSectionsKey = ksLookupByName (config, "/mergesections", KDB_O_NONE);
	Key * contStringKey = ksLookupByName (config, "/linecont", KDB_O_NONE);
	Key * delimiterKey = ksLookupByName (config, "/delimiter", KDB_O_NONE);
	Key * commentKey = ksLookupByName (config, "/comment", KDB_O_NONE);

	if (!commentKey)
	{
		pluginConfig->commentChar = DEFAULT_COMMENT_CHAR;
	}
	else
	{
		pluginConfig->commentChar = keyString (commentKey)[0];
	}


	if (!contStringKey)
	{
		pluginConfig->continuationString = elektraStrDup ("\t");
	}
	else
	{
		pluginConfig->continuationString = elektraStrDup (keyString (contStringKey));
	}
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
	IniPluginConfig * pluginConfig = elektraPluginGetData (handle);
	if (pluginConfig->oldKS) ksDel (pluginConfig->oldKS);
	if (pluginConfig->lastOrder) elektraFree (pluginConfig->lastOrder);
	if (pluginConfig->lastComments) keyDel (pluginConfig->lastComments);
	elektraFree (pluginConfig->continuationString);
	elektraFree (pluginConfig);
	elektraPluginSetData (handle, 0);
	return 0;
}

#if VERBOSE
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
			fprintf (stderr, ", %s: %s", keyName (meta), (char *) keyValue (meta));
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
	char * parentName = elektraStrDup (keyName (lookedUp));
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
			keySetMeta (cur, "internal/ini/parent", parentName);
		}
		elektraFree (parentName);
	}
}
static void stripInternalData (Key * parentKey, KeySet *);

static void incOrder (Key * key)
{
	Key * orderKey = keyNew ("/", KEY_CASCADING_NAME, KEY_END);
	keyAddName (orderKey, keyString (keyGetMeta (key, "internal/ini/order")));
	elektraArrayIncName (orderKey);
	keySetMeta (key, "internal/ini/order", keyBaseName (orderKey));
	keyDel (orderKey);
}

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
	ELEKTRA_LOG_DEBUG ("Opened file %s for reading", keyString (parentKey));

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
	if (pluginConfig->lastOrder && !keyGetMeta (parentKey, "internal/ini/order"))
		keySetMeta (parentKey, "internal/ini/order", pluginConfig->lastOrder);
	else if (!keyGetMeta (parentKey, "internal/ini/order"))
		keySetMeta (parentKey, "internal/ini/order", "#1");
	cbHandle.array = pluginConfig->array;
	cbHandle.mergeSections = pluginConfig->mergeSections;
	cbHandle.pluginConfig = pluginConfig;
	ELEKTRA_LOG_DEBUG ("Try to parse file");
	int ret = ini_parse_file (fh, &iniConfig, &cbHandle);
	ELEKTRA_LOG_DEBUG ("Parsed file");
	if (cbHandle.collectedComment)
	{
		pluginConfig->lastComments = keyDup (cbHandle.collectedComment);
		keyDel (cbHandle.collectedComment);
	}
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
			ELEKTRA_SET_RESOURCE_ERROR (parentKey, "Unable to open the ini file");
			break;
		case -2:
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey, "Memory allocation error while reading the ini file");
			break;
		default:
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Could not parse ini file %s. First error at line %d",
								 keyString (parentKey), ret);
			break;
		}
		ret = -1;
	}
	ksDel (cbHandle.result);
	if (pluginConfig->lastOrder) elektraFree (pluginConfig->lastOrder);
	incOrder (parentKey);
	pluginConfig->lastOrder = elektraStrDup (keyString (keyGetMeta (parentKey, "internal/ini/order")));
	elektraPluginSetData (handle, pluginConfig);
	return ret; /* success */
}

// TODO: # and ; comments get mixed up, patch inih to differentiate and
// create comment keys instead of writing metadata. Writing the meta
// data can be done by keytometa then
void writeComments (Key * current, FILE * fh, const char commentChar)
{
	const Key * meta = keyGetMeta (current, "comments");
	if (meta == NULL) return;
	Key * lookupKey = keyDup (meta);
	keyAddBaseName (lookupKey, "#0");
	const Key * comment = keyGetMeta (current, keyName (lookupKey));
	while (comment != NULL)
	{
		if (strlen (keyString (comment)) == 0)
		{
			fprintf (fh, "\n");
		}
		else
		{
			const char * ptr = keyString (comment);
			while (*ptr)
			{
				if (!isblank (*ptr))
				{
					if (*ptr == ';' || *ptr == '#')
						fprintf (fh, "%s\n", keyString (comment));
					else
						fprintf (fh, "%c%s\n", commentChar, keyString (comment));
					break;
				}
				++ptr;
			}
		}
		elektraArrayIncName (lookupKey);
		comment = keyGetMeta (current, keyName (lookupKey));
	}
	keyDel (lookupKey);
}
static int keyContainsSpecialCharacter (const char *);
static int valueContainsSpecialCharacter (const char *);

void writeMultilineKey (Key * key, const char * iniName, FILE * fh, IniPluginConfig * config)
{
	size_t valueSize = keyGetValueSize (key);
	char * saveptr = 0;
	char * result = 0;
	char * value = elektraMalloc (valueSize);
	keyGetString (key, value, valueSize);
	result = strtok_r (value, "\n", &saveptr);
	if (keyContainsSpecialCharacter (iniName))
	{
		fprintf (fh, ININAME_DELIM_SPECIAL_MASK, iniName, config->delim);
	}
	else
	{
		fprintf (fh, ININAME_DELIM_MASK, iniName, config->delim);
	}
	if (result == NULL)
		fprintf (fh, "\"\n%s\"", config->continuationString);
	else
	{
		if (valueContainsSpecialCharacter (result))
			fprintf (fh, "\"%s\"\n", result);
		else
			fprintf (fh, "%s\n", result);
	}
	while ((result = strtok_r (0, "\n", &saveptr)) != 0)
	{
		if (valueContainsSpecialCharacter (result))
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
	if (!strcmp (keyName (section), keyName (key))) return elektraStrDup (keyBaseName (key));
	if (keyName (section)[0] == '/')
	{
		if (!strcmp (keyName (section), strchr (keyName (key) + 1, '/'))) return elektraStrDup (keyBaseName (key));
	}
	int slashCount = 0;
	char * slashCounter = (char *) keyName (key);
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
		ptr = (char *) keyName (key);
	}
	else if (keyName (section)[0] == '/' && keyName (key)[0] != '/')
	{
		size_t offset = strchr (keyName (key) + 1, '/') - keyName (key);
		ptr = (char *) keyName (key) + strlen (keyName (section)) + offset + 1;
	}
	else
	{
		ptr = (char *) keyName (key) + strlen (keyName (section)) + 1;
	}

	size_t size = 0;
	char * tmp = elektraStrDup (ptr);
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
	Key * lookup = keyNew (keyName (parentKey), KEY_END);
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
	Key * appendKey = keyNew (keyName (parentKey), KEY_END);
	keyAddName (appendKey, INTERNAL_ROOT_SECTION);
	keySetMeta (appendKey, "internal/ini/order", "#0");
	keySetMeta (appendKey, "internal/ini/key/last", "#0");
	ksAppendKey (ks, appendKey);
}

void arrayHandler (Key * parentKey, Key * newKey, Key * cur, Key * sectionKey, KeySet * newKS)
{
	Key * arrayParentLookup = keyDup (newKey);
	keySetBaseName (arrayParentLookup, 0);
	Key * arrayParent = ksLookup (newKS, arrayParentLookup, KDB_O_NONE);
	keyDel (arrayParentLookup);
	const Key * arrayMeta = keyGetMeta (arrayParent, "internal/ini/array");
	if (arrayMeta)
	{
		if (strcmp (keyString (arrayMeta), keyBaseName (cur)) < 0)
		{
			keySetMeta (arrayParent, "internal/ini/array", keyBaseName (newKey));
			keySetMeta (newKey, "internal/ini/arrayMember", "");
			keySetMeta (newKey, "internal/ini/order", keyString (keyGetMeta (arrayParent, "internal/ini/order")));
			keySetMeta (newKey, "internal/ini/key/number", 0); // keyBaseName (newKey));
			ksAppendKey (newKS, newKey);
		}
	}
	else if (arrayParent && !keyGetMeta (arrayParent, "internal/ini/section"))
	{
		const char * oldVal = keyString (arrayParent);
		keySetMeta (arrayParent, "internal/ini/array", keyBaseName (cur));
		keySetMeta (arrayParent, "internal/ini/key/number", 0);
		if (oldVal && strlen (oldVal))
		{
			Key * arrayInitKey = keyDup (arrayParent);
			keyAddBaseName (arrayInitKey, "#0");
			keySetString (arrayInitKey, oldVal);
			keySetMeta (arrayInitKey, "internal/ini/array", 0);
			keySetMeta (arrayInitKey, "internal/ini/arrayMember", "");
			ksAppendKey (newKS, arrayInitKey);
			keySetMeta (arrayInitKey, "internal/ini/order", keyString (keyGetMeta (arrayParent, "internal/ini/order")));
			keySetMeta (arrayInitKey, "internal/ini/key/number", 0); // keyBaseName(arrayInitKey));
		}
		ksAppendKey (newKS, newKey);
		keySetMeta (newKey, "internal/ini/order", keyString (keyGetMeta (arrayParent, "internal/ini/order")));
		keySetMeta (newKey, "internal/ini/key/number", 0); // keyBaseName(newKey));
		keySetMeta (newKey, "internal/ini/arrayMember", "");
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
			keySetMeta (arrayParent, "internal/ini/section", "");
			keySetMeta (arrayParent, "internal/ini/array", keyBaseName (cur));
			ksAppendKey (newKS, arrayParent);
			insertKeyIntoKeySet (parentKey, arrayParent, newKS);
			keySetMeta (arrayParent, "internal/ini/key/last", 0);
			keySetMeta (arrayParent, "internal/ini/key/number", 0);
		}
		keySetMeta (newKey, "internal/ini/section", "");
		keySetMeta (arrayParent, "internal/ini/array", keyBaseName (cur));
		keyAddName (newKey, "..");
		ksAppendKey (newKS, newKey);
		insertKeyIntoKeySet (parentKey, newKey, newKS);
		keySetMeta (newKey, "internal/ini/section", 0);
		keySetString (newKey, keyString (cur));
		keySetMeta (newKey, "internal/ini/key/last", 0);
		keySetMeta (newKey, "internal/ini/key/number", 0);
	}
	else if (keyIsDirectlyBelow (parentKey, newKey))
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
	Key * appendKey = keyNew (keyName (parentKey), KEY_END);
	Key * sectionKey = keyDup (appendKey);
	Key * newKey = NULL;
	keySetName (sectionKey, keyName (cur));
	keySetMeta (sectionKey, "internal/ini/section", "");
	if (!keyGetMeta (cur, "internal/ini/section") && !keyIsBinary (cur))
	{
		keyAddName (sectionKey, "..");
		if (keyIsDirectlyBelow (parentKey, cur))
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
		keySetMeta (newKey, "internal/ini/section", 0);
		keyCopyAllMeta (newKey, cur);
		keySetString (newKey, keyString (cur));
	}
	else
	{
		keyCopyAllMeta (sectionKey, cur);
	}
	if (pluginConfig->sectionHandling == ALWAYS || keyGetMeta (cur, "internal/ini/section") || keyIsBinary (cur))
	{
		if (!ksLookup (newKS, sectionKey, KDB_O_NONE))
		{
			keySetMeta (sectionKey, "internal/ini/section", "");
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
	const Key * ka = (*(const Key **) a);
	const Key * kb = (*(const Key **) b);

	if (!ka && !kb) return 0;
	if (ka && !kb) return 1;
	if (!ka && kb) return -1;

	const Key * kaom = keyGetMeta (ka, "internal/ini/order");
	const Key * kbom = keyGetMeta (kb, "internal/ini/order");
	const Key * kakm = keyGetMeta (ka, "internal/ini/key/number");
	const Key * kbkm = keyGetMeta (kb, "internal/ini/key/number");

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

// test if the keyname contains a reserved character and needs to be quoted
static int keyContainsSpecialCharacter (const char * str)
{
	char * ptr = (char *) str;
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

// test if the keyvalue contains a reserved character and needs to be quoted
static int valueContainsSpecialCharacter (const char * str)
{
	char * ptr = (char *) str;
	if (isspace (*ptr) || (isspace (*(ptr + strlen (str) - 1)))) return 1;
	if (*ptr == '#' || *ptr == ';') return 1;
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

static void iniWriteMeta (FILE * fh, Key * key)
{
	keyRewindMeta (key);
	while (keyNextMeta (key) != NULL)
	{
		Key * meta = (Key *) keyCurrentMeta (key);
		const char * name = keyName (meta);
		if (strncmp (name, "internal/", 9) && strcmp (name, "internal/ini/section") && strncmp (name, "comment", 7) &&
		    strncmp (name, "warnings/", 9) && strncmp (name, "error/", 6) && strcmp (name, "warnings") && strcmp (name, "error"))
		{
			if (!strcmp (name, "binary") && keyGetNamespace (key) != KEY_NS_SPEC) continue;
			const char * string = keyString (meta);
			fprintf (fh, "#@META %s = %s\n", name, string);
		}
	}
}


static int iniWriteKeySet (FILE * fh, Key * parentKey, KeySet * returned, IniPluginConfig * config)
{
	ksRewind (returned);
	Key ** keyArray;
	ssize_t arraySize = ksGetSize (returned);
	if (arraySize == 0) return 0;
	keyArray = elektraCalloc (arraySize * sizeof (Key *));
	elektraKsToMemArray (returned, keyArray);
	qsort (keyArray, arraySize, sizeof (Key *), iniCmpOrder);
	Key * cur = NULL;
	Key * sectionKey = parentKey;
	int ret = 1;
	const char delim = config->delim;

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
			else
			{
				sectionKey = cur;
			}
		}
		writeComments (cur, fh, config->commentChar);
		iniWriteMeta (fh, cur);
		if (config->sectionHandling == NONE)
		{
			char * name = getIniName (parentKey, cur);
			if (!keyGetMeta (cur, "internal/ini/section"))
			{
				const char * string = keyString (cur);
				if (keyContainsSpecialCharacter (name))
					fprintf (fh, ININAME_DELIM_SPECIAL_MASK, name, delim);
				else
					fprintf (fh, ININAME_DELIM_MASK, name, delim);
				if (strlen (string) && (valueContainsSpecialCharacter (string)))
					fprintf (fh, "\"%s\"\n", string);
				else
					fprintf (fh, "%s\n", string);
			}
			free (name);
		}
		else
		{
			if (isSectionKey (cur) && keyGetValueSize (cur) <= 1)
			{
				if (keyIsBelow (parentKey, cur))
				{
					char * iniName = getIniName (parentKey, cur);
					fprintf (fh, "[%s]\n", iniName);
					free (iniName);
				}
				else
				{
					if (removeSectionKey)
					{
						keyDel (sectionKey);
						removeSectionKey = 0;
					}
					sectionKey = parentKey;
					fprintf (fh, "[]\n");
				}
			}
			else
			{
				// handle possible section conflicts
				const Key * parentMeta = keyGetMeta (cur, "internal/ini/parent");
				if (parentMeta && !keyIsBelow (sectionKey, cur))
				{
					Key * oldSectionKey = sectionKey;
					sectionKey = keyNew (keyString (parentMeta), KEY_END);
					if (!keyIsBelow (oldSectionKey, sectionKey))
					{
						if (keyIsBelow (parentKey, sectionKey))
						{
							char * name = getIniName (parentKey, sectionKey);
							fprintf (fh, "[%s]\n", name);
							elektraFree (name);
						}
						else if (!strcmp (keyName (parentKey), "/") && !strcmp (keyString (parentMeta), "/"))
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

						if (removeSectionKey)
						{
							// remove previous sectionKey
							keyDel (oldSectionKey);
						}

						// mark allocated sectionKey to be freed later
						removeSectionKey = 1;
					}
					else
					{
						keyDel (sectionKey);
						sectionKey = oldSectionKey;
						removeSectionKey = 0;
					}
				}
				if (keyGetMeta (cur, "internal/ini/array") && config->array)
				{
					int lastArrayIndex = atoi (keyString (keyGetMeta (cur, "internal/ini/array")) + 1);
					keySetBaseName (cur, 0);

					char * name = getIniName (sectionKey, cur);
					++i;
					for (int j = i; j <= i + lastArrayIndex; ++j)
					{
						cur = keyArray[j];
						const char * string = keyString (cur);
						if (keyContainsSpecialCharacter (name))
							fprintf (fh, ININAME_DELIM_SPECIAL_MASK, name, delim);
						else
							fprintf (fh, ININAME_DELIM_MASK, name, delim);
						if (strlen (string) && (valueContainsSpecialCharacter (string)))
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

					if (keyGetMeta (cur, "internal/ini/empty"))
					{
						if (keyContainsSpecialCharacter (name))
							fprintf (fh, "\"%s\"\n", name);
						else
							fprintf (fh, "%s\n", name);
					}
					else if (strstr (keyString (cur), "\n") == 0)
					{
						const char * string = keyString (cur);
						if (keyContainsSpecialCharacter (name))
							fprintf (fh, ININAME_DELIM_SPECIAL_MASK, name, delim);
						else
							fprintf (fh, ININAME_DELIM_MASK, name, delim);
						if (strlen (string) && (valueContainsSpecialCharacter (string)))
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
							ELEKTRA_SET_INSTALLATION_ERROR (
								parentKey,
								"Encountered a multiline value but multiline support is not enabled. "
								"Have a look at kdb plugin-info ini for more details");
							ret = -1;
						}
					}
					free (name);
				}
			}
		}
	}
	if (config->lastComments)
	{
		writeComments (config->lastComments, fh, config->commentChar);
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
			char * oldName = elektraStrDup (keyName (cur));
			char * newName = elektraCalloc (elektraStrLen (keyName (cur)));
			char * token = strtok (oldName, "/");
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
			keySetMeta (newKey, "internal/ini/parent", parent);
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
		const Key * parentMeta = keyGetMeta (cur, "internal/ini/parent");
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
					const Key * meta = keyGetMeta (k, "internal/ini/parent");
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
	if (pluginConfig->lastOrder && !keyGetMeta (parentKey, "internal/ini/order"))
	{
		keySetMeta (parentKey, "internal/ini/order", pluginConfig->lastOrder);
		incOrder (parentKey);
	}
	else if (!keyGetMeta (parentKey, "internal/ini/order"))
	{
		keySetMeta (parentKey, "internal/ini/order", "#1");
	}
	else
	{
		incOrder (parentKey);
	}
	Key * cur;
	KeySet * newKS = ksNew (0, KS_END);
	ksRewind (returned);
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyGetMeta (cur, "internal/ini/order"))
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

	int rootNeededSync = 0;
	if (keyNeedSync (parentKey) && root)
	{
		if (strncmp (keyString (parentKey), keyString (root), strlen (keyString (root))))
		{
			if (keyString (root) && strlen (keyString (root)))
			{
				iniWriteMeta (fh, root);
				fprintf (fh, "= %s\n", keyString (root));
				rootNeededSync = 1;
			}
		}
		else
		{
			iniWriteMeta (fh, root);
			fprintf (fh, "[]\n");
			rootNeededSync = 1;
		}
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
	pluginConfig->lastOrder = elektraStrDup (keyString (keyGetMeta (parentKey, "internal/ini/order")));
	elektraPluginSetData (handle, pluginConfig);

	if (ret == 0 && rootNeededSync) return ELEKTRA_PLUGIN_STATUS_SUCCESS;

	return ret; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("ini",
	    ELEKTRA_PLUGIN_OPEN, &elektraIniOpen,
	    ELEKTRA_PLUGIN_CLOSE, &elektraIniClose,
	    ELEKTRA_PLUGIN_GET, &elektraIniGet,
	    ELEKTRA_PLUGIN_SET, &elektraIniSet,
	    ELEKTRA_PLUGIN_END);
}
