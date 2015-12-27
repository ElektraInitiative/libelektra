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
#include <kdbproposal.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbos.h>
#include <inih.h>
#include "ini.h"

int elektraIniOpen(Plugin *handle, Key *parentKey);
int elektraIniClose(Plugin *handle, Key *parentKey);

#include "contract.h"

#define INTERNAL_ROOT_SECTION "GLOBALROOT"

typedef enum{NONE, BINARY, ALWAYS}SectionHandling;

typedef struct {
    Key *parentKey;	/* the parent key of the result KeySet */
    KeySet *result;			/* the result KeySet */
    char *collectedComment;	/* buffer for collecting comments until a non comment key is reached */
    short array;
    short mergeSections;
} CallbackHandle;

typedef struct {
    short supportMultiline;	/* defines whether multiline keys are supported */
    short preserverOrder;
    SectionHandling sectionHandling;
    short array;
    short mergeSections;
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
        keyAddName(lookupKey, "..");
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
        char *ptr = keyString(orderKey);
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

static void insertNewKeyIntoExistendOrder(Key *key, KeySet *ks)
{
    if (keyGetMeta(ksLookup(ks, key, KDB_O_NONE), "order"))
        return;
    ksRewind(ks);	
    Key *curKey;
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
        prevKey = curKey;
    }
}

static int iniKeyToElektraKey (void *vhandle, const char *section, const char *name, const char *value, unsigned short lineContinuation)
{
    CallbackHandle *handle = (CallbackHandle *)vhandle;
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
    setSectionNumber(handle->parentKey, appendKey, handle->result);
    appendKey = createUnescapedKey(appendKey, name);
    existingKey = ksLookup(handle->result, appendKey, KDB_O_NONE);

    if (existingKey)
    {
        //a key with the same name already exists
        if (handle->array)
        {
            //array support is turned on 
            keySetMeta(appendKey, "ini/section", 0);
            if (keyGetMeta(existingKey, "ini/array"))
            {
                //array already exists, appending new key
                const char *lastIndex = keyString(keyGetMeta(existingKey, "ini/array"));
                keyAddBaseName(appendKey, lastIndex);
                keySetMeta(appendKey, "order/parent", 0);
                keySetMeta(appendKey, "ini/array", 0);
                keySetMeta(appendKey, "order", 0);
                if (elektraArrayIncName(appendKey) == 1)
                {
                    return -1;
                }
                keySetString(appendKey, value);
                keySetMeta(appendKey, "ini/key", 0);
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
                keySetMeta(appendKey, "order/parent", keyName(sectionKey));
                setSectionNumber(handle->parentKey, appendKey, handle->result);
                setOrderNumber(handle->parentKey, appendKey);
                keySetMeta(appendKey, "ini/key", "");
                ksAppendKey(handle->result, keyDup(appendKey));
                keySetMeta(appendKey, "ini/key", 0);
                keySetMeta(appendKey, "ini/array", 0);
                keySetMeta(appendKey, "parent", 0);
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
        else if(!lineContinuation)
        {
            ELEKTRA_SET_ERRORF(140, handle->parentKey, "Key: %s\n", name);
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
    keySetMeta(appendKey, "ini/lastSection", 0);
    createUnescapedKey(appendKey, section);
    Key *existingKey = NULL;
    if ((existingKey = ksLookup(handle->result, appendKey, KDB_O_NONE)))
    {
        keyDel(appendKey);
        if(!handle->mergeSections)
        {
            ELEKTRA_SET_ERRORF(139, handle->parentKey, "Section name: %s\n", section);
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
    Key *multilineKey = ksLookupByName (config, "/multiline", KDB_O_NONE);
    Key *sectionHandlingKey = ksLookupByName(config, "/section", KDB_O_NONE);
    Key *arrayKey = ksLookupByName(config, "/array", KDB_O_NONE);
    Key *mergeSectionsKey = ksLookupByName(config, "/mergeSections", KDB_O_NONE);
    pluginConfig->mergeSections = mergeSectionsKey != 0;
    pluginConfig->array = arrayKey != 0;
    if(!multilineKey)
    {
        pluginConfig->supportMultiline = 1;
    }
    else
    {
        if(!strcmp(keyString(multilineKey), "0"))
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
    elektraFree(pluginConfig);
    elektraPluginSetData(handle, 0);
    return 0;
}

static void outputDebug(KeySet *ks)
{
    Key *cur;
    ksRewind(ks);
    while ((cur = ksNext(ks)) != NULL)
    {
        fprintf(stderr, "%s:(%s)\t", keyName(cur), keyString(cur));
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
static const char *findParent(Key *parentKey, Key *searchkey, KeySet *ks)
{
    Key *key = keyDup(searchkey);
    Key *lookedUp;
    while (strcmp(keyName(key), keyName(parentKey)))
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
    while ((cur = ksNext(ks)) != NULL)
    {
        const char *parentName = findParent(parentKey, cur, ksDup(ks));
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
    ksAppendKey (cbHandle.result, keyDup(parentKey));

    struct IniConfig iniConfig;
    iniConfig.keyHandler=iniKeyToElektraKey;
    iniConfig.sectionHandler = iniSectionToElektraKey;
    iniConfig.commentHandler = iniCommentToMeta;
    IniPluginConfig *pluginConfig = elektraPluginGetData(handle);
    iniConfig.supportMultiline = pluginConfig->supportMultiline;

    cbHandle.array = pluginConfig->array;
    cbHandle.mergeSections = pluginConfig->mergeSections;
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
    char *ptr = (char *)keyName(key)+strlen(keyName(section))+1;
    char *strPos = strstr(keyName(key), INTERNAL_ROOT_SECTION);
    if (strPos == ((char *)keyName(key)+strlen(keyName(section))+1))
    {
        ptr += (strlen(INTERNAL_ROOT_SECTION)+1);
    }
    for (; *ptr; ++ptr)
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

static void insertSectionIntoExistingOrder(Key *appendKey, KeySet *newKS)
{
    char *lastOrderNumber = NULL;
    int sectionNumber = atoi(keyString(keyGetMeta(appendKey, "ini/section")));
    KeySet *searchKS = ksDup(newKS);
    ksRewind(searchKS);
    Key *looking;
    while ((looking = ksNext(searchKS)) != NULL)
    {
        if (atoi(keyString(keyGetMeta(looking, "ini/section"))) == sectionNumber)
            break;
    }
    lastOrderNumber = keyString(keyGetMeta(looking, "order"));
    KeySet *cutKS = ksCut(searchKS, looking);
    ksRewind(cutKS);
    while ((looking = ksNext(cutKS)) != NULL)
    {
        if (strcmp(keyString(keyGetMeta(looking, "order")), lastOrderNumber) > 0)
            lastOrderNumber = keyString(keyGetMeta(looking, "order"));
    }

    char *newOrder = elektraMalloc(elektraStrLen(lastOrderNumber)+ELEKTRA_MAX_ARRAY_SIZE);
    char *lastIndexPtr = NULL;
    if ((lastIndexPtr = strrchr(lastOrderNumber, '/')))
    {
        kdb_long_long_t subIndex = 0;
        char *ptr = lastIndexPtr;
        ++ptr; //skip /
        ++ptr; //skip #
        while (*ptr == '_')
            ++ptr;
        elektraReadArrayNumber(ptr, &subIndex);
        ++subIndex;
        int len = (lastIndexPtr+1) - lastOrderNumber;
        char buffer[ELEKTRA_MAX_ARRAY_SIZE];
        elektraWriteArrayNumber(buffer, subIndex);
        sprintf(newOrder, "%.*s%s", len, lastOrderNumber, buffer);
    }
    else
    {
        sprintf(newOrder, "%s/#1", lastOrderNumber);
    }
    keySetMeta(appendKey, "order", newOrder);
    elektraFree(newOrder);
    ksDel(cutKS);
    ksDel(searchKS);
}

void insertIntoKS(Key *parentKey, Key *cur, KeySet *newKS, IniPluginConfig *pluginConfig)
{
    Key *appendKey = keyDup(parentKey);
    keySetMeta(appendKey, "ini/lastSection", 0);
    keySetString(appendKey, 0);
    keySetMeta(appendKey, "order", 0);
    keySetMeta(appendKey, "binary", 0);

    char *oldSectionNumber = strdup(keyString(keyGetMeta(parentKey, "ini/lastSection")));

    if (keyIsBinary(cur))
    {
        // create new section here
        const char *sectionName = keyName(cur)+strlen(keyName(parentKey))+1;
        createUnescapedKey(appendKey, sectionName);
        setSectionNumber(parentKey, appendKey, newKS);
        keySetBinary(appendKey, 0, 0);
        ksAppendKey(newKS, appendKey);
        if (atoi(oldSectionNumber) < atoi(keyString(keyGetMeta(appendKey, "ini/section"))))
        {
            setOrderNumber(parentKey, appendKey);
        }
        else
        {
            insertSectionIntoExistingOrder(appendKey, newKS);
        }
    }
    else if (keyIsDirectBelow(parentKey, cur))
    {
        // create global key here
        const char *name = keyName(cur)+strlen(keyName(parentKey))+1;
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
        const char *sectionName = keyName(sectionKey)+strlen(keyName(parentKey))+1;
        appendKey = createUnescapedKey(appendKey, sectionName);
        if ((!strcmp(keyBaseName(appendKey), INTERNAL_ROOT_SECTION)) && (!ksLookup(newKS, appendKey, KDB_O_NONE)))
            keySetMeta(appendKey, "order", "#0");
        if (pluginConfig->sectionHandling == ALWAYS)
        {
            setSectionNumber(parentKey, appendKey, newKS);

            if (atoi(keyString(keyGetMeta(appendKey, "ini/section"))) > atoi(oldSectionNumber))
            {
                setOrderNumber(parentKey, appendKey);
                keySetBinary(appendKey, 0, 0);
                ksAppendKey(newKS, keyDup(appendKey));
            }
            else
            {
                insertSectionIntoExistingOrder(appendKey, newKS);
            }
            keySetBinary(appendKey, 0, 0);
            if (!ksLookup(newKS, appendKey, KDB_O_NONE))
            {
                ksAppendKey(newKS, keyDup(appendKey));
            }
            keySetMeta(appendKey, "order", 0);
            keySetMeta(appendKey, "ini/section", 0);
            keySetMeta(appendKey, "binary", 0);
        }
        appendKey = createUnescapedKey(appendKey, keyBaseName(cur));
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
    for (ssize_t i = 0; i < arraySize; ++i)
    {
        cur = keyArray[i];
        if (!strcmp(keyName(parentKey), keyName(cur)))
            continue;
        if (isSectionKey(cur))
        {
            sectionKey = cur;
        }
        writeComments(cur, fh);
        if (config->sectionHandling == NONE)
        {
            char *iniName = getIniName(parentKey, cur);
            if (isIniKey(cur))
            {
                fprintf(fh, "%s = %s\n", iniName, keyString(cur));
            }
            free(iniName);
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
                if (keyGetMeta(cur, "ini/array") && config->array)
                {
                    int lastArrayIndex = atoi(keyString(keyGetMeta(cur, "ini/array"))+1);
                    char *name = strdup(keyBaseName(cur));
                    ++i;
                    for (int j = i; j <= i+lastArrayIndex; ++j)
                    {
                        cur = keyArray[j];
                        fprintf(fh, "%s = %s\n", name, keyString(cur));
                    }
                    free(name);
                    i += lastArrayIndex;
                }
                else
                {
                    char *iniName;
                    if (keyIsBelow(sectionKey, cur))
                    {
                        iniName = getIniName(sectionKey, cur);
                    }
                    else
                    {
                        iniName = getIniName(parentKey, cur);
                    }

                    if (keyGetMeta(cur, "ini/empty"))
                    {
                        fprintf(fh, "%s\n", iniName);
                    }
                    else if (strstr(keyString(cur), "\n") == 0)
                    {
                        fprintf(fh, "%s = %s\n", iniName, keyString(cur));
                    }
                    else
                    {
                        if (config->supportMultiline)
                        {
                            writeMultilineKey(cur, iniName, fh);
                        }
                        else
                        {
                            ELEKTRA_SET_ERROR(97, parentKey,
                                    "Encountered a multiline value but multiline support is not enabled. "
                                    "Have a look at kdb info ini for more details");
                            ret = -1;
                        }
                    }
                    free(iniName);
                }
            }
        }
    }
    elektraFree(keyArray);
    return ret;
}
static void stripInternalData(Key *parentKey, KeySet *ks)
{
    ksRewind(ks);
    Key *cur;
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
            keySetName(newKey, newName);
            if (strcmp(keyName(parentKey), keyName(newKey)) && (!ksLookup(ks, newKey, KDB_O_NONE)))
            {
                ksAppendKey(ks, newKey);
                keyDel(ksLookup(ks, cur, KDB_O_POP));
            }
            else if (!strcmp(keyName(parentKey), keyName(newKey)))
            {
                keyDel(newKey);
                keyDel(ksLookup(ks, cur, KDB_O_POP));
            }
            else if (ksLookup(ks, newKey, KDB_O_NONE))
            {
                keyDel(newKey);
            }
            elektraFree(oldName);
            elektraFree(newName);
        }
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
    Key *cur;
    KeySet *newKS = ksNew(0, KS_END);
    keySetMeta(parentKey, "order", "#0");
    while ((cur = ksNext(returned)) != NULL)
    {
        if (keyGetMeta(cur, "order"))
        {
            if (atoi(keyString(keyGetMeta(parentKey, "order"))) < atoi(keyString(keyGetMeta(cur, "order"))))
                keySetMeta(parentKey, "order", keyString(keyGetMeta(cur, "order")));
            ksAppendKey(newKS, cur);
            keyDel(ksLookup(returned, cur, KDB_O_POP));
        }

    }
    //ksAppendKey(newKS, parentKey);
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
    setParents(returned, parentKey);
    ksDel(newKS);
    stripInternalData(parentKey, returned);
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

