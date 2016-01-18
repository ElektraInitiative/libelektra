/**
 * @file
 *
 * @brief Source for spec plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "spec.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fnmatch.h>
#include <kdbhelper.h>
#include <kdbease.h>
#include <kdbprivate.h>

#include "metafunctions.h"

typedef enum{GET, SET}Direction;

typedef enum{ERROR, WARNING, LOG, IGNORE}OnConflict;

typedef enum{ARRAYMEMBER, INVALID, SUBCOUNT, CONFLICT, OUTOFRANGE, NAC}Conflict;

typedef struct
{
    OnConflict member;
    OnConflict invalid;
    OnConflict count;
    OnConflict conflict;
    OnConflict range; 
}ConflictHandling;

static char *keyNameToMatchingString(const Key *key)
{
    uint8_t arrayCount = 0;
    char *name = strchr(keyName(key), '/');
    for(char *ptr = name; *ptr != '\0'; ++ptr)
        if(*ptr == '#')
            ++arrayCount;
    char *pattern = elektraMalloc(elektraStrLen(name)+arrayCount);
    char *dst = pattern;
    for(char *src = (name+1); *src != '\0'; ++src)
    {
        if(*src == '_' && *(src-1) == '/' && (*(src+1) == '/' || *(src+1) == '\0'))
        {
            *dst++ = '*';
        }
        else if(*src == '#' && *(src-1) == '/' && (*(src+1) == '/' || *(src+1) == '\0'))
        {
            *dst++ = '#';
            *dst++ = '*';
        }
        else
        {
            *dst++ = *src;
        }
    }
    *dst = '\0';
    return pattern;
}

static int matchPatternToKey(const char *pattern, const Key *key)
{
    return !fnmatch(pattern, (strchr(keyName(key), '/')+1), FNM_NOESCAPE|FNM_PATHNAME);
}

static int isValidArrayKey(Key *key)
{
    Key *copy = keyDup(key);
    do
    {
        if(keyBaseName(copy)[0] == '#')
        {
            if(elektraArrayValidateName(copy) == -1)
            {
                keyDel(copy);
                return 0;
            }
        }
    }while(keySetBaseName(copy, 0) != -1);
    keyDel(copy);
    return 1;
}

static int hasArray(Key *key)
{
    if(!strstr(keyName(key), "/#"))
        return 0;
    else 
        return 1;
}

static OnConflict getConfOption(Key *key)
{
    const char *string = keyString(key);
    if(!strcmp(string, "ERROR"))
    {
        return ERROR;
    }
    else if(!strcmp(string, "WARNING"))
    {
        return WARNING;
    }
    else if(!strcmp(string, "LOG"))
    {
        return LOG;
    }
    else
    {
        return IGNORE;
    }
}


static void validateArrayRange(Key *parent, long validCount, Key *specKey)
{
    const Key *arrayRange = keyGetMeta(specKey, "array");
    if(arrayRange != NULL)
    {
        char *rangeString = elektraMalloc(keyGetValueSize(arrayRange));
        keyGetString(arrayRange, rangeString, keyGetValueSize(arrayRange));
        char *delimPtr = strchr(rangeString, '-');
        char *maxString = delimPtr+1;
        *delimPtr = '\0';
        char *minString = rangeString;
        long min = atoi(minString);
        long max = atoi(maxString);
        if(validCount < min || validCount > max)
        {
            keySetMeta(parent, "conflict/range", ""); 
        }
        elektraFree(rangeString);
    }
}

static void validateArray(KeySet *ks, Key *arrayKey, Key *specKey)
{
    Key *tmpArrayParent = keyDup(arrayKey);
    keySetBaseName(tmpArrayParent, 0);
    Key *arrayParent = ksLookup(ks, tmpArrayParent, KDB_O_NONE);
    keyDel(tmpArrayParent);
    if(arrayParent == NULL)
        return;
    KeySet *ksCopy = ksDup(ks);
    KeySet *subKeys = ksCut(ksCopy, arrayParent);
    Key *cur;
    long validCount = 0;
    while((cur = ksNext(subKeys)) != NULL)
    {
        if(!keyIsDirectBelow(arrayParent, cur))
            continue;
        if(keyBaseName(cur)[0] == '#')
        {
            if(elektraArrayValidateName(cur) == 1)
            { 
                ++validCount;
                keySetMeta(cur, "specInternal/valid", "");
            }
            else
            {
                KeySet *invalidCutKS = ksCut(subKeys, cur);
                Key *toMark;
                while((toMark = ksNext(invalidCutKS)) != NULL)
                { 
                    keySetMeta(toMark, "conflict/invalid", "");
                    elektraMetaArrayAdd(arrayParent, "conflict/hasInvalidMembers", keyName(toMark));
                }
                ksDel(invalidCutKS);
            }
        }
    }
    ksDel(subKeys);
    ksDel(ksCopy);
    validateArrayRange(arrayParent, validCount, specKey); 
}
static void validateWildcardSubs(KeySet *ks, Key *key, Key *specKey)
{
    const Key *requiredMeta = keyGetMeta(specKey, "required");
    if(!requiredMeta)
        return;
    Key *tmpParent = keyDup(key);
    keySetBaseName(tmpParent, 0);
    Key *parent = ksLookup(ks, tmpParent, KDB_O_NONE);
    keyDel(tmpParent);
    if(parent == NULL)
        return;
    KeySet *ksCopy = ksDup(ks);
    KeySet *subKeys = ksCut(ksCopy, parent);
    Key *cur;
    long subCount = 0;
    while((cur = ksNext(subKeys)) != NULL)
    {
        if(keyIsDirectBelow(parent, cur))
            ++subCount;
    }
    long required = atol(keyString(requiredMeta));
    if(required != subCount)
    {
        keySetMeta(parent, "conflict/invalidSubCount", "");
    }

    ksDel(subKeys);
    ksDel(ksCopy);
}


static void parseLocalConfig(Key *specKey, ConflictHandling *localCh, Direction dir) 
{
    Key *localConflictMeta = NULL;
    switch(dir)
    {
        case GET:
            if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/get/member")) != NULL)
            {
                localCh->member = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/get/invalid")) != NULL)
            {
                localCh->invalid = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/get/count")) != NULL)
            {
                localCh->count = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/get/conflict")) != NULL)
            {
                localCh->conflict = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/get/range")) != NULL)
            {
                localCh->range = getConfOption(localConflictMeta);
            }
            break;
        case SET:
            if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/set/member")) != NULL)
            {
                localCh->member = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/set/invalid")) != NULL)
            {
                localCh->invalid = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/set/count")) != NULL)
            {
                localCh->count = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/set/conflict")) != NULL)
            {
                localCh->conflict = getConfOption(localConflictMeta);
            }
            else if((localConflictMeta = (Key *)keyGetMeta(specKey, "conflict/set/range")) != NULL)
            {
                localCh->range = getConfOption(localConflictMeta);
            }
            break;
    }
}

static Conflict getConflict(Key *conflictMeta)
{
    if(!strcmp(keyName(conflictMeta), "conflict/invalid"))
        return INVALID;
    else if(!strcmp(keyName(conflictMeta), "conflict"))
        return CONFLICT;
    else if(!strcmp(keyName(conflictMeta), "conflict/hasInvalidMembers"))
        return ARRAYMEMBER;
    else if(!strcmp(keyName(conflictMeta), "conflict/range"))
        return OUTOFRANGE;
    else if(!strcmp(keyName(conflictMeta), "conflict/invalidSubCount"))
        return SUBCOUNT;
    else
        return NAC;
}

static int handleInvalidConflict(Key *parentKey, Key *key, Key *conflictMeta, OnConflict onConflict)
{
    switch(onConflict)
    {
        case ERROR:
            fprintf(stderr, "ERROR: INVALID: %s\n", keyName(key));
            break;
        case WARNING:
            fprintf(stderr, "WARNING: INVALID: %s\n", keyName(key));
            break;
        case LOG:
            fprintf(stderr, "LOG: INVALID: %s\n", keyName(key));                   
            break;
        case IGNORE:

            break;
    }
}

static int handleArrayConflict(Key *parentKey, Key *key, Key *conflictMeta, OnConflict onConflict)
{
    switch(onConflict)
    {
        case ERROR:
            fprintf(stderr, "ERROR: ARRAYMEMBER: %s\n", keyName(key));
            break;
        case WARNING:
            fprintf(stderr, "WARNING: ARRAYMEMBER: %s\n", keyName(key));
            break;
        case LOG:
            fprintf(stderr, "LOG: ARRAYMEMBER: %s\n", keyName(key));                   
            break;
        case IGNORE:

            break;
    }
}

static int handleSubCountConflict(Key *parentKey, Key *key, Key *conflictMeta, OnConflict onConflict)
{
    switch(onConflict)
    {
        case ERROR:
            fprintf(stderr, "ERROR: SUBCOUNT: %s\n", keyName(key));
            break;
        case WARNING:
            fprintf(stderr, "WARNING: SUBCOUNT: %s\n", keyName(key));
            break;
        case LOG:
            fprintf(stderr, "LOG: SUBCOUNT: %s\n", keyName(key));                   
            break;
        case IGNORE:

            break;
    }
}

static int handleConflictConflict(Key *parentKey, Key *key, Key *conflictMeta, OnConflict onConflict)
{
    switch(onConflict)
    {
        case ERROR:
            fprintf(stderr, "ERROR: CONFLICT: %s\n", keyName(key));
            break;
        case WARNING:
            fprintf(stderr, "WARNING: CONFLICT: %s\n", keyName(key));
            break;
        case LOG:
            fprintf(stderr, "LOG: CONFLICT: %s\n", keyName(key));                   
            break;
        case IGNORE:

            break;
    }
}

static int handleOutOfRangeConflict(Key *parentKey, Key *key, Key *conflictMeta, OnConflict onConflict)
{
    switch(onConflict)
    {
        case ERROR:
            fprintf(stderr, "ERROR: OUTOFRANGE: %s\n", keyName(key));
            break;
        case WARNING:
            fprintf(stderr, "WARNING: OUTOFRANGE: %s\n", keyName(key));
            break;
        case LOG:
            fprintf(stderr, "LOG: OUTOFRANGE: %s\n", keyName(key));                   
            break;
        case IGNORE:

            break;
    }
}

static int handleError(Key *parentKey, Key *key, Key *conflictMeta, Conflict conflict, ConflictHandling *ch)
{
    int ret = 0;
    switch(conflict)
    {
        case INVALID:
            ret = handleInvalidConflict(parentKey, key, conflictMeta, ch->invalid);
            break;
        case ARRAYMEMBER:
            ret = handleArrayConflict(parentKey, key, conflictMeta, ch->member);
            break;
        case SUBCOUNT:
            ret = handleSubCountConflict(parentKey, key, conflictMeta, ch->count);
            break;
        case CONFLICT:
            ret = handleConflictConflict(parentKey, key, conflictMeta, ch->conflict);
            break;
        case OUTOFRANGE:
            ret = handleOutOfRangeConflict(parentKey, key, conflictMeta, ch->range);
            break;
        default:
            break;
   }
    return ret;
}

static int handleErrors(Key *parentKey, KeySet *ks, Key *key, Key *specKey, ConflictHandling *ch, Direction dir)
{
    cursor_t cursor = ksGetCursor(ks); 
    int ret = 0;
    ConflictHandling *localCh = elektraMalloc(sizeof(ConflictHandling));
    memcpy(localCh, ch, sizeof(ConflictHandling));
    parseLocalConfig(specKey, localCh, dir);
    Key *parentLookup = keyDup(key);
    keySetBaseName(parentLookup, 0);
    Key *parent = ksLookup(ks, parentLookup, KDB_O_NONE);
    keyDel(parentLookup);
    keyRewindMeta(parent);
    Conflict conflict;
    Key *meta;
    while(keyNextMeta(parent) != NULL)
    {
        meta = keyCurrentMeta(parent);
        conflict = getConflict(meta);
        if(conflict != NAC)
        {
            fprintf(stderr, "PARENT: ");
            ret |= handleError(parentKey, parent, meta, conflict, localCh); 
            keySetMeta(parent, keyName(meta), 0);
        }
    }
    keyDel(parent);
    keyRewindMeta(key);
    while(keyNextMeta(key) != NULL)
    {
        meta = keyCurrentMeta(key);
        conflict = getConflict(meta);
        if(conflict != NAC)
        {
            ret |= handleError(parentKey, key, meta, conflict, localCh); 
            keySetMeta(key, keyName(meta), 0);
        }
       
    }
    elektraFree(localCh);
    ksSetCursor(ks, cursor);
    return ret;
}

static void copyMeta(Key *key, Key *specKey)
{
    keyRewindMeta(specKey);
    while(keyNextMeta(specKey) != NULL)
    {
        const Key *meta = keyCurrentMeta(specKey);
        const char *name = keyName(meta);
        if(!(!strcmp(name, "array") || !strcmp(name, "required") || !strncmp(name, "conflict/", 9)))
        {
            const Key *oldMeta;
            if((oldMeta = keyGetMeta(key, name)) != NULL)
            {
                int conflictStringSize=elektraStrLen(name)+elektraStrLen("conflict/");
                char *conflictName = elektraMalloc(conflictStringSize);
                snprintf(conflictName, conflictStringSize, "conflict/%s", name);
                keySetMeta(key, conflictName, keyString(oldMeta));
                keyCopyMeta(key, specKey, name);
                elektraFree(conflictName);
                elektraMetaArrayAdd(key, "conflict", name);
            }
            else
            {
                keyCopyMeta(key, specKey, name);
            }
        }
    }
    keySetMeta(key, "specInternal/valid", 0);
}

static int doGlobbing(Key *parentKey, KeySet *returned, ConflictHandling *ch, Direction dir)
{
    Key *specCutKey = keyNew("spec", KEY_END);
    KeySet *specKS = ksCut(returned, specCutKey);
    keyDel(specCutKey);
    Key *specKey;
    ksRewind(specKS);
    Key *cur;
    int ret = 1;
    while((specKey = ksNext(specKS)) != NULL)
    {
        char *pattern = keyNameToMatchingString(specKey);
        ksRewind(returned);
        while((cur = ksNext(returned)) != NULL)
        {
            cursor_t cursor = ksGetCursor(returned);
            if(matchPatternToKey(pattern, cur))
            {

                if(keyGetMeta(cur, "conflict/invalid"))
                {
                    copyMeta(cur, specKey);
                }
                else if(keyGetMeta(cur, "specInternal/valid"))
                {
                    copyMeta(cur, specKey);
                }
                else if(elektraArrayValidateName(cur) == 1)
                {
                    validateArray(returned, cur, specKey); 
                    copyMeta(cur, specKey);
                }
                else if(!(strcmp(keyBaseName(specKey), "_")))
                {
                    validateWildcardSubs(returned, cur, specKey);
                    copyMeta(cur, specKey);
                }
                else
                {
                    if(hasArray(cur))
                    {
                        if(isValidArrayKey(cur))
                        {
                            copyMeta(cur, specKey);
                        }
                    }
                    else
                    {
                        copyMeta(cur, specKey);
                    }
                }
            }
            ksSetCursor(returned, cursor);
        }
        ksRewind(returned);
        while((cur = ksNext(returned)) != NULL)
        {
            ret = handleErrors(parentKey, returned, cur, specKey, ch, dir);
            keySetMeta(cur, "conflict/invalid", 0);
        }

        elektraFree(pattern);
    }
    ksAppend(returned, specKS);
    ksDel(specKS);
    return ret;
}

static void parseConfig(KeySet *config, ConflictHandling *ch)
{
    Key *onConflictConf = NULL;
    while((onConflictConf = ksNext(config)) != NULL)
    {
        const char *baseName = keyBaseName(onConflictConf);
        if(!strcmp(baseName, "member"))
        {
            ch->member = getConfOption(onConflictConf);
        }
        else if(!strcmp(baseName, "invalid"))
        {
            ch->invalid = getConfOption(onConflictConf);
        } 
        else if(!strcmp(baseName, "count"))
        {
            ch->count = getConfOption(onConflictConf);
        } 
        else if(!strcmp(baseName, "conflict"))
        {
            ch->conflict = getConfOption(onConflictConf);
        } 
        else if(!strcmp(baseName, "range"))
        {
            ch->range = getConfOption(onConflictConf);
        } 
    }
}

int elektraSpecGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
    if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/spec"))
    {
        KeySet * contract = ksNew (30,
                keyNew ("system/elektra/modules/spec",
                    KEY_VALUE, "spec plugin waits for your orders", KEY_END),
                keyNew ("system/elektra/modules/spec/exports", KEY_END),
                keyNew ("system/elektra/modules/spec/exports/get",
                    KEY_FUNC, elektraSpecGet, KEY_END),
                keyNew ("system/elektra/modules/spec/exports/set",
                    KEY_FUNC, elektraSpecSet, KEY_END),
#include ELEKTRA_README (spec)
                keyNew ("system/elektra/modules/spec/infos/version",
                    KEY_VALUE, PLUGINVERSION, KEY_END),
                KS_END);
        ksAppend (returned, contract);
        ksDel (contract);

        return 1; // success
    }
    KeySet *config = elektraPluginGetConfig(handle);
    Key *onConflictConf = ksLookupByName(config, "/conflict/get", KDB_O_NONE);
    OnConflict onConflict = IGNORE;
    ConflictHandling *ch = elektraMalloc(sizeof(ConflictHandling));
    if(onConflictConf)
    {
        const char *onConflictString = keyString(onConflictConf);
        if(!strcmp(onConflictString, "ERROR"))
        {
            onConflict = ERROR;
        }
        else if(!strcmp(onConflictString, "WARNING"))
        {
            onConflict = WARNING;
        }
        else if(!strcmp(onConflictString, "LOG"))
        {
            onConflict = LOG;
        }
        else if(!strcmp(onConflictString, "IGNORE"))
        {
            onConflict = IGNORE;
        }
    }
    ch->member = onConflict;
    ch->invalid = onConflict;
    ch->count = onConflict;
    ch->conflict = onConflict;
    ch->range = onConflict;

    KeySet *conflictCut = ksCut(config, onConflictConf);
    parseConfig(conflictCut, ch);
    ksAppend(config, conflictCut);
    ksDel(conflictCut);
    int ret = doGlobbing(parentKey, returned, ch, GET);
    elektraFree(ch);
    return ret; // success
}

int elektraSpecSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{

    KeySet *config = elektraPluginGetConfig(handle);
    Key *onConflictConf = ksLookupByName(config, "/conflict/set", KDB_O_NONE);
    OnConflict onConflict = IGNORE;
    ConflictHandling *ch = elektraMalloc(sizeof(ConflictHandling));
    if(onConflictConf)
    {
        const char *onConflictString = keyName(onConflictConf);
        if(!strcmp(onConflictString, "ERROR"))
        {
            onConflict = ERROR;
        }
        else if(!strcmp(onConflictString, "WARNING"))
        {
            onConflict = WARNING;
        }
        else if(!strcmp(onConflictString, "LOG"))
        {
            onConflict = LOG;
        }
    }
    ch->member = onConflict;
    ch->invalid = onConflict;
    ch->count = onConflict;
    ch->conflict = onConflict;
    ch->range = onConflict;

    KeySet *conflictCut = ksCut(config, onConflictConf);
    parseConfig(conflictCut, ch);
    ksAppend(config, conflictCut);
    ksDel(conflictCut);
    int ret = doGlobbing(parentKey, returned, ch, SET);
    elektraFree(ch);
    return ret; // success
}


Plugin * ELEKTRA_PLUGIN_EXPORT (spec)
{
    return elektraPluginExport ("spec",
            ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
            ELEKTRA_PLUGIN_SET,	&elektraSpecSet,
            ELEKTRA_PLUGIN_END);
}

