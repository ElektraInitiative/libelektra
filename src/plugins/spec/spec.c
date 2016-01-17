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

typedef enum{ERROR, WARNING, LOG, IGNORE}OnConflict;

typedef struct
{
    OnConflict member;
    OnConflict invalid;
    OnConflict count;
    OnConflict conflict;
    OnConflict range; 
    int firstError;
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

static void conflictAction(Key *key, OnConflict conflict, const char *msg, ConflictHandling *ch)
{
    if(conflict == ERROR && !ch->firstError)
        return;
    switch(conflict)
    {
        case ERROR:
            ch->firstError = 0;
            fprintf(stderr, "\t ERROR: %s:%s\n", keyName(key), msg);
            break;
        case WARNING:
            fprintf(stderr, "\t WARNING: %s:%s\n", keyName(key), msg);
            break;
        case LOG:
            fprintf(stderr, "\t LOG: %s:%s\n", keyName(key), msg);
            break;
        case IGNORE:
            fprintf(stderr, "\t IGNORE: %s\n", keyName(key));
        default:
            break;
    }
}

static int isValidArrayKey(Key *key, ConflictHandling *ch)
{
    Key *copy = keyDup(key);
    do
    {
        if(keyBaseName(copy)[0] == '#')
        {
            if(elektraArrayValidateName(copy) == -1)
            {
                conflictAction(key, ch->invalid, "not a valid array name", ch);
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


static void validateArrayRange(Key *parent, long validCount, Key *specKey, ConflictHandling *ch)
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
            conflictAction(parent, ch->range, "out of range", ch);
        }
        elektraFree(rangeString);
    }
}

static void validateArray(KeySet *ks, Key *arrayKey, Key *specKey, ConflictHandling *ch)
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
                keySetMeta(cur, "spec/internal/valid", "");
            }
            else
            {
                
                conflictAction(arrayParent, ch->member, "has invalid members", ch);
                KeySet *invalidCutKS = ksCut(subKeys, cur);
                Key *toMark;
                while((toMark = ksNext(invalidCutKS)) != NULL)
                { 
                    conflictAction(toMark, ch->member, "invalid member", ch);
                }
                ksDel(invalidCutKS);
            }
        }
    }
    ksDel(subKeys);
    ksDel(ksCopy);
    validateArrayRange(arrayParent, validCount, specKey, ch); 
}
static int validateWildcardSubs(KeySet *ks, Key *key, Key *specKey, ConflictHandling *ch)
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
        conflictAction(parent, ch->count, "invalid number of subkeys", ch);
    }
    ksDel(subKeys);
    ksDel(ksCopy);
}

static int copyMeta(Key *key, Key *specKey, ConflictHandling *ch)
{
    int ret = 1;
    keyRewindMeta(specKey);
    while(keyNextMeta(specKey) != NULL)
    {
        const Key *meta = keyCurrentMeta(specKey);
        const char *name = keyName(meta);
        if(!(!strcmp(name, "array") || !strcmp(name, "required") || !strcmp(name, "spec/internal/valid") || !strncmp(name, "conflict/", 9)))
        {
            const Key *oldMeta;
            if(((oldMeta = keyGetMeta(key, name)) != NULL) && (ch->conflict != IGNORE))
            {
                int conflictStringSize=elektraStrLen(name)+elektraStrLen("conflict/");
                char *conflictName = elektraMalloc(conflictStringSize);
                snprintf(conflictName, conflictStringSize, "conflict/%s", name);
                keySetMeta(key, conflictName, keyString(oldMeta));
                keyCopyMeta(key, specKey, name);
                elektraFree(conflictName);
                elektraMetaArrayAdd(key, "conflict", name);
                conflictAction(key, ch->conflict, "conflicting meta names", ch);
                ret = -1;
            }
            else
            {
                keyCopyMeta(key, specKey, name);
            }
        }
    }
    keySetMeta(key, "spec/internal/valid", 0);
    return ret;
}

static int doGlobbing(KeySet *returned, ConflictHandling *ch)
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
                    conflictAction(cur, ch->invalid, "marked as invalid", ch);
                    continue;
                }
                else if(keyGetMeta(cur, "spec/internal/valid"))
                {
                    copyMeta(cur, specKey, ch);
                }
                else if(elektraArrayValidateName(cur) == 1)
                {
                    validateArray(returned, cur, specKey, ch); 
                    copyMeta(cur, specKey, ch);
                }
                else if(!(strcmp(keyBaseName(specKey), "_")))
                {
                    validateWildcardSubs(returned, cur, specKey, ch);
                    copyMeta(cur, specKey, ch);
                }
                else
                {
                    if(hasArray(cur))
                    {
                        if(isValidArrayKey(cur, ch))
                        {
                            copyMeta(cur, specKey, ch);
                        }
                        else
                        {
                            conflictAction(cur, ch->invalid, "contains invalid array keys", ch);
                        }
                    }
                    else
                    {
                        copyMeta(cur, specKey, ch);
                    }
                }
            }
            ksSetCursor(returned, cursor);
        }
        ksRewind(returned);
        elektraFree(pattern);
    }
    ksAppend(returned, specKS);
    ksDel(specKS);
    return ret;
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
    ch->firstError = 1;

    KeySet *conflictCut = ksCut(config, onConflictConf);
    parseConfig(conflictCut, ch);
    ksAppend(config, conflictCut);
    int ret = doGlobbing(returned, ch);
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
    ch->firstError = 1;

    KeySet *conflictCut = ksCut(config, onConflictConf);
    parseConfig(conflictCut, ch);
    ksAppend(config, conflictCut);

    int ret = doGlobbing(returned, ch);
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

