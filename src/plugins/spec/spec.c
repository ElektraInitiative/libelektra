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

static char *keyNameToMatchingString(const Key *key)
{
    fprintf(stdout, "keyname: %s\n", keyName(key));
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
    fprintf(stdout, "keyToPattern: %s\n", pattern);
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
                fprintf(stderr, "%s not a valid array name\n", keyName(copy));
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

static void setArrayConflicts(Key *parent, long validCount, int hasInvalidKeys, Key *specKey)
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
            fprintf(stderr, "%ld not within range %ld - %ld\n", validCount, min, max);
            keySetMeta(parent, "conflict/range", ""); 
        }
        else
        {
            fprintf(stderr, "%ld within range %ld - %ld\n", validCount, min, max);
        }
    }
    if(hasInvalidKeys)
        keySetMeta(parent, "conflict/hasInvalidMembers", "");
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
    short hasInvalidMembers = 0;
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
                fprintf(stderr, "%s: marked as valid\n", keyName(cur));
            }
            else
            {
                KeySet *invalidCutKS = ksCut(subKeys, cur);
                Key *toMark;
                while((toMark = ksNext(invalidCutKS)) != NULL)
                { 
                    fprintf(stderr, "%s: marked as invalid\n", keyName(cur));
                    keySetMeta(toMark, "conflict/invalid", "");
                }
                ksDel(invalidCutKS);
                hasInvalidMembers = 1;
            }
        }
    }
    ksDel(subKeys);
    ksDel(ksCopy);
    setArrayConflicts(arrayParent, validCount, hasInvalidMembers, specKey); 
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
        fprintf(stderr, "%ld != %ld\n", subCount, required);
        keySetMeta(parent, "conflict/invalidSubCount", "");
    }
    else
    {
        fprintf(stderr, "%ld != %ld\n", subCount, required);
    }

    ksDel(subKeys);
    ksDel(ksCopy);
}

static void copyMeta(Key *key, Key *specKey)
{
    keyRewindMeta(specKey);
    while(keyNextMeta(specKey) != NULL)
    {
        const Key *meta = keyCurrentMeta(specKey);
        const char *name = keyName(meta);
        if(!(!strcmp(name, "array") || !strcmp(name, "required") || !strcmp(name, "specInternal/valid") || !strncmp(name, "conflict/", 9)))
        {
            keyCopyMeta(key, specKey, name);
        }
    }
    keySetMeta(key, "specInternal/valid", 0);
}

static void doGlobbing(KeySet *returned)
{
    Key *specCutKey = keyNew("spec", KEY_END);
    KeySet *specKS = ksCut(returned, specCutKey);
    keyDel(specCutKey);
    Key *specKey;
    ksRewind(specKS);
    Key *cur;
    while((specKey = ksNext(specKS)) != NULL)
    {
        char *pattern = keyNameToMatchingString(specKey);
        ksRewind(returned);
        while((cur = ksNext(returned)) != NULL)
        {
            cursor_t cursor = ksGetCursor(returned);
            if(matchPatternToKey(pattern, cur))
            {

                fprintf(stderr, "%s matched %s\n", keyName(cur), pattern);
                if(keyGetMeta(cur, "conflict/invalid"))
                {
                    fprintf(stderr, "%s marked as invalid. Pattern: %s\n", keyName(cur), pattern);
                    continue;
                }
                else if(keyGetMeta(cur, "specInternal/valid"))
                {
                    fprintf(stderr, "%s marked as valid. Pattern :%s\n\n", keyName(cur), pattern);
                    copyMeta(cur, specKey);
                }
                else if(elektraArrayValidateName(cur) == 1)
                {
                    fprintf(stderr, "%s is valid array key. Pattern: %s\n", keyName(cur), pattern);
                    validateArray(returned, cur, specKey); 
                    copyMeta(cur, specKey);
                }
                else if(!(strcmp(keyBaseName(specKey), "_")))
                {
                    fprintf(stderr, "%s is a wildcard match. Pattern: %s\n", keyName(cur), pattern);
                    validateWildcardSubs(returned, cur, specKey);
                    copyMeta(cur, specKey);
                }
                else
                {
                    if(hasArray(cur))
                    {
                        if(isValidArrayKey(cur))
                        {
                            fprintf(stderr, "%s: contains valid array keys\n", keyName(cur));
                            copyMeta(cur, specKey);
                        }
                        else
                        {
                            fprintf(stderr, "%s: contains invalid array keys\n", keyName(cur));
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
        elektraFree(pattern);
    }
    ksAppend(returned, specKS);
    ksDel(specKS);
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
    doGlobbing(returned);
    return 1; // success
}

int elektraSpecSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
    doGlobbing(returned);
    return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT (spec)
{
    return elektraPluginExport ("spec",
            ELEKTRA_PLUGIN_GET,	&elektraSpecGet,
            ELEKTRA_PLUGIN_SET,	&elektraSpecSet,
            ELEKTRA_PLUGIN_END);
}

