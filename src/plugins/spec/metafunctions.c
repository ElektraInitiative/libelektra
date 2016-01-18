#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <fnmatch.h>
#include <kdbhelper.h>
#include <kdbease.h>
#include <kdbprivate.h>

#include "metafunctions.h"

int elektraMetaArrayAdd(Key *key, const char *metaName, const char *value)
{
    const Key *meta = keyGetMeta(key, metaName);
    Key *arrayKey;
    if(!meta)
    {
        keySetMeta(key, metaName, "#0");
        arrayKey = keyDup(keyGetMeta(key, metaName));
        keySetString(arrayKey, 0);
        keyAddBaseName(arrayKey, "#");
    }
    else
    {
        arrayKey = keyDup(meta);
        keyAddBaseName(arrayKey, keyString(meta));
    }
    elektraArrayIncName(arrayKey);
    keySetMeta(key, keyName(arrayKey), value);
    keySetMeta(key, metaName, keyBaseName(arrayKey));
    keyDel(arrayKey);
}

char * elektraMetaArrayToString(Key *key, const char *metaName, const char *delim)
{
        char *result = NULL;
        Key *lookupElem = keyDup(keyGetMeta(key, metaName));
        keyAddBaseName(lookupElem, "#0");
        Key *elem = keyGetMeta(key, keyName(lookupElem));
        if(elem != NULL)
        {
            elektraRealloc((void **)&result, keyGetValueSize(elem));
            snprintf(result, keyGetValueSize(elem), "%s", keyString(elem));
        }
        elektraArrayIncName(lookupElem);
        elem = keyGetMeta(key, keyName(lookupElem));
        while(elem != NULL)
        {
            elektraRealloc((void **)&result, elektraStrLen(result)+keyGetValueSize(elem)+1); //String (incl. +2 times \0) + delimiter + whitespace
            strcat(result, delim);
            strcat(result, keyString(elem));
            elektraArrayIncName(lookupElem);
            elem = keyGetMeta(key, keyName(lookupElem));
        }
        keyDel(lookupElem);
        return result;
}
