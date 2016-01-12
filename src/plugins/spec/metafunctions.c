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
