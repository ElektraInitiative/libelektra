#include "typehelper.h"
#include <kdbmodule.h>    	//elektraModulesInit, elektraModulesClose
#include <kdbprivate.h>   	//elektraPluginClose. elektraPluginOpen, elektraStrNDup
#include <kdbease.h>		//elektraArrayIncName
#include <stdio.h>
#include <string.h>
#include <strings.h>


// hekoer fir freeTyoes
// free a single type configuration
static void freeType(TypeConfig *tc)
{
    if(tc)
	ksDel(tc->checks);
    elektraFree(tc);
}

// helper for closeDispatch
// free all type configurations
static void freeTypes(KeySet *types)
{
    if(!types)
	return;
    Key *key;
    ksRewind(types);
    while((key = ksNext(types)) != NULL)
    {
	TypeConfig *tc = *(TypeConfig **)keyValue(key);
	ksDel(tc->types);
	ksDel(tc->params);
	keyDel(tc->scope);
	freeType(tc);
    }
}

// helper for closeDispatch
// close all loaded plugins
static void closePlugins(KeySet *plugins)
{
    if(!plugins)
	return;
    Key *key;
    ksRewind(plugins);
    while((key = ksNext(plugins)) != NULL)
    {
	PluginConfig *pc = *(PluginConfig **)keyValue(key);
	if(pc->plugin)
	{
	    elektraPluginClose(pc->plugin, NULL);
	}
	elektraFree(pc);
    }
}


// close and free everything
void closeDispatchConfig(Plugin *handle)
{
    DispatchConfig *config = elektraPluginGetData(handle);
    if(!config)
    {
	return;
    }
    closePlugins(config->plugins);
    ksDel(config->plugins);
    elektraModulesClose(config->modules, NULL);
    ksDel(config->modules);
    freeTypes(config->types);
    ksDel(config->types);
    elektraFree(config);
    elektraPluginSetData(handle, NULL);
}


// initialize plugin configuration
DispatchConfig * initDispatchConfig()
{
    DispatchConfig *config = NULL;
    config = elektraCalloc(sizeof(DispatchConfig));
    if(!config)
	return NULL;
    config->plugins = ksNew(0, KS_END);
    config->modules = ksNew(0, KS_END);
    config->types = ksNew(0, KS_END);
    elektraModulesInit(config->modules, NULL);
    return config;
}

static KeySet *getKeysBelow(const Key *key, KeySet *ks, KeyRelType rel)
{
    if(!key)
	return NULL;
    if(!ks)
	return NULL;
    ksRewind(ks);
    KeySet *result = NULL;
    result = ksNew(ksGetSize(ks), KS_END);
    Key *cur;
    while((cur = ksNext(ks)) != NULL)
    {
	int r = keyRel2(key, cur, rel);
	if(r > 0)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	    fprintf(stderr, "\t Key %s is %d levels below %s\n", keyName(cur), r, keyName(key));
#endif
	    ksAppendKey(result, cur);
	}
    }
    ksRewind(result);
    return result;
}

// return a keyset containing all keys below <key>
KeySet *getAllKeysBelow(const Key *key, KeySet *ks)
{
    return getKeysBelow(key, ks, ELEKTRA_REL_BELOW_SAME_NS);
}

// return a keyset containing all keys directly below <key>
KeySet *getKeysDirectBelow(const Key *key, KeySet *ks)
{
    return getKeysBelow(key, ks, ELEKTRA_REL_DIRECT_BELOW_SAME_NS);
}

//initialize type config skeleton
TypeConfig *newTypeConfig()
{
    TypeConfig *tc = NULL;
    tc = elektraCalloc(sizeof(TypeConfig));
    if(!tc)
	return NULL;
    tc->type = SKEL;
    tc->scope = keyNew(0, KEY_END);
    tc->checks = ksNew(0, KS_END);
    tc->types = ksNew(0, KS_END);
    tc->params = ksNew(0, KS_END);
    return tc;
}

// sets TypeConfig->type to SUMTYPE if keyString matches "SUM"
// SUBTYPE otherwise
void setTypeType(TypeConfig *tc, const Key *key)
{
    if(!strncasecmp(keyString(key), "SUM", 3))
	tc->type = SUMTYPE;
    else
	tc->type = SUBTYPE;
}

TypeType getTypeType(TypeConfig *tc)
{
    return tc->type;
}

Key *getTypeKey(DispatchConfig *config, const char *type)
{
    Key *lookup = ksLookupByName(config->types, type, KDB_O_NONE);
    return lookup;
}

// look up <type> and return its configuration if found
TypeConfig *getType(DispatchConfig *config, const char *type)
{
    Key *lookup = getTypeKey(config, type);
    if(!lookup)
	return NULL;
    TypeConfig *tc = *(TypeConfig**)keyValue(lookup);
    return tc;
}


ArgumentConfig *parseTypeString(DispatchConfig *config ELEKTRA_UNUSED, const char *typeString)
{
    const char *ptr = typeString;
    while(*ptr)
    {
	if(*ptr == '(' || *ptr == ' ')
	    break;
	++ptr;
    }
    if(!*ptr)
	return NULL;
    ssize_t typeNameLen = (ptr - typeString) + 1;
    if(typeNameLen <= 0)
	return NULL;
    
    while(*ptr && (*ptr != '('))
	++ptr;
    if(!*ptr)
	return NULL;

    ArgumentConfig *argConfig = NULL;
    argConfig = elektraCalloc(sizeof(ArgumentConfig));
    argConfig->type = elektraCalloc(typeNameLen); 
    snprintf(argConfig->type, typeNameLen, "%s", typeString);
#ifdef DEVBUILD
    fprintf(stderr, "parsing typeString %s\n", typeString);
#endif
    argConfig->args = ksNew(0, KS_END);
    char *token = NULL;
    char *localCopy = elektraStrDup(ptr+1);
    char *rest = localCopy;
    Key *indexKey = keyNew("/#", KEY_META_NAME, KEY_END);
    while((token = strtok_r(rest, ",)", &rest)) != NULL)
    {
	elektraArrayIncName(indexKey);
	Key *key = keyNew(keyBaseName(indexKey), KEY_META_NAME, KEY_VALUE, token, KEY_END);	
#ifdef DEVBUILD
	fprintf(stderr, "\tadding %s:(%s)\n", keyName(key), keyString(key));
#endif
	ksAppendKey(argConfig->args, key);
    }
    keyDel(indexKey);
    elektraFree(localCopy);
    fprintf(stderr, "argConfig: %p\n", argConfig);
    return argConfig;
} 

KeySet *makeParamKS(KeySet *params, ArgumentConfig *argConfig)
{
    KeySet *args = argConfig->args;
    if(ksGetSize(params) <= 0 || ksGetSize(args) <= 0)
    {
#ifdef DEVBUILD
	fprintf(stderr, "ksSize(params): %zd, ksSize(args): %zd\n", ksGetSize(params), ksGetSize(args));
#endif
	return NULL;
    }
    ksRewind(params);
    ksRewind(args);
    KeySet *result = NULL;
    result = ksNew(ksGetSize(params), KS_END);
    Key *paramKey = NULL;
    while((paramKey = ksNext(params)) != NULL)
    {
	Key *argKey = ksLookup(args, paramKey, KDB_O_NONE);
	if(!argKey)
	{
	    ksDel(result);
	    return NULL;
	}
	else
	{
	    Key *appendKey = keyNew(keyString(paramKey), KEY_META_NAME, KEY_VALUE, keyString(argKey), KEY_END);
#ifdef DEVBUILD
	    fprintf(stderr, "mapping argument %s:(%s) to parameter %s:(%s)\n", keyName(argKey), keyString(argKey), keyName(paramKey), keyString(paramKey));
#endif
	    ksAppendKey(result, appendKey);
	}
    }
    return result;
}

void freeArgumentConfig(ArgumentConfig *config)
{
    if(!config)
	return;
    elektraFree(config->type);
    ksDel(config->args);
    elektraFree(config);
}
