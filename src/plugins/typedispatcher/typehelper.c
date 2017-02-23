#include "typehelper.h"
#include <kdbplugin.h>
#include <kdbmodule.h>    	//elektraModulesInit, elektraModulesClose
#include <kdbprivate.h>   	//elektraPluginClose. elektraPluginOpen
#include <kdbproposal.h>  	//keyRel2, elektraKeyGetMetaKeySet
#include <kdbhelper.h> 		//elektraCalloc
#include <kdbease.h> 		//KeyGetRelativeName
#include <strings.h>
#include <stdio.h>

static void freeType(TypeConfig *tc)
{
    if(tc)
	ksDel(tc->checks);
    elektraFree(tc);
}

void freeTypes(KeySet *types)
{
    if(!types)
	return;
    Key *key;
    ksRewind(types);
    while((key = ksNext(types)) != NULL)
    {
	TypeConfig *tc = *(TypeConfig **)keyValue(key);
	ksDel(tc->types);
	freeType(tc);
    }
}

void closePlugins(KeySet *plugins)
{
    if(!plugins)
	return;
    Key *key;
    while((key = ksNext(plugins)) != NULL)
    {
	Plugin *plugin = *(Plugin **)keyValue(key);
	if(plugin)
	    elektraPluginClose(plugin, NULL);
    }
}

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

DispatchConfig * initDispatchConfig()
{
    DispatchConfig *config;
    config = elektraCalloc(sizeof(DispatchConfig));
    if(!config)
	return NULL;
    config->plugins = ksNew(0, KS_END);
    config->modules = ksNew(0, KS_END);
    config->types = ksNew(0, KS_END);
    elektraModulesInit(config->modules, NULL);
    return config;
}

KeySet *getKeysBelow(const Key *key, KeySet *ks, KeyRelType rel)
{
    if(!key)
	return NULL;
    if(!ks)
	return NULL;
    ksRewind(ks);
    KeySet *result = ksNew(ksGetSize(ks), KS_END);
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

KeySet *getAllKeysBelow(const Key *key, KeySet *ks)
{
    return getKeysBelow(key, ks, ELEKTRA_REL_BELOW_SAME_NS);
}

KeySet *getKeysDirectBelow(const Key *key, KeySet *ks)
{
    return getKeysBelow(key, ks, ELEKTRA_REL_DIRECT_BELOW_SAME_NS);
}

static TypeConfig *newTypeConfig()
{
    TypeConfig *tc = NULL;
    tc = elektraCalloc(sizeof(TypeConfig));
    if(!tc)
	return NULL;
    tc->type = SUBTYPE;
    tc->checks = ksNew(0, KS_END);
    tc->types = ksNew(0, KS_END);
    return tc;
}

static void setTypeType(TypeConfig *tc, const Key *key)
{
    if(!strncasecmp(keyString(key), "SUM", 3))
	tc->type = SUMTYPE;
    else
	tc->type = SUBTYPE;
}

int readTypeNames(DispatchConfig *config, const Key *typeKey, Key *parentKey)
{
    if(!typeKey)
	return ERROR;

    const char *typeName = keyBaseName(typeKey);
#ifdef DEVBUILD
    fprintf(stderr, "\tdefines type %s\n", typeName);
#endif
    if(ksLookupByName(config->types, typeName, KDB_O_NONE))
    {
#ifdef DEVBUILD
	fprintf(stderr, "ERROR: %s already defined\n", typeName);
#endif
	return ERROR;
    }

    TypeConfig *tc = newTypeConfig();
    if(!tc)
	return ERROR;
    setTypeType(tc, typeKey);

    Key *newType = keyNew(typeName, KEY_META_NAME, KEY_BINARY, KEY_SIZE, sizeof(TypeConfig *), KEY_VALUE, &tc, KEY_END);
    ksAppendKey(config->types, newType);

    return SUCCESS;
}


int readTypeConfig(DispatchConfig *config, const Key *typeKey, KeySet *defKS, Key *parentKey)
{
    if(!typeKey)
	return ERROR;

    const char *typeName = keyBaseName(typeKey);
#ifdef DEVBUILD
    fprintf(stderr, "populating %s\n", typeName);
#endif

    Key *lookup = ksLookupByName(config->types, typeName, KDB_O_NONE);
    if(!lookup)
    {
#ifdef DEVBUILD
	fprintf(stderr, "datatype %s not found but should exist\n", typeName);
#endif 
	return ERROR;
    }
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
    else
    {
	fprintf(stderr, "%s found in config->types\n", keyName(lookup));
    }
#endif
    TypeConfig *tc = *(TypeConfig **)keyValue(lookup);

    Key *cur;	
    Key *checkKey = keyNew(keyName(typeKey), KEY_META_NAME, KEY_END);
    keyAddBaseName(checkKey, "check");
    KeySet *typeKS = getAllKeysBelow(checkKey, defKS);
    KeySet *dupKS = ksDup(typeKS);
    while((cur = ksNext(typeKS)) != NULL)
    {
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	fprintf(stderr, "\t%s:(%s)\n", keyName(cur), keyString(cur));
#endif
	if(keyRel2(checkKey, cur, ELEKTRA_REL_DIRECT_BELOW_SAME_NS) == 1)
	{
	    // cur has form define/type/<TYPENAME>/check/<PLUGIN>
	    Key *appendKey = keyNew(keyBaseName(cur), KEY_META_NAME, KEY_END);
	    const char *metaName = elektraKeyGetRelativeName(cur, typeKey);
	    keySetMeta(appendKey, metaName, keyString(cur));    
#ifdef DEVBUILD
	    fprintf(stderr,"\t\tadding Metadata %s:(%s) to key %s\n", metaName, keyString(cur), keyName(appendKey));
#endif
	    if(!strncmp(keyString(cur), "#", 1))
	    {
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf(stderr, "\t\t\t is array\n");
#endif
		//array / multiple definitions
		KeySet *c = getAllKeysBelow(cur, dupKS);

		Key *cur2;
		while((cur2 = ksNext(c)) != NULL)
		{
		    metaName = elektraKeyGetRelativeName(cur2, typeKey);
		    keySetMeta(appendKey, metaName, keyString(cur2));
#ifdef DEVBUILD
	    fprintf(stderr, "\t\t\tadding %s:(%s) to %s\n", metaName, keyString(cur2), keyName(appendKey));
#endif	    
		}
		ksDel(c);
	    }
	    ksAppendKey(tc->checks, appendKey);
	}
    }
    ksDel(dupKS);
    keyDel(checkKey);
    ksDel(typeKS);
    
    Key *relKey = keyNew(keyName(typeKey), KEY_META_NAME, KEY_END);
    keyAddBaseName(relKey, "type");
    typeKS = getAllKeysBelow(relKey, defKS);
    while((cur = ksNext(typeKS)) != NULL)
    {
	if(!strncmp(keyString(cur), "#", 1))
	    continue;
	const char *relTypeName = keyString(cur);
	lookup = ksLookupByName(config->types, relTypeName, KDB_O_NONE);
	if(!lookup)
	{
#ifdef DEVBUILD
	    fprintf(stderr, "%s references %s, but doesn't exist\n", keyName(typeKey), relTypeName);
#endif
	    keyDel(relKey);
	    ksDel(typeKS);
	    return ERROR;
	}
#ifdef DEVBUILD
	fprintf(stderr, "\t\t\t%s references %s - adding\n", keyName(cur), keyName(lookup));
#endif
	ksAppendKey(tc->types, lookup);
    }
    keyDel(relKey);
    ksDel(typeKS);

    return SUCCESS;
}

