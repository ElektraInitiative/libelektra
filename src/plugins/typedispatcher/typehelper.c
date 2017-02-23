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
	keyDel(tc->scope);
	freeType(tc);
    }
}

static void closePlugins(KeySet *plugins)
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

static KeySet *getKeysBelow(const Key *key, KeySet *ks, KeyRelType rel)
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

static KeySet *getAllKeysBelow(const Key *key, KeySet *ks)
{
    return getKeysBelow(key, ks, ELEKTRA_REL_BELOW_SAME_NS);
}

static KeySet *getKeysDirectBelow(const Key *key, KeySet *ks)
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
    tc->scope = keyNew(0, KEY_END);
    tc->checks = ksNew(0, KS_END);
    tc->types = ksNew(0, KS_END);
    return tc;
}


// sets TypeConfig->type to SUMTYPE if keyString matches "SUM"
// SUBTYPE otherwise
static void setTypeType(TypeConfig *tc, const Key *key)
{
    if(!strncasecmp(keyString(key), "SUM", 3))
	tc->type = SUMTYPE;
    else
	tc->type = SUBTYPE;
}

static Key *getTypeKey(DispatchConfig *config, const char *type)
{
    Key *lookup = ksLookupByName(config->types, type, KDB_O_NONE);
    return lookup;
}

static TypeConfig *getType(DispatchConfig *config, const char *type)
{
    Key *lookup = getTypeKey(config, type);
    if(!lookup)
	return NULL;
    TypeConfig *tc = *(TypeConfig**)keyValue(lookup);
    return tc;
}

// creates skeletons(name+type+scope) for new types and fails if type already exist
static int readTypeNames(DispatchConfig *config, const Key *scope, const Key *typeKey, Key *parentKey)
{
    if(!typeKey)
	return ERROR;

    const char *typeName = keyBaseName(typeKey);
#ifdef DEVBUILD
    fprintf(stderr, "\tdefines type %s\n", typeName);
#endif
    TypeConfig *tc = getType(config, typeName);
    if(tc && keyCmp(tc->scope, scope))
    {
#ifdef DEVBUILD
	fprintf(stderr, "ERROR: %s already defined\n", typeName);
#endif
	return ERROR;
    }
    else if(tc)
    {
	return SUCCESS;
    }

    tc = newTypeConfig();
    if(!tc)
	return ERROR;
    setTypeType(tc, typeKey);
    keySetName(tc->scope, keyName(scope)); 

    Key *newType = keyNew(typeName, KEY_META_NAME, KEY_BINARY, KEY_SIZE, sizeof(TypeConfig *), KEY_VALUE, &tc, KEY_END);
    ksAppendKey(config->types, newType);

    return SUCCESS;
}


static int readTypeConfig(DispatchConfig *config, const Key *key, const Key *typeKey, KeySet *defKS, Key *parentKey)
{
    if(!typeKey)
	return ERROR;

    const char *typeName = keyBaseName(typeKey);
#ifdef DEVBUILD
    fprintf(stderr, "populating %s\n", typeName);
#endif

    TypeConfig *tc = getType(config, typeName);
    if(!tc)
    {
#ifdef DEVBUILD
	fprintf(stderr, "datatype %s not found but should exist\n", typeName);
#endif
	return ERROR;
    }
    else
    {
#ifdef DEVBUILD
	fprintf(stderr, "datatype %s found in config->types\n", typeName);
#endif
	if((ksGetSize(tc->checks) > 0) || (ksGetSize(tc->types) > 0))
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	    fprintf(stderr, "%s already exists - changing types at runtime not supported yet\n", typeName);
#endif
	    return SUCCESS;
	}
    }
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
    Key *lookup = ksLookup(defKS, relKey, KDB_O_NONE);
    if(!strncmp(keyString(lookup), "#", 1))
    {
	typeKS = getAllKeysBelow(relKey, defKS);
    }
    else
    {
	typeKS = ksNew(0, KS_END);
	ksAppendKey(typeKS, lookup);
    }
    ksRewind(typeKS);
    while((cur = ksNext(typeKS)) != NULL)
    {
	const char *relTypeName = keyString(cur);
	TypeConfig *rt = getType(config, relTypeName);
	if(!rt)
	{
#ifdef DEVBUILD
	    fprintf(stderr, "%s references %s, but doesn't exist\n", keyName(key), relTypeName);
#endif
	    keyDel(relKey);
	    ksDel(typeKS);
	    return ERROR;
	}
	else
	{
	    if(keyCmp(rt->scope, key))
	    {
		if(keyRel2(rt->scope, key, ELEKTRA_REL_BELOW_SAME_NS) <= 0)
		{
#ifdef DEVBUILD
		    fprintf(stderr, "%s references %s, but not within scope\n", keyName(key), relTypeName);
#endif
		    keyDel(relKey);
		    ksDel(typeKS);
		    return ERROR;
		}
	    }
#ifdef DEVBUILD
	    fprintf(stderr, "\t\t\t%s references %s - adding\n", keyName(key), relTypeName);
#endif
	    ksAppendKey(tc->types, getTypeKey(config, relTypeName));
	}
    }
    keyDel(relKey);
    ksDel(typeKS);

    return SUCCESS;
}


int getTypeDefinitions(Key *key, DispatchConfig *config, Key *parentKey)
{
#ifdef DEVBUILD
    fprintf(stderr, "Getting type definitions from %s\n", keyName(key));
#endif
    RC rc = SUCCESS;
    KeySet *metaKS = elektraKeyGetMetaKeySet(key);
    Key *defineParent = keyNew("define/type", KEY_META_NAME, KEY_END);
    KeySet *defKS = getAllKeysBelow(defineParent, metaKS);

    if(!defKS)
    {
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	fprintf(stderr, "Key %s has no type definitions\n", keyName(key));
#endif
    }
    else
    {
	// typeParents - define/type/<TYPENAME>
	KeySet *typeParentKS = getKeysDirectBelow(defineParent, defKS);
	if(!typeParentKS)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	    fprintf(stderr, "Key %s has no metakey define/type/<TYPENAME>\n", keyName(key));
#endif
	    ksDel(defKS);
	}	
	else
	{
	    Key *typeKey = NULL;
	    ksRewind(typeParentKS);
	    while((typeKey = ksNext(typeParentKS)) != NULL)
	    {
		// since metakeys are ordered alphabetical
		// create skeletons to handle possible dependencies
		rc = readTypeNames(config, key, typeKey, parentKey);
		if(rc == ERROR)
		    goto GETDEFCLEANUP;
	    }
	    ksRewind(typeParentKS);
	    while((typeKey = ksNext(typeParentKS)) != NULL)
	    {
		// create actual type config
		rc = readTypeConfig(config, key, typeKey, defKS, parentKey);
		if(rc == ERROR)
		    goto GETDEFCLEANUP;
	    }
	}
GETDEFCLEANUP:
	ksDel(typeParentKS);
	ksDel(defKS);
    }
    ksDel(metaKS);
    keyDel(defineParent);
    return rc;
}


int validateKey(Key *key, DispatchConfig *config, Key *parentKey)
{
    const Key *typeMeta = keyGetMeta(key, "type");
    if(!typeMeta)
	return SUCCESS;
    if(!strncmp(keyString(typeMeta), "#", 1))
    {
	//multiple type entrys

    }
    else
    {
	const char *type = keyString(typeMeta);
#ifdef DEVBUILD
	fprintf(stderr, "%s has type meta %s\n", keyName(key), type);
#endif
	Key *lookup = ksLookupByName(config->types, type, KDB_O_NONE);
	if(!lookup)
	{
#ifdef DEVBUILD
	    fprintf(stderr, "%s has type unknown type meta %s\n", keyName(key), type);
#endif
	    return ERROR;
	}

	TypeConfig *tc = *(TypeConfig **)keyValue(lookup);
    }
    return SUCCESS;
}
