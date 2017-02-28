#include "typehelper.h"
#include <kdbproposal.h>  	//elektraKeyGetMetaKeySet
#include <stdio.h>


// helper
// copy check metadata  
static void copyCheckMeta(Key *key, Key *checkMeta)
{
#ifdef DEVBUILD
    fprintf(stderr, "\t%s-validation\n", keyName(checkMeta));
#endif
    keyRewindMeta(checkMeta);
    const Key *metaKey;
    while((metaKey = keyNextMeta(checkMeta)) != NULL)
    {
	keySetMeta(key, keyName(metaKey), keyString(metaKey));
#ifdef DEVBUILD
	fprintf(stderr, "\t\t%s:(%s)\n", keyName(metaKey), keyString(metaKey));
#endif
    }
}

// validate a key
// checkMeta holds the metadata for a checker plugin
static int checkKey(DispatchConfig *config, const Key *key, Key *checkMeta)
{
    Key *testKey = keyNew(keyName(key), KEY_VALUE, keyString(key), KEY_END);
    copyCheckMeta(testKey, checkMeta);
    ValidateFunction validate = getValidateFunction(config, keyName(checkMeta));
    if(!validate)
    {
	keyDel(testKey);
	return ERROR;
    }
    Key *errorKey = keyNew(0, KEY_END);
    RC rc = validate(testKey, errorKey);
#ifdef DEVBUILD
    fprintf(stderr, "%s->validateKey(%s, errorKey) returned %d\n", keyName(checkMeta), keyName(key), rc);
#endif
    if(rc == 0)
    {
#ifdef DEVBUILD
	fprintf(stderr,"number: %s\n", keyString (keyGetMeta (errorKey, "error/number")));
	fprintf(stderr,"description: : %s\n", keyString (keyGetMeta (errorKey, "error/description")));
	fprintf(stderr,"ingroup: : %s\n", keyString (keyGetMeta (errorKey, "error/ingroup")));
	fprintf(stderr,"module: : %s\n", keyString (keyGetMeta (errorKey, "error/module")));
	fprintf(stderr,"at: %s:%s\n", keyString (keyGetMeta (errorKey, "error/file")), keyString (keyGetMeta (errorKey, "error/line")));
	fprintf(stderr,"reason: : %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
	fprintf(stderr,"mountpoint: : %s\n", keyString (keyGetMeta (errorKey, "error/mountpoint")));
	fprintf(stderr,"configfile: : %s\n", keyString (keyGetMeta (errorKey, "error/configfile")));
#endif
	rc = ERROR;
    }
    keyDel(errorKey);
    keyDel(testKey);
    return rc;
}

// actual typechecker 
// first iterate over all checks for the current type and if needed
// recursively call itself for every supertype (DFS)
static int doTypeCheck(DispatchConfig *config, TypeConfig *tc, const Key *key)
{
    ksRewind(tc->checks);
    Key *checkMeta;
    TypeType t = getTypeType(tc);
    RC rc = ERROR;
    if(t == SUMTYPE)
       rc = ERROR;
    else if(t == SUBTYPE)
	rc = SUCCESS;
    else if(t == SKEL)
	return SUCCESS;

    while((checkMeta = ksNext(tc->checks)) != NULL)
    {
	RC r = checkKey(config, key, checkMeta);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	fprintf(stderr, "\t\t\tcheckKey(config, %s:(%s), %s) returned %d\n", keyName(key), keyString(key), keyName(checkMeta), r);
#endif
	switch(t)
	{
	    case SUMTYPE:
		if(r == SUCCESS)
		{
		    rc = SUCCESS;
		    goto TYPECHECKDONE;
		}
		break;
	    case SUBTYPE:
		if(r == ERROR)
		{
		    rc = ERROR;
		    goto TYPECHECKDONE;
		}
		break;
	    case SKEL:
	    default:
		break;
	}

    }
    ksRewind(tc->types);
    while((checkMeta = ksNext(tc->types)) != NULL)
    {
	TypeConfig *t_tc = *(TypeConfig **)keyValue(checkMeta);
	RC r = doTypeCheck(config, t_tc, key);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	fprintf(stderr, "\t\t\tcheckKey(config, %s:(%s), %s) returned %d\n", keyName(key), keyString(key), keyName(checkMeta), r);
#endif
	switch(t)
	{
	    case SUMTYPE:
		if(r == SUCCESS)
		{
		    rc = SUCCESS;
		    goto TYPECHECKDONE;
		}
		break;
	    case SUBTYPE:
		if(r == ERROR)
		{
		    rc = ERROR;
		    goto TYPECHECKDONE;
		}
		break;
	    case SKEL:
	    default:
		break;
	}
    }
TYPECHECKDONE:
    return rc;
}


// helperfunction
// checks if type exists and calls actual typecheck function doTypCheck
// TODO: merge with doTypeCheck
static int typeCheck(DispatchConfig *config, const Key *key, const char *typeName, Key *parentKey)
{
#ifdef DEVBUILD
    fprintf(stderr, "\twith type %s\n", typeName); 
#endif


    TypeConfig *tc = getType(config, typeName); 
    if(!tc)
    {
#ifdef DEVBUILD
	fprintf(stderr, "%s has type unknown type meta %s\n", keyName(key), typeName);
#endif
	return ERROR;
    }
    RC rc = doTypeCheck(config, tc, key);

    return rc;
}


// checks type metadata and calls typeCheck function 
// assume subtype for multiple type entries
int validateKey(Key *key, DispatchConfig *config, Key *parentKey)
{
    const Key *typeMeta = keyGetMeta(key, "type");
    if(!typeMeta)
	return SUCCESS;
#ifdef DEVBUILD
    fprintf(stderr, "validating %s:(%s)\n", keyName(key), keyString(key));
#endif

    if(!strncmp(keyString(typeMeta), "#", 1))
    {
	//multiple type entrys
	KeySet *metaKS = elektraKeyGetMetaKeySet(key);
	KeySet *typeKS = getAllKeysBelow(typeMeta, metaKS);
	ksDel(metaKS);
	Key *cur = NULL;
	RC rc = SUCCESS;
	while((cur = ksNext(typeKS)) != NULL)
	{
	    rc = typeCheck(config, key, keyString(cur), parentKey);
	    if(rc == ERROR)
	    {
		ksDel(typeKS);
		return ERROR;
	    }
	}
	ksDel(typeKS);
    }
    else
    {
	const char *typeName = keyString(typeMeta);
#ifdef DEVBUILD
	fprintf(stderr, "%s has type meta %s\n", keyName(key), typeName);
#endif
	RC rc = typeCheck(config, key, typeName, parentKey);
	if(rc == ERROR)
	    return ERROR;
    }
    return SUCCESS;
}
