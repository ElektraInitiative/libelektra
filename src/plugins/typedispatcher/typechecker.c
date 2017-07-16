/**
 * @file
 *
 * @brief Source for typedispatcher plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#include "typehelper.h"
#include <kdberrors.h>
#include <kdbhelper.h>   // elektraStrLen
#include <kdbproposal.h> //elektraKeyGetMetaKeySet, keyRel2
#include <stdio.h>
#include <stdlib.h>


// helper
// copy check metadata
static void copyCheckMeta (Key * key, Key * checkMeta, KeySet * args)
{
#ifdef DEVBUILD
	fprintf (stderr, "\t%s-validation\n", keyName (checkMeta));
#endif
	keyRewindMeta (checkMeta);
	const Key * metaKey;
	while ((metaKey = keyNextMeta (checkMeta)) != NULL)
	{
		if (strncmp (keyName (metaKey), "internal/", 9))
		{
			char * parameter = replaceParametersWithArguments (metaKey, args);
#ifdef DEVBUILD
			fprintf (stderr, "\t\t%s:(%s)\n", keyName (metaKey), parameter);
#endif
			keySetMeta (key, keyName (metaKey), parameter);
			elektraFree (parameter);
		}
	}
}

static void copyError (const Key * src, Key * dst, const Key * key)
{
	const Key * errNoKey = keyGetMeta (src, "error/number");
	if (!errNoKey) return;
	const char * errNoStr = keyString (errNoKey);
	const char * description = keyString (keyGetMeta (src, "error/description"));
	const char * reason = keyString (keyGetMeta (src, "error/reason"));
	ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_TYPEDISP_VAL_FAILED, dst, "validation of %s failed with error %d:%s.\n\t%s", keyName (key),
			      atoi (errNoStr), description, reason);
}

// validate a key
// checkMeta holds the metadata for a checker plugin
static int checkKey (DispatchConfig * config, const Key * key, Key * parentKey, KeySet * args, Key * checkMeta)
{
	Key * testKey = keyNew (keyName (key), KEY_VALUE, keyString (key), KEY_END);
	copyCheckMeta (testKey, checkMeta, args);
	ValidateFunction validate = getValidateFunction (config, keyName (checkMeta));
	if (!validate)
	{
		keyDel (testKey);
		return ERROR;
	}
	Key * errorKey = keyNew (0, KEY_END);
	RC rc = validate (testKey, errorKey);
#ifdef DEVBUILD
	fprintf (stderr, "%s->validateKey(%s, errorKey) returned %d\n", keyName (checkMeta), keyName (key), rc);
#endif
	if (rc == 0)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "number: %s\n", keyString (keyGetMeta (errorKey, "error/number")));
		fprintf (stderr, "description: : %s\n", keyString (keyGetMeta (errorKey, "error/description")));
		fprintf (stderr, "ingroup: : %s\n", keyString (keyGetMeta (errorKey, "error/ingroup")));
		fprintf (stderr, "module: : %s\n", keyString (keyGetMeta (errorKey, "error/module")));
		fprintf (stderr, "at: %s:%s\n", keyString (keyGetMeta (errorKey, "error/file")),
			 keyString (keyGetMeta (errorKey, "error/line")));
		fprintf (stderr, "reason: : %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
		fprintf (stderr, "mountpoint: : %s\n", keyString (keyGetMeta (errorKey, "error/mountpoint")));
		fprintf (stderr, "configfile: : %s\n", keyString (keyGetMeta (errorKey, "error/configfile")));
#endif
		copyError (errorKey, parentKey, key);
		rc = ERROR;
	}
	keyDel (errorKey);
	keyDel (testKey);
	return rc;
}

static inline int checkCleanup (DispatchConfig * config, Key * key, KeySet * returned, RC rc)
{
	switch (config->onError)
	{
	case FAIL:
		if (rc == ERROR)
			return ERROR;
		else
			return SUCCESS;
		break;
	case IGNORE:
		return SUCCESS;
		break;
	case DROPKEY:
		if (rc == ERROR) keyDel (ksLookup (returned, (Key *)key, KDB_O_POP));
		return SUCCESS;
		break;
	default:
		return rc;
		break;
	}
}

// actual typechecker
// first iterate over all checks for the current type and if needed
// recursively call itself for every supertype (DFS)
static int doTypeCheck (DispatchConfig * config, TypeConfig * tc, ArgumentConfig * argConfig, const Key * key, Key * parentKey,
			KeySet * returned)
{
	ksRewind (tc->checks);
	Key * checkMeta;
	TypeType t = getTypeType (tc);
	RC rc = ERROR;
	if (t == SUMTYPE)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "\t\t%s has SUMTYPE\n", keyName (key));
#endif
		rc = ERROR;
	}
	else if (t == SUBTYPE)
	{
		rc = SUCCESS;
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "\t\t%s has SUBTYPE\n", keyName (key));
#endif
	}
	else if (t == SKEL)
		return SUCCESS;
	KeySet * args = NULL;
	if (argConfig)
	{
		args = makeParamKS (tc->params, argConfig);
	}
	while ((checkMeta = ksNext (tc->checks)) != NULL)
	{
		RC r = checkKey (config, key, parentKey, args, checkMeta);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "\t\t\tcheckKey(config, %s:(%s), %s) returned %d\n", keyName (key), keyString (key), keyName (checkMeta),
			 r);
#endif
		switch (t)
		{
		case SUMTYPE:
			if (r == SUCCESS)
			{
				rc = SUCCESS;
				goto TYPECHECKDONE;
			}
			break;
		case SUBTYPE:
			if (r == ERROR)
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
	ksRewind (tc->types);
	const Key * typeMeta = NULL;
	while ((typeMeta = ksNext (tc->types)) != NULL)
	{
		TypeConfig * t_tc = *(TypeConfig **)keyValue (typeMeta);
		char * typeString = replaceParametersWithArguments (keyGetMeta (typeMeta, "internal/typedispatcher/typeString"), args);
		ArgumentConfig * ac = parseTypeString (config, typeString);
		elektraFree (typeString);
		RC r = doTypeCheck (config, t_tc, ac, key, parentKey, returned);
		freeArgumentConfig (ac);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "\t\t\tcheckKey(config, %s:(%s), %s) returned %d\n", keyName (key), keyString (key), keyName (typeMeta),
			 r);
#endif
		switch (t)
		{
		case SUMTYPE:
			if (r == SUCCESS)
			{
				rc = SUCCESS;
				goto TYPECHECKDONE;
			}
			break;
		case SUBTYPE:
			if (r == ERROR)
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
	ksDel (args);
	return checkCleanup (config, key, returned, rc);
}


// helperfunction
// checks if type exists and calls actual typecheck function doTypCheck
static int typeCheck (DispatchConfig * config, const Key * key, KeySet * returned, const char * typeString, Key * parentKey)
{
#ifdef DEVBUILD
	fprintf (stderr, "\twith type %s\n", typeString);
#endif

	ArgumentConfig * argConfig = parseTypeString (config, typeString);
	const char * typeName = NULL;
	if (!argConfig)
		typeName = typeString;
	else
		typeName = argConfig->type;
#ifdef DEVBUILD
	fprintf (stderr, "\tparseTypeString %s returned %p, typeName: %s\n", typeString, (void *)argConfig, typeName);
#endif
	TypeConfig * tc = getType (config, typeName);
	if (!tc)
	{
#ifdef DEVBUILD
		fprintf (stderr, "%s has type unknown type meta %s\n", keyName (key), typeName);
#endif
		return ERROR;
	}
	RC rc = doTypeCheck (config, tc, argConfig, key, parentKey, returned);

	freeArgumentConfig (argConfig);

	return rc;
}


// checks type metadata and calls typeCheck function
// assume subtype for multiple type entries
int validateTypeKey (Key * key, KeySet * returned, DispatchConfig * config, Key * parentKey)
{
	const Key * typeMeta = keyGetMeta (key, "type");
	if (!typeMeta) return SUCCESS;
#ifdef DEVBUILD
	fprintf (stderr, "validating %s:(%s)\n", keyName (key), keyString (key));
#endif

	if (!strncmp (keyString (typeMeta), "#", 1))
	{
		// multiple type entrys
		KeySet * metaKS = elektraKeyGetMetaKeySet (key);
		KeySet * typeKS = getAllKeysBelow (typeMeta, metaKS);
		ksDel (metaKS);
		Key * cur = NULL;
		RC rc = SUCCESS;
		while ((cur = ksNext (typeKS)) != NULL)
		{
			rc = typeCheck (config, key, returned, keyString (cur), parentKey);
			if (rc == ERROR)
			{
				ksDel (typeKS);
				return ERROR;
			}
		}
		ksDel (typeKS);
	}
	else
	{
		const char * typeName = keyString (typeMeta);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "%s has type meta \"%s\"\n", keyName (key), typeName);
#endif
		RC rc = typeCheck (config, key, returned, typeName, parentKey);
		if (rc == ERROR) return ERROR;
	}
	return SUCCESS;
}


// checks for check/ metadata and validates
int validateCheckKey (Key * key, KeySet * returned, DispatchConfig * config, Key * parentKey)
{
	keyRewindMeta (key);
	const Key * metaKey = NULL;
	Key * checkMeta = keyNew ("check", KEY_VALUE, "", KEY_META_NAME, KEY_END);
	KeySet * metaKS = elektraKeyGetMetaKeySet (key);
	KeySet * checkKS = getKeysDirectBelow (checkMeta, metaKS);
	keyDel (checkMeta);
	ksDel (metaKS);
	RC rc = SUCCESS;
	while ((metaKey = ksNext (checkKS)) != NULL)
	{
#ifdef DEVBUILD
		fprintf (stderr, "Key %s:(%s) has metaKey %s\n", keyName (key), keyString (key), keyName (metaKey));
#endif
		ValidateFunction validate = getValidateFunction (config, keyBaseName (metaKey));
		if (!validate)
		{
			ksDel (checkKS);
			return ERROR;
		}
		Key * errorKey = keyNew (0, KEY_END);
		rc = validate (key, errorKey);
#ifdef DEVBUILD
		fprintf (stderr, "%s->validateKey(%s, errorKey) returned %d\n", keyName (checkMeta), keyName (key), rc);
#endif
		if (rc == 0)
		{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
			fprintf (stderr, "number: %s\n", keyString (keyGetMeta (errorKey, "error/number")));
			fprintf (stderr, "description: : %s\n", keyString (keyGetMeta (errorKey, "error/description")));
			fprintf (stderr, "ingroup: : %s\n", keyString (keyGetMeta (errorKey, "error/ingroup")));
			fprintf (stderr, "module: : %s\n", keyString (keyGetMeta (errorKey, "error/module")));
			fprintf (stderr, "at: %s:%s\n", keyString (keyGetMeta (errorKey, "error/file")),
				 keyString (keyGetMeta (errorKey, "error/line")));
			fprintf (stderr, "reason: : %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
			fprintf (stderr, "mountpoint: : %s\n", keyString (keyGetMeta (errorKey, "error/mountpoint")));
			fprintf (stderr, "configfile: : %s\n", keyString (keyGetMeta (errorKey, "error/configfile")));
#endif
			copyError (errorKey, parentKey, key);
			rc = ERROR;
			break;
		}
		keyDel (errorKey);
	}
	ksDel (checkKS);
	return checkCleanup (config, key, returned, rc);
}
