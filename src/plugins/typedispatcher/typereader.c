#include "typehelper.h"
#include <kdbease.h>     //KeyGetRelativeName, elektraArrayIncName
#include <kdbhelper.h>   //elektraCalloc
#include <kdbproposal.h> //keyRel2, elektraKeyGetMetaKeySet
#include <stdio.h>


// creates skeletons(name+type+scope) for new types and fails if type already exist
static int readTypeNames (DispatchConfig * config, const Key * scope, const Key * typeKey, Key * parentKey ELEKTRA_UNUSED)
{
	if (!typeKey) return ERROR;

	const char * typeName = keyBaseName (typeKey);
#ifdef DEVBUILD
	fprintf (stderr, "\tdefines type %s\n", typeName);
#endif
	TypeConfig * tc = getType (config, typeName);
	if (tc && keyCmp (tc->scope, scope))
	{
#ifdef DEVBUILD
		fprintf (stderr, "ERROR: %s already defined\n", typeName);
#endif
		return ERROR;
	}
	else if (tc)
	{
		return SUCCESS;
	}

	tc = newTypeConfig ();
	if (!tc) return ERROR;
	keySetName (tc->scope, keyName (scope));

	Key * newType = keyNew (typeName, KEY_META_NAME, KEY_BINARY, KEY_SIZE, sizeof (TypeConfig *), KEY_VALUE, &tc, KEY_END);
	ksAppendKey (config->types, newType);

	return SUCCESS;
}

// iterate over a defined types check/<PLUGIN> meta
// and add a new key holding each plugins config to TypeConfig->checks
static void readCheckTypeConfig (TypeConfig * tc, const Key * key ELEKTRA_UNUSED, const Key * typeKey, KeySet * defKS,
				 Key * parentKey ELEKTRA_UNUSED)
{
	Key * cur = NULL;
	Key * checkKey = keyNew (keyName (typeKey), KEY_META_NAME, KEY_END);
	keyAddBaseName (checkKey, "check");
	KeySet * typeKS = getAllKeysBelow (checkKey, defKS);
	KeySet * dupKS = ksDup (typeKS);
	while ((cur = ksNext (typeKS)) != NULL)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "\t%s:(%s)\n", keyName (cur), keyString (cur));
#endif
		if (keyRel2 (checkKey, cur, ELEKTRA_REL_DIRECT_BELOW_SAME_NS) == 1)
		{
			// cur has form define/type/<TYPENAME>/check/<PLUGIN>
			Key * appendKey = keyNew (keyBaseName (cur), KEY_META_NAME, KEY_END);
			const char * metaName = elektraKeyGetRelativeName (cur, typeKey);
			keySetMeta (appendKey, metaName, keyString (cur));
#ifdef DEVBUILD
			fprintf (stderr, "\t\tadding Metadata %s:(%s) to key %s\n", metaName, keyString (cur), keyName (appendKey));
#endif
			if (!strncmp (keyString (cur), "#", 1))
			{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
				fprintf (stderr, "\t\t\t is array\n");
#endif
				// array / multiple definitions
				KeySet * c = getAllKeysBelow (cur, dupKS);

				Key * cur2;
				while ((cur2 = ksNext (c)) != NULL)
				{
					metaName = elektraKeyGetRelativeName (cur2, typeKey);
					keySetMeta (appendKey, metaName, keyString (cur2));
#ifdef DEVBUILD
					fprintf (stderr, "\t\t\tadding %s:(%s) to %s\n", metaName, keyString (cur2), keyName (appendKey));
#endif
				}
				ksDel (c);
			}
			else
			{
				KeySet * extraConfig = getAllKeysBelow (cur, defKS);
				Key * extraKey;
				while ((extraKey = ksNext (extraConfig)) != NULL)
				{
					metaName = elektraKeyGetRelativeName (extraKey, typeKey);
					keySetMeta (appendKey, metaName, keyString (extraKey));
				}
				ksDel (extraConfig);
			}
			ksAppendKey (tc->checks, appendKey);
		}
	}
	ksDel (dupKS);
	keyDel (checkKey);
	ksDel (typeKS);
}


// iterate over a defined types type meta, checks if the supertype exists
// and adds a reference to the supertype to TypeConfig->types
static int readTypeTypeSuperTypes (DispatchConfig * config, TypeConfig * tc, const Key * key, const Key * typeKey, KeySet * defKS,
				   Key * parentKey ELEKTRA_UNUSED)
{
	Key * relKey = keyNew (keyName (typeKey), KEY_META_NAME, KEY_END);
	keyAddBaseName (relKey, "type");
	Key * lookup = ksLookup (defKS, relKey, KDB_O_NONE);
	KeySet * typeKS = NULL;
	if (!strncmp (keyString (lookup), "#", 1))
	{
		typeKS = getAllKeysBelow (relKey, defKS);
	}
	else
	{
		typeKS = ksNew (0, KS_END);
		ksAppendKey (typeKS, lookup);
	}
	ksRewind (typeKS);
	Key * cur;
	while ((cur = ksNext (typeKS)) != NULL)
	{
		const char * relTypeName = NULL;
		const char * typeString = keyString (cur);

		ArgumentConfig * argConfig = parseTypeString (config, typeString);
		if (!argConfig)
			relTypeName = typeString;
		else
			relTypeName = argConfig->type;

		TypeConfig * rt = getType (config, relTypeName);
		if (!rt)
		{
#ifdef DEVBUILD
			fprintf (stderr, "%s references %s, but doesn't exist\n", keyName (key), relTypeName);
#endif
			freeArgumentConfig (argConfig);
			keyDel (relKey);
			ksDel (typeKS);
			return ERROR;
		}
		else
		{
			if (!isWithinScope (rt, key))
			{
#ifdef DEVBUILD
				fprintf (stderr, "%s references %s, but not within scope\n", keyName (key), relTypeName);
#endif
				freeArgumentConfig (argConfig);
				keyDel (relKey);
				ksDel (typeKS);
				return ERROR;
			}
#ifdef DEVBUILD
			fprintf (stderr, "\t\t\t%s references %s - adding\n", keyName (key), relTypeName);
#endif
			Key * refKey = getTypeKey (config, relTypeName);
			Key * appendKey = keyNew (keyName (refKey), KEY_META_NAME, KEY_BINARY, KEY_SIZE, keyGetValueSize (refKey),
						  KEY_VALUE, keyValue (refKey), KEY_END);
			keyCopyAllMeta (appendKey, refKey);

			// in cases like define/type/a/type := otherType (a, b, c) we need to store
			// the whole string to keep the arguments.
			keySetMeta (appendKey, "internal/typedispatcher/typeString", typeString);
			ksAppendKey (tc->types, appendKey);
			freeArgumentConfig (argConfig);
		}
	}
	keyDel (relKey);
	ksDel (typeKS);
	return SUCCESS;
}


// read SUMTYPE config
// if array-style config is found create a new type <TYPENAME>/#*
// for every array element
static int readSumTypeConfig (DispatchConfig * config, TypeConfig * tc, const Key * key, const Key * typeKey, KeySet * defKS,
			      Key * parentKey)
{
	KeySet * typeKS = NULL;
	Key * checkKey = keyNew (keyName (typeKey), KEY_META_NAME, KEY_END);
	keyAddBaseName (checkKey, "#0");
	typeKS = getAllKeysBelow (checkKey, defKS);
	if (ksGetSize (typeKS) <= 0)
	{
#ifdef DEVBUILD
		fprintf (stderr, "no array below SUMTYPE %s\n", keyBaseName (typeKey));
#endif
		// no array below, treat every entry as a separate type
		ksDel (typeKS);
		keyDel (checkKey);

		readCheckTypeConfig (tc, key, typeKey, defKS, parentKey);

		return readTypeTypeSuperTypes (config, tc, key, typeKey, defKS, parentKey);
	}
	else
	{
// array
#ifdef DEVBUILD
		fprintf (stderr, "array blow SUMTYPE %s\n", keyBaseName (typeKey));
#endif
		do
		{
			ksDel (typeKS);
			typeKS = getAllKeysBelow (checkKey, defKS);
			if (ksGetSize (typeKS) <= 0)
			{
				ksDel (typeKS);
				break;
			}
			TypeConfig * stc = NULL;
			stc = newTypeConfig ();
			stc->type = SUBTYPE;
			readCheckTypeConfig (stc, key, checkKey, typeKS, parentKey);
			RC rc = ERROR;
			rc = readTypeTypeSuperTypes (config, stc, key, checkKey, typeKS, parentKey);
			if (rc == ERROR)
			{
#ifdef DEVBUILD
				fprintf (stderr, "SUMTYPE %s/%s readTypeTypeSuperTypes failed\n", keyBaseName (typeKey),
					 keyBaseName (checkKey));
#endif
				ksDel (typeKS);
				elektraFree (stc);
				keyDel (checkKey);
				return ERROR;
			}
			Key * appendKey = keyNew (keyBaseName (typeKey), KEY_META_NAME, KEY_BINARY, KEY_SIZE, sizeof (TypeConfig *),
						  KEY_VALUE, &stc, KEY_END);
			keyAddBaseName (appendKey, keyBaseName (checkKey));
#ifdef DEVBUILD
			fprintf (stderr, "appending new type %s\n", keyName (appendKey));
#endif
			ksAppendKey (config->types, appendKey);
			ksAppendKey (tc->types, appendKey);
		} while (elektraArrayIncName (checkKey) != ERROR);
	}
	keyDel (checkKey);
	return SUCCESS;
}


// helper for readability
static void readSubTypeCheckConfig (TypeConfig * tc, const Key * key, const Key * typeKey, KeySet * defKS, Key * parentKey)
{
	readCheckTypeConfig (tc, key, typeKey, defKS, parentKey);
}

// helper for readability
static int readSubTypeTypeSuperTypes (DispatchConfig * config, TypeConfig * tc, const Key * key, const Key * typeKey, KeySet * defKS,
				      Key * parentKey)
{
	return readTypeTypeSuperTypes (config, tc, key, typeKey, defKS, parentKey);
}

static int readTypeParameters (TypeConfig * tc, const Key * key ELEKTRA_UNUSED, const Key * typeKey, KeySet * defKS,
			       Key * parentKey ELEKTRA_UNUSED)
{
	Key * checkKey = keyNew (keyName (typeKey), KEY_META_NAME, KEY_END);
	keyAddBaseName (checkKey, "parameter");
	Key * paramKey = ksLookup (defKS, checkKey, KDB_O_NONE);

	if (!paramKey)
	{
		keyDel (checkKey);
		return SUCCESS;
	}
	else
	{
		if (!strncmp (keyString (paramKey), "#", 1))
		{
			// is param array
			KeySet * typeKS = getKeysDirectBelow (checkKey, defKS);
			Key * cur = NULL;
			while ((cur = ksNext (typeKS)) != NULL)
			{
				Key * parameter = keyNew (keyBaseName (cur), KEY_META_NAME, KEY_VALUE, keyString (cur), KEY_END);
#ifdef DEVBUILD
				fprintf (stderr, "adding parameter key %s:(%s) to type %s\n", keyName (parameter), keyString (parameter),
					 keyBaseName (typeKey));
#endif
				ksAppendKey (tc->params, parameter);
			}
			ksDel (typeKS);
		}
		else
		{
			// single parameter
			Key * parameter = keyNew ("#0", KEY_META_NAME, KEY_VALUE, keyString (paramKey), KEY_END);
#ifdef DEVBUILD
			fprintf (stderr, "adding parameter key %s:(%s) to type %s\n", keyName (parameter), keyString (parameter),
				 keyBaseName (typeKey));
#endif
			ksAppendKey (tc->params, parameter);
		}
	}
	keyDel (checkKey);
	return SUCCESS;
}

// read type configuration
// only call after type skeletons are created with readTypeNames
// depending on the algebraic type call readSubTypeCheckConfig/readSubTypeTypeSuperTypes
// or readSumTypeConfig
static int readTypeConfig (DispatchConfig * config, const Key * key, const Key * typeKey, KeySet * defKS, Key * parentKey)
{
	if (!typeKey) return ERROR;

	const char * typeName = keyBaseName (typeKey);
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
	fprintf (stderr, "populating %s\n", typeName);
#endif

	TypeConfig * tc = getType (config, typeName);
	if (!tc)
	{
#ifdef DEVBUILD
		fprintf (stderr, "datatype %s not found but should exist\n", typeName);
#endif
		return ERROR;
	}
	else
	{
#ifdef DEVBUILD
		fprintf (stderr, "datatype %s found in config->types\n", typeName);
#endif
		if (getTypeType (tc) != SKEL)
		{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
			fprintf (stderr, "%s already exists - changing types at runtime not supported yet\n", typeName);
#endif
			return SUCCESS;
		}
		else
		{
			setTypeType (tc, typeKey);
		}
	}

	RC rc = ERROR;

	if (getTypeType (tc) == SUBTYPE)
	{
		rc = readTypeParameters (tc, key, typeKey, defKS, parentKey);
		if (rc == ERROR) return ERROR;
		readSubTypeCheckConfig (tc, key, typeKey, defKS, parentKey);

		rc = readSubTypeTypeSuperTypes (config, tc, key, typeKey, defKS, parentKey);
	}
	else
	{
		rc = readSumTypeConfig (config, tc, key, typeKey, defKS, parentKey);
	}
	return rc;
}

// iterate over define/type metadata
// and and call readTypeNames and readTypeConfig for each type
int getTypeDefinitions (Key * key, DispatchConfig * config, Key * parentKey)
{
#ifdef DEVBUILD
	fprintf (stderr, "Getting type definitions from %s\n", keyName (key));
#endif
	RC rc = SUCCESS;
	KeySet * metaKS = elektraKeyGetMetaKeySet (key);
	Key * defineParent = keyNew ("define/type", KEY_META_NAME, KEY_END);
	KeySet * defKS = getAllKeysBelow (defineParent, metaKS);

	if (!defKS)
	{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
		fprintf (stderr, "Key %s has no type definitions\n", keyName (key));
#endif
	}
	else
	{
		// typeParents - define/type/<TYPENAME>
		KeySet * typeParentKS = getKeysDirectBelow (defineParent, defKS);
		if (!typeParentKS)
		{
#if defined(DEVBUILD) && defined(VERBOSEBUILD)
			fprintf (stderr, "Key %s has no metakey define/type/<TYPENAME>\n", keyName (key));
#endif
			ksDel (defKS);
		}
		else
		{
			Key * typeKey = NULL;
			ksRewind (typeParentKS);
			while ((typeKey = ksNext (typeParentKS)) != NULL)
			{
				// since metakeys are ordered alphabetical
				// create skeletons to handle possible dependencies
				rc = readTypeNames (config, key, typeKey, parentKey);
				if (rc == ERROR) goto GETDEFCLEANUP;
			}
			ksRewind (typeParentKS);
			while ((typeKey = ksNext (typeParentKS)) != NULL)
			{
				// create actual type config
				rc = readTypeConfig (config, key, typeKey, defKS, parentKey);
				if (rc == ERROR) goto GETDEFCLEANUP;
			}
		}
	GETDEFCLEANUP:
		ksDel (typeParentKS);
		ksDel (defKS);
	}
	ksDel (metaKS);
	keyDel (defineParent);
	return rc;
}
