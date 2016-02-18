/**
 * @file
 *
 * @brief Source for conditionals plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include "conditionals.h"
#include <ctype.h>
#include <errno.h>
#include <kdberrors.h>
#include <math.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EPSILON 0.00001

typedef enum { EQU, NOT, LT, LE, GT, GE, SET } Comparator;

typedef enum { CONDITION, ASSIGN } Operation;

static int isNumber (const char * s)
{
	char * endPtr = NULL;
	int ret;
	ret = strtol (s, &endPtr, 10);
	if (*endPtr != 0)
	{
		return 0;
	}
	else if (ret == 0 && errno == EINVAL)
	{
		return 0;
	}
	else if (*endPtr == '.')
	{
		ret = strtof (s, &endPtr);
		if (*endPtr != 0)
		{
			return 0;
		}
		else if (ret == 0 && errno == EINVAL)
		{
			return 0;
		}
		else
		{
			return 2;
		}
	}
	else
	{
		return 1;
	}
}
static int compareStrings (const char * s1, const char * s2)
{
	int ret;
	int ret2;
	float result;
	int retval = -1;
	if (s1 == NULL)
	{
		retval = -1;
	}
	else if (s2 == NULL)
	{
		retval = 1;
	}
	else if ((ret = isNumber (s1)) && (ret2 = isNumber (s2)))
	{
		if (ret == 2 || ret2 == 2)
		{
			result = (fabs (atof (s1) - atof (s2)));
			if (result < EPSILON)
				retval = 0;
			else if (result > 0)
				retval = 1;
			else if (result < 0)
				retval = -1;
		}
		else
		{
			result = atol (s1) - atol (s2);
			retval = result;
		}
	}
	else
	{
		retval = strcmp (s1, s2);
	}
	return retval;
}

static int evalCondition (const char * leftSide, Comparator cmpOp, const char * rightSide, const char * condition, KeySet * ks,
			  Key * parentKey)
{
	char * lookupName = NULL;
	char * compareTo = NULL;
	Key * key;
	int len;
	long result = 0;
	if (rightSide[0] == '\'')
	{
		char * endPos = strchr (rightSide + 1, '\'');
		if (!endPos)
		{
			result = -1;
			goto Cleanup;
		}
		if (elektraRealloc ((void **)&compareTo, endPos - rightSide) < 0)
		{
			ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
			result = -1;
			goto Cleanup;
		}
		memset (compareTo, 0, endPos - rightSide);
		strncat (compareTo, rightSide + 1, endPos - rightSide - 1);
	}
	else if (rightSide && elektraStrLen (rightSide) > 1)
	{
		len = keyGetNameSize (parentKey) + elektraStrLen (rightSide);
		if (elektraRealloc ((void **)&lookupName, len) < 0)
		{
			ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
			result = -1;
			goto Cleanup;
		}
		snprintf (lookupName, len, "%s/%s", keyName (parentKey), rightSide);
		key = ksLookupByName (ks, lookupName, 0);
		if (!key)
		{
			ELEKTRA_SET_ERRORF (133, parentKey, "Key %s not found but is required for the evaluation of %s", lookupName,
					    condition);
			result = -2;
			goto Cleanup;
		}
		if (elektraRealloc ((void **)&compareTo, keyGetValueSize (key)) < 0)
		{
			ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
			result = -1;
			goto Cleanup;
		}
		strcpy (compareTo, keyString (key));
	}

	len = keyGetNameSize (parentKey) + elektraStrLen (leftSide);
	if (elektraRealloc ((void **)&lookupName, len) < 0)
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
		result = -1;
		goto Cleanup;
	}
	snprintf (lookupName, len, "%s/%s", keyName (parentKey), leftSide);
	key = ksLookupByName (ks, lookupName, 0);
	if (!key)
	{
		ELEKTRA_SET_ERRORF (133, parentKey, "Key %s not found but is required for the evaluation of %s", lookupName, condition);
		result = -2;
		goto Cleanup;
	}
	long ret;
	ret = compareStrings (keyString (key), compareTo);
	switch (cmpOp)
	{
	case EQU:
		if (!ret) result = 1;
		break;
	case NOT:
		if (ret) result = 1;
		break;
	case LT:
		if (ret < 0) result = 1;
		break;
	case LE:
		if (ret <= 0) result = 1;
		break;
	case GT:
		if (ret > 0) result = 1;
		break;
	case GE:
		if (ret >= 0) result = 1;
		break;
	case SET:
		keySetString (key, compareTo);
		result = 1;
		break;
	default:
		result = -1;
		break;
	}
// freeing allocated heap
Cleanup:
	if (lookupName) elektraFree (lookupName);
	if (compareTo) elektraFree (compareTo);
	return result;
}


static int parseSingleCondition (const char * condition, KeySet * ks, Key * parentKey)
{
	Comparator cmpOp;
	char * opStr;
	if ((opStr = strstr (condition, "==")))
		cmpOp = EQU;
	else if ((opStr = strstr (condition, "!=")))
		cmpOp = NOT;
	else if ((opStr = strstr (condition, "<=")))
		cmpOp = LE;
	else if ((opStr = strstr (condition, "<")))
		cmpOp = LT;
	else if ((opStr = strstr (condition, ">=")))
		cmpOp = GE;
	else if ((opStr = strstr (condition, ">")))
		cmpOp = GT;
	else if ((opStr = strstr (condition, ":=")))
		cmpOp = SET;
	else
		return -1;
	int opLen;
	if (cmpOp == LT || cmpOp == GT)
		opLen = 1;
	else
		opLen = 2;
	unsigned long startPos = 0;
	unsigned long endPos = 0;
	char * ptr = (char *)condition;
	while (isspace (*ptr))
	{
		++ptr;
		++startPos;
	}
	ptr = opStr - 1;
	while (isspace (*ptr))
	{
		--ptr;
		++endPos;
	}
	char * leftSide = NULL;
	int len = opStr - condition - endPos - startPos + 2;
	leftSide = elektraMalloc (len);
	strncpy (leftSide, condition + startPos, len - 2);
	leftSide[len - 2] = '\0';
	startPos = 0;
	endPos = 0;
	ptr = opStr + opLen;
	while (isspace (*ptr))
	{
		++ptr;
		++startPos;
	}
	ptr = (char *)condition + (elektraStrLen (condition) - 2);
	while (isspace (*ptr))
	{
		--ptr;
		++endPos;
	}
	char * rightSide = NULL;
	len = elektraStrLen (condition) - (opStr - condition) - opLen - endPos - startPos;
	rightSide = elektraMalloc (len);
	strncpy (rightSide, opStr + opLen + startPos, len - 1);
	rightSide[len - 1] = '\0';
	int ret;
	ret = evalCondition (leftSide, cmpOp, rightSide, condition, ks, parentKey);
	elektraFree (rightSide);
	elektraFree (leftSide);
	return ret;
}
static char * isAssign (char * expr)
{
	char * firstPtr = expr;
	char * lastPtr = expr + elektraStrLen (expr) - 2;
	while (isspace (*firstPtr))
		++firstPtr;
	while (isspace (*lastPtr))
		--lastPtr;
	if (*firstPtr != '\'' || *lastPtr != '\'') return NULL;
	if (firstPtr == lastPtr) return NULL;
	char * nextMark = strchr (firstPtr + 1, '\'');
	if (nextMark != lastPtr) return NULL;
	*lastPtr = '\0';
	*firstPtr = '\0';
	++firstPtr;
	return firstPtr;
}
static int parseConditionString (const Key * meta, Key * parentKey, Key * key, KeySet * ks, Operation op)
{
	const char * conditionString = keyString (meta);
	const char * regexString =
		"(\\(([^\\)]*)\\))[[:space:]]*(\\?)[[:space:]]*(\\(([^\\)]*)\\))[[:space:]]*(:[[:space:]]*(\\(([^\\)]*)\\))){0,1}";
	regex_t regex;
	int ret;
	if ((ret = regcomp (&regex, regexString, REG_EXTENDED | REG_NEWLINE)))
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Couldn't compile regex: most likely out of memory"); // the regex compiles so the only
													// possible error would be out of
													// memory
		ksDel (ks);
		return -1;
	}
	int subMatches = 9;
	regmatch_t m[subMatches];
	char * ptr = (char *)conditionString;
	int nomatch = regexec (&regex, ptr, subMatches, m, 0);
	if (nomatch)
	{
		ELEKTRA_SET_ERRORF (134, parentKey, "Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
				    conditionString);
		regfree (&regex);
		ksDel (ks);
		return -1;
	}
	if (m[2].rm_so == -1 || m[5].rm_so == -1)
	{
		ELEKTRA_SET_ERRORF (134, parentKey, "Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
				    conditionString);
		regfree (&regex);
		ksDel (ks);
		return -1;
	}
	char * condition = NULL;
	char * thenexpr = NULL;
	char * elseexpr = NULL;
	int startPos;
	int endPos;
	startPos = m[2].rm_so + (ptr - conditionString);
	endPos = m[2].rm_eo + (ptr - conditionString);
	condition = elektraMalloc (endPos - startPos + 1);
	strncpy (condition, conditionString + startPos, endPos - startPos);
	condition[endPos - startPos] = '\0';

	startPos = m[5].rm_so + (ptr - conditionString);
	endPos = m[5].rm_eo + (ptr - conditionString);
	thenexpr = elektraMalloc (endPos - startPos + 1);
	strncpy (thenexpr, conditionString + startPos, endPos - startPos);
	thenexpr[endPos - startPos] = '\0';


	if (m[8].rm_so != -1)
	{
		startPos = m[8].rm_so + (ptr - conditionString);
		endPos = m[8].rm_eo + (ptr - conditionString);
		elseexpr = elektraMalloc (endPos - startPos + 1);
		strncpy (elseexpr, conditionString + startPos, endPos - startPos);
		elseexpr[endPos - startPos] = '\0';
	}
	ret = parseSingleCondition (condition, ks, parentKey);
	if (ret == 1)
	{
		if (op == ASSIGN)
		{
			const char * assign = isAssign (thenexpr);
			if (assign != NULL)
			{
				keySetString (key, assign);
				ret = 1;
				goto CleanUp;
			}
			else
			{
				ret = -1;
				ELEKTRA_SET_ERRORF (134, parentKey,
						    "Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
						    thenexpr);
				goto CleanUp;
			}
		}
		else
		{
			ret = parseSingleCondition (thenexpr, ks, parentKey);
			if (ret == 0)
			{
				ELEKTRA_SET_ERRORF (135, parentKey, "Validation of %s failed. (%s failed)", conditionString, thenexpr);
			}
			else if (ret == (-1))
			{
				ELEKTRA_SET_ERRORF (134, parentKey,
						    "Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
						    thenexpr);
			}
		}
	}
	else if (ret == 0)
	{
		if (elseexpr)
		{
			if (op == ASSIGN)
			{
				const char * assign = isAssign (elseexpr);
				if (assign != NULL)
				{
					keySetString (key, assign);
					ret = 1;
					goto CleanUp;
				}
				else
				{
					ret = -1;
					ELEKTRA_SET_ERRORF (
						134, parentKey,
						"Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
						elseexpr);
					goto CleanUp;
				}
			}
			else
			{
				ret = parseSingleCondition (elseexpr, ks, parentKey);
				if (ret == 0)
				{
					ELEKTRA_SET_ERRORF (135, parentKey, "Validation of %s failed. (%s failed)", conditionString,
							    elseexpr);
				}
				else if (ret == (-1))
				{
					ELEKTRA_SET_ERRORF (
						134, parentKey,
						"Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
						elseexpr);
				}
			}
		}
		else
		{
			ELEKTRA_SET_ERRORF (135, parentKey, "Validation of %s failed. (%s failed) ", conditionString, condition);
			ret = 1;
		}
	}
	else if (ret == (-1))
	{
		ELEKTRA_SET_ERRORF (134, parentKey, "Invalid syntax: \"%s\". Check kdb info conditionals for additional information\n",
				    condition);
	}

CleanUp:
	elektraFree (condition);
	elektraFree (thenexpr);
	if (elseexpr) elektraFree (elseexpr);
	regfree (&regex);
	ksDel (ks);
	return ret;
}

int elektraConditionalsGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (keyName (parentKey), "system/elektra/modules/conditionals"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/conditionals", KEY_VALUE, "conditionals plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/conditionals/exports", KEY_END),
			keyNew ("system/elektra/modules/conditionals/exports/get", KEY_FUNC, elektraConditionalsGet, KEY_END),
			keyNew ("system/elektra/modules/conditionals/exports/set", KEY_FUNC, elektraConditionalsSet, KEY_END),
#include ELEKTRA_README (conditionals)
			keyNew ("system/elektra/modules/conditionals/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; /* success */
	}
	Key * cur;
	int ret = 0;
	while ((cur = ksNext (returned)) != NULL)
	{
		Key * conditionMeta = (Key *)keyGetMeta (cur, "check/condition");
		Key * assignMeta = (Key *)keyGetMeta (cur, "assign/condition");
		int result;
		if (conditionMeta)
		{
			result = parseConditionString (conditionMeta, parentKey, cur, ksDup (returned), CONDITION);
			if (result == -1)
			{
				ret |= -1;
			}
			else if (result == 0)
			{
				ret |= -1;
			}
			else if (result == 1)
			{
				ret |= 1;
			}
			else if (result == -2)
			{
				ret |= -1;
			}
		}
		if (assignMeta)
		{
			result = parseConditionString (assignMeta, parentKey, cur, ksDup (returned), ASSIGN);
			if (result == -1)
			{
				ret |= -1;
			}
			else
			{
				ret |= 1;
			}
		}
	}

	return ret; /* success */
}

int elektraConditionalsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	Key * cur;
	int ret = 0;
	while ((cur = ksNext (returned)) != NULL)
	{
		Key * conditionMeta = (Key *)keyGetMeta (cur, "check/condition");
		Key * assignMeta = (Key *)keyGetMeta (cur, "assign/condition");
		int result;
		if (conditionMeta)
		{
			result = parseConditionString (conditionMeta, parentKey, cur, ksDup (returned), CONDITION);
			if (result == -1)
			{
				ret |= -1;
			}
			else if (result == 0)
			{
				ret |= -1;
			}
			else if (result == 1)
			{
				ret |= 1;
			}
			else if (result == -2)
			{
				ret |= -1;
			}
		}
		if (assignMeta)
		{
			result = parseConditionString (assignMeta, parentKey, cur, ksDup (returned), ASSIGN);
			if (result == -1)
			{
				ret |= -1;
			}
			else
			{
				ret |= 1;
			}
		}
	}

	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (conditionals)
{
	// clang-format off
	return elektraPluginExport("conditionals",
			ELEKTRA_PLUGIN_GET,	&elektraConditionalsGet,
			ELEKTRA_PLUGIN_SET,	&elektraConditionalsSet,
			ELEKTRA_PLUGIN_END);
}

