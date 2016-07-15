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

#include <ctype.h>
#include <errno.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <math.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "conditionals.h"

#define EPSILON 0.00001

#if defined(__APPLE__)
#define REGEX_FLAGS_CONDITION (REG_EXTENDED | REG_NEWLINE | REG_ENHANCED | REG_MINIMAL)
#else
#define REGEX_FLAGS_CONDITION (REG_EXTENDED | REG_NEWLINE)
#endif

typedef enum {
	EQU,
	NOT,
	LT,
	LE,
	GT,
	GE,
	SET,
	NEX,
	AND,
	OR,
} Comparator;

typedef enum { CONDITION, ASSIGN } Operation;

typedef enum {
	TRUE = 1,
	FALSE = 0,
	ERROR = -1,
	NOEXPR = -3,
} CondResult;

static int isValidSuffix (char * suffix, const Key * suffixList)
{
	if (!suffixList) return 0;
	char * searchString = elektraMalloc (strlen (suffix) + 3);
	snprintf (searchString, strlen (suffix) + 3, "'%s'", suffix);
	int ret = 0;
	if (strstr (keyString (suffixList), searchString))
	{
		ret = 1;
	}
	elektraFree (searchString);
	return ret;
}

static int isNumber (const char * s, const Key * suffixList)
{
	char * endPtr = NULL;
	int ret;
	ret = strtol (s, &endPtr, 10);
	if (*endPtr != 0 && isValidSuffix (endPtr, suffixList))
	{
		return 1;
	}
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
		if (*endPtr != 0 && isValidSuffix (endPtr, suffixList))
		{
			return 2;
		}
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
static int compareStrings (const char * s1, const char * s2, const Key * suffixList)
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
	else if (*s1 == '\0' && *s2 != '\0')
	{
		retval = -1;
	}
	else if (*s2 == '\0' && *s1 != '\0')
	{
		retval = 1;
	}
	else if (*s1 == '\0' && *s2 == '\0')
	{
		retval = 0;
	}
	else if ((ret = isNumber (s1, suffixList)) && (ret2 = isNumber (s2, suffixList)))
	{
		char * s1EndPtr;
		char * s2EndPtr;
		if (ret == 2 || ret2 == 2)
		{
			float s1Value = strtof (s1, &s1EndPtr);
			float s2Value = strtof (s2, &s2EndPtr);
			if (!strcmp (s1EndPtr, s2EndPtr) || *s1EndPtr == 0 || *s2EndPtr == 0)
			{
				result = (fabs (s1Value - s2Value));
				if (result < EPSILON)
				{
					retval = 0;
				}
				else if (result > 0)
				{
					retval = 1;
				}
				else if (result < 0)
				{
					retval = -1;
				}
			}
			else
			{
				retval = strcmp (s1, s2);
			}
		}
		else
		{
			int s1Value = strtol (s1, &s1EndPtr, 10);
			int s2Value = strtol (s2, &s2EndPtr, 10);
			if (!strcmp (s1EndPtr, s2EndPtr) || *s1EndPtr == 0 || *s2EndPtr == 0)
			{
				result = s1Value - s2Value;
				retval = result;
			}
			else
			{
				retval = strcmp (s1, s2);
			}
		}
	}
	else
	{
		retval = strcmp (s1, s2);
	}
	return retval;
}

static CondResult evalCondition (const Key * curKey, const char * leftSide, Comparator cmpOp, const char * rightSide,
				 const char * condition, const Key * suffixList, KeySet * ks, Key * parentKey)
{
	char * lookupName = NULL;
	char * compareTo = NULL;
	Key * key;
	int len;
	long result = 0;
	if (rightSide)
	{
		if (rightSide[0] == '\'')
		{
			// right side of the statement is a literal enclosed by ''
			char * endPos = strchr (rightSide + 1, '\'');
			if (!endPos)
			{
				result = ERROR;
				goto Cleanup;
			}
			if (elektraRealloc ((void **)&compareTo, endPos - rightSide) < 0)
			{
				ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
				result = ERROR;
				goto Cleanup;
			}
			memset (compareTo, 0, endPos - rightSide);
			strncat (compareTo, rightSide + 1, endPos - rightSide - 1);
		}
		else if (rightSide && elektraStrLen (rightSide) > 1)
		{
			// not a literal, it has to be a key
			if (rightSide[0] == '@')
				len = keyGetNameSize (parentKey) + elektraStrLen (rightSide);
			else if (!strncmp (rightSide, "..", 2) || (rightSide[0] == '.'))
				len = keyGetNameSize (curKey) + elektraStrLen (rightSide);
			else
				len = elektraStrLen (rightSide);

			if (elektraRealloc ((void **)&lookupName, len) < 0)
			{
				ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
				result = ERROR;
				goto Cleanup;
			}
			if (rightSide[0] == '@')
				snprintf (lookupName, len, "%s/%s", keyName (parentKey), rightSide + 1);
			else if (rightSide[0] == '.') // either starts with . or .., doesn't matter at this point
				snprintf (lookupName, len, "%s/%s", keyName (curKey), rightSide);
			else
				snprintf (lookupName, len, "%s", rightSide);

			key = ksLookupByName (ks, lookupName, 0);
			if (!key)
			{
				if (!keyGetMeta (parentKey, "error"))
				{
					ELEKTRA_SET_ERRORF (133, parentKey, "Key %s not found but is required for the evaluation of %s",
							    lookupName, condition);
				}
				result = FALSE;
				goto Cleanup;
			}
			if (elektraRealloc ((void **)&compareTo, keyGetValueSize (key)) < 0)
			{
				ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
				result = ERROR;
				goto Cleanup;
			}
			strcpy (compareTo, keyString (key));
		}
	}
	if (leftSide[0] == '@')
		len = keyGetNameSize (parentKey) + elektraStrLen (leftSide);
	else if (!strncmp (leftSide, "..", 2) || (leftSide[0] == '.'))
		len = keyGetNameSize (curKey) + elektraStrLen (leftSide);
	else
		len = elektraStrLen (leftSide);

	if (elektraRealloc ((void **)&lookupName, len) < 0)
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Out of memory");
		result = ERROR;
		goto Cleanup;
	}
	if (leftSide[0] == '@')
		snprintf (lookupName, len, "%s/%s", keyName (parentKey), leftSide + 1);
	else if (leftSide[0] == '.') // either . or .., doesn't matter here
		snprintf (lookupName, len, "%s/%s", keyName (curKey), leftSide);
	else
		snprintf (lookupName, len, "%s", leftSide);
	key = ksLookupByName (ks, lookupName, 0);
	if (cmpOp == NEX)
	{
		if (key)
			result = FALSE;
		else
			result = TRUE;
		goto Cleanup;
	}
	if (!key && cmpOp != OR && cmpOp != AND)
	{
		if (!keyGetMeta (parentKey, "error"))
		{
			ELEKTRA_SET_ERRORF (133, parentKey, "Key %s not found but is required for the evaluation of %s", lookupName,
					    condition);
		}
		result = FALSE;
		goto Cleanup;
	}
	long ret;
	if (cmpOp == OR || cmpOp == AND)
		ret = compareStrings (leftSide, rightSide, NULL);
	else
		ret = compareStrings (keyString (key), compareTo, suffixList);
	switch (cmpOp)
	{
	case EQU:
		if (!ret) result = TRUE;
		break;
	case NOT:
		if (ret) result = TRUE;
		break;
	case LT:
		if (ret < 0) result = TRUE;
		break;
	case LE:
		if (ret <= 0) result = TRUE;
		break;
	case GT:
		if (ret > 0) result = TRUE;
		break;
	case GE:
		if (ret >= 0) result = TRUE;
		break;
	case SET:
		keySetString (key, compareTo);
		result = TRUE;
		break;
	case AND:
		if (ret == 0 && !strcmp (leftSide, "'1'")) result = TRUE;
		break;
	case OR:
		if (!strcmp (leftSide, "'1'") || !strcmp (rightSide, "'1'")) result = TRUE;
		break;
	default:
		result = ERROR;
		break;
	}
// freeing allocated heap
Cleanup:
	if (lookupName) elektraFree (lookupName);
	if (compareTo) elektraFree (compareTo);
	return result;
}


static char * condition2cmpOp (const char * condition, Comparator * cmpOp)
{
	char * opStr;
	if ((opStr = strstr (condition, "==")))
	{
		*cmpOp = EQU;
	}
	else if ((opStr = strstr (condition, "!=")))
	{
		*cmpOp = NOT;
	}
	else if ((opStr = strstr (condition, "<=")))
	{
		*cmpOp = LE;
	}
	else if ((opStr = strstr (condition, "<")))
	{
		*cmpOp = LT;
	}
	else if ((opStr = strstr (condition, ">=")))
	{
		*cmpOp = GE;
	}
	else if ((opStr = strstr (condition, ">")))
	{
		*cmpOp = GT;
	}
	else if ((opStr = strstr (condition, ":=")))
	{
		*cmpOp = SET;
	}
	else if ((opStr = strstr (condition, "&&")))
	{
		*cmpOp = AND;
	}
	else if ((opStr = strstr (condition, "||")))
	{
		*cmpOp = OR;
	}
	else
	{
		char * ptr = (char *)condition;
		while (isspace (*ptr) && *ptr != '!' && *ptr)
		{
			++ptr;
		}
		if (*ptr != '!')
		{
			return NULL;
		}
		else
		{
			opStr = ptr + strlen (condition) + 1;
			*cmpOp = NEX;
		}
	}
	return opStr;
}

static CondResult parseSingleCondition (const Key * key, const char * condition, const Key * suffixList, KeySet * ks, Key * parentKey)
{
	Comparator cmpOp;
	char * opStr;
	opStr = condition2cmpOp (condition, &cmpOp);

	if (!opStr)
	{
		return ERROR;
	}

	int opLen;
	if (cmpOp == LT || cmpOp == GT || cmpOp == NEX)
	{
		opLen = 1;
	}
	else
	{
		opLen = 2;
	}
	unsigned long startPos = 0;
	unsigned long endPos = 0;
	char * ptr = (char *)condition;
	int firstNot = 1;
	if (*ptr == '!')
	{
		++ptr;
		++startPos;
		firstNot = 0;
	}
	while (isspace (*ptr))
	{
		++ptr;
		if ((cmpOp == NEX) && (*ptr == '!') && firstNot)
		{
			firstNot = 0;
			++ptr;
			++startPos;
		}
		++startPos;
	}

	ptr = opStr - 1;
	while (ptr > condition && isspace (*ptr))
	{
		--ptr;
		++endPos;
	}
	char * leftSide = NULL;
	char * rightSide = NULL;
	int len = opStr - condition - endPos - startPos + 2;
	leftSide = elektraMalloc (len);
	strncpy (leftSide, condition + startPos, len - 2);
	leftSide[len - 2] = '\0';
	startPos = 0;
	endPos = 0;
	if (cmpOp == NEX)
	{
		goto parseSingleEnd;
	}
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
	len = elektraStrLen (condition) - (opStr - condition) - opLen - endPos - startPos;
	rightSide = elektraMalloc (len);
	strncpy (rightSide, opStr + opLen + startPos, len - 1);
	rightSide[len - 1] = '\0';
	CondResult ret;

parseSingleEnd:
	ret = evalCondition (key, leftSide, cmpOp, rightSide, condition, suffixList, ks, parentKey);
	if (rightSide) elektraFree (rightSide);
	elektraFree (leftSide);
	return ret;
}

static const char * isAssign (Key * key, char * expr, Key * parentKey, KeySet * ks)
{
	char * firstPtr = expr + 1;
	char * lastPtr = expr + elektraStrLen (expr) - 3;
	while (isspace (*firstPtr))
		++firstPtr;
	while (isspace (*lastPtr))
		--lastPtr;
	if (*firstPtr != '\'' || *lastPtr != '\'')
	{
		if (lastPtr <= firstPtr)
		{
			ELEKTRA_SET_ERRORF (134, parentKey,
					    "Invalid syntax: \"%s\". Check kdb info conditionals for additional information", expr);
			return NULL;
		}
		*(lastPtr + 1) = '\0';
		Key * lookupKey;
		if (*firstPtr == '@')
		{
			lookupKey = keyDup (parentKey);
			++firstPtr;
			keyAddName (lookupKey, firstPtr);
		}
		else if (!strncmp (firstPtr, "..", 2) || !strncmp (firstPtr, ".", 1))
		{
			lookupKey = keyDup (key);
			keyAddName (lookupKey, firstPtr);
		}
		else
		{
			lookupKey = keyNew (firstPtr, KEY_END);
		}
		Key * assign = ksLookup (ks, lookupKey, KDB_O_NONE);
		if (!assign)
		{
			ELEKTRA_SET_ERRORF (133, parentKey, "Key %s not found", keyName (lookupKey));
			keyDel (lookupKey);
			return NULL;
		}
		else
		{
			keyDel (lookupKey);
			return keyString (assign);
		}
	}
	else
	{
		if (firstPtr == lastPtr) // only one quote in the assign string, invalid syntax
		{
			ELEKTRA_SET_ERRORF (134, parentKey,
					    "Invalid syntax: \"%s\". Check kdb info conditionals for additional information", expr);
			return NULL;
		}
		char * nextMark = strchr (firstPtr + 1, '\'');
		if (nextMark != lastPtr) // more than two quotes, invalid syntax too
		{
			ELEKTRA_SET_ERRORF (134, parentKey,
					    "Invalid syntax: \"%s\". Check kdb info conditionals for additional information", expr);
			return NULL;
		}
		*lastPtr = '\0';
		*firstPtr = '\0';
		++firstPtr;
		return firstPtr;
	}
}

static CondResult parseCondition (Key * key, const char * condition, const Key * suffixList, KeySet * ks, Key * parentKey)
{
	CondResult result = FALSE;
	const char * regexString = "((\\(([^\\(\\)]*)\\)))";
	regex_t regex;

	if ((regcomp (&regex, regexString, REG_EXTENDED | REG_NEWLINE)))
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Couldn't compile regex: most likely out of memory"); // the regex compiles so the only
		// possible error would be out of
		// memory
		ksDel (ks);
		return ERROR;
	}

	char * localCondition = strdup (condition);
	int subMatches = 4;
	regmatch_t m[subMatches];
	char * ptr = localCondition;
	while (1)
	{
		int nomatch = regexec (&regex, ptr, subMatches, m, 0);
		if (nomatch)
		{
			break;
		}
		if (m[3].rm_so == -1)
		{
			result = -1;
			break;
		}
		int startPos;
		int endPos;
		startPos = m[3].rm_so + (ptr - localCondition);
		endPos = m[3].rm_eo + (ptr - localCondition);
		char * singleCondition = elektraMalloc (endPos - startPos + 1);
		strncpy (singleCondition, localCondition + startPos, endPos - startPos);
		singleCondition[endPos - startPos] = '\0';
		result = parseSingleCondition (key, singleCondition, suffixList, ks, parentKey);
		for (int i = startPos - 1; i < endPos + 1; ++i)
			localCondition[i] = ' ';
		localCondition[startPos - 1] = '\'';
		localCondition[startPos] = (result == TRUE) ? '1' : '0';
		localCondition[startPos + 1] = '\'';
		elektraFree (singleCondition);
	}
	elektraFree (localCondition);
	regfree (&regex);
	return result;
}


static CondResult parseConditionString (const Key * meta, const Key * suffixList, Key * parentKey, Key * key, KeySet * ks, Operation op)
{
	const char * conditionString = keyString (meta);
	const char * regexString = "((\\(((.*)?)\\))[[:space:]]*\\?[[:space:]]*(\\(((.*)?)\\)))($|([[:space:]]*:[[:space:]]*(\\((.*)\\))))";

	regex_t regex;
	CondResult ret;
	if ((ret = regcomp (&regex, regexString, REGEX_FLAGS_CONDITION)))
	{
		ELEKTRA_SET_ERROR (87, parentKey, "Couldn't compile regex: most likely out of memory"); // the regex compiles so the only
		// possible error would be out of
		// memory
		ksDel (ks);
		return ERROR;
	}
	int subMatches = 12;
	regmatch_t m[subMatches];
	char * ptr = (char *)conditionString;
	int nomatch = regexec (&regex, ptr, subMatches, m, 0);
	if (nomatch)
	{
		ELEKTRA_SET_ERRORF (134, parentKey, "Invalid syntax: \"%s\". Check kdb info conditionals for additional information",
				    conditionString);
		regfree (&regex);
		ksDel (ks);
		return ERROR;
	}
	if (m[2].rm_so == -1 || m[6].rm_so == -1)
	{
		ELEKTRA_SET_ERRORF (134, parentKey, "Invalid syntax: \"%s\". Check kdb info conditionals for additional information",
				    conditionString);
		regfree (&regex);
		ksDel (ks);
		return ERROR;
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

	if (m[10].rm_so != -1)
	{
		startPos = m[10].rm_so + (ptr - conditionString);
		endPos = m[10].rm_eo + (ptr - conditionString);
		elseexpr = elektraMalloc (endPos - startPos + 1);
		strncpy (elseexpr, conditionString + startPos, endPos - startPos);
		elseexpr[endPos - startPos] = '\0';
	}


	ret = parseCondition (key, condition, suffixList, ks, parentKey);
	if (ret == TRUE)
	{
		if (op == ASSIGN)
		{
			const char * assign = isAssign (key, thenexpr, parentKey, ks);
			if (assign != NULL)
			{
				keySetString (key, assign);
				ret = TRUE;
				goto CleanUp;
			}
			else
			{
				ret = ERROR;
				goto CleanUp;
			}
		}
		else
		{
			ret = parseCondition (key, thenexpr, suffixList, ks, parentKey);
			if (ret == FALSE)
			{
				ELEKTRA_SET_ERRORF (135, parentKey, "Validation of Key %s: %s failed. (%s failed)",
						    keyName (key) + strlen (keyName (parentKey)) + 1, conditionString, thenexpr);
			}
			else if (ret == ERROR)
			{
				ELEKTRA_SET_ERRORF (134, parentKey,
						    "Invalid syntax: \"%s\". Check kdb info conditionals for additional information",
						    thenexpr);
			}
		}
	}
	else if (ret == FALSE)
	{
		if (elseexpr)
		{
			if (op == ASSIGN)
			{
				const char * assign = isAssign (key, elseexpr, parentKey, ks);
				if (assign != NULL)
				{
					keySetString (key, assign);
					ret = TRUE;
					goto CleanUp;
				}
				else
				{
					ret = ERROR;
					goto CleanUp;
				}
			}
			else
			{
				ret = parseCondition (key, elseexpr, suffixList, ks, parentKey);
				if (ret == FALSE)
				{
					ELEKTRA_SET_ERRORF (135, parentKey, "Validation of Key %s: %s failed. (%s failed)",
							    keyName (key) + strlen (keyName (parentKey)) + 1, conditionString, elseexpr);
				}
				else if (ret == ERROR)
				{
					ELEKTRA_SET_ERRORF (
						134, parentKey,
						"Invalid syntax: \"%s\". Check kdb info conditionals for additional information", elseexpr);
				}
			}
		}
		else
		{
			ret = NOEXPR;
		}
	}
	else if (ret == ERROR)
	{
		ELEKTRA_SET_ERRORF (134, parentKey, "Invalid syntax: \"%s\". Check kdb info conditionals for additional information",
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

static CondResult evaluateKey (const Key * meta, const Key * suffixList, Key * parentKey, Key * key, KeySet * ks, Operation op)
{
	CondResult result;
	result = parseConditionString (meta, suffixList, parentKey, key, ksDup (ks), op);
	if (result == ERROR)
	{
		return ERROR;
	}
	else if (result == FALSE && op != ASSIGN)
	{
		return ERROR;
	}
	else if (result == TRUE && op != ASSIGN)
	{
		return TRUE;
	}
	else if (result != ERROR && op == ASSIGN)
	{
		return TRUE;
	}
	else if (result == NOEXPR)
	{
		return NOEXPR;
	}
	return TRUE;
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
	ksRewind (returned);
	CondResult ret = FALSE;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyGetNamespace (cur) == KEY_NS_SPEC) continue;
		Key * conditionMeta = (Key *)keyGetMeta (cur, "check/condition");
		Key * assignMeta = (Key *)keyGetMeta (cur, "assign/condition");
		Key * suffixList = (Key *)keyGetMeta (cur, "condition/validsuffix");
		if (conditionMeta)
		{
			CondResult result;
			result = evaluateKey (conditionMeta, suffixList, parentKey, cur, returned, CONDITION);
			if (result == NOEXPR)
			{
				ret |= TRUE;
			}
			else
			{
				ret |= result;
			}
		}
		if (assignMeta)
		{
			ret |= evaluateKey (assignMeta, suffixList, parentKey, cur, returned, ASSIGN);
		}
	}
	if (ret == TRUE) keySetMeta (parentKey, "error", 0);
	return ret; /* success */
}


int elektraConditionalsSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	Key * cur;
	ksRewind (returned);
	CondResult ret = FALSE;
	while ((cur = ksNext (returned)) != NULL)
	{
		if (keyGetNamespace (cur) == KEY_NS_SPEC) continue;
		Key * conditionMeta = (Key *)keyGetMeta (cur, "check/condition");
		Key * assignMeta = (Key *)keyGetMeta (cur, "assign/condition");
		Key * suffixList = (Key *)keyGetMeta (cur, "condition/validsuffix");
		if (conditionMeta)
		{
			CondResult result;
			result = evaluateKey (conditionMeta, suffixList, parentKey, cur, returned, CONDITION);
			if (result == NOEXPR)
			{
				ret |= TRUE;
			}
			else
			{
				ret |= result;
			}
		}
		if (assignMeta)
		{
			ret |= evaluateKey (assignMeta, suffixList, parentKey, cur, returned, ASSIGN);
		}
	}
	if (ret == TRUE) keySetMeta (parentKey, "error", 0);
	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (conditionals)
{
	// clang-format off
	return elektraPluginExport ("conditionals",
					ELEKTRA_PLUGIN_GET, &elektraConditionalsGet,
					ELEKTRA_PLUGIN_SET, &elektraConditionalsSet,
					ELEKTRA_PLUGIN_END);
}
