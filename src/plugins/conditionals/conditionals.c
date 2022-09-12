/**
 * @file
 *
 * @brief Source for conditionals plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <ctype.h>
#include <errno.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbmeta.h>
#include <math.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "conditionals.h"

#define EPSILON 0.00001

#define REGEX_FLAGS_CONDITION (REG_EXTENDED)

typedef enum
{
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

typedef enum
{
	CONDITION,
	ASSIGN
} Operation;

typedef enum
{
	TRUE = 1,
	FALSE = 0,
	ERROR = -1,
	NOEXPR = -3,
} CondResult;

static int isValidSuffix (char * suffix, const ElektraKey * suffixList)
{
	if (!suffixList) return 0;
	char * searchString = elektraMalloc (strlen (suffix) + 3);
	snprintf (searchString, strlen (suffix) + 3, "'%s'", suffix);
	int ret = 0;
	if (strstr (elektraKeyString (suffixList), searchString))
	{
		ret = 1;
	}
	elektraFree (searchString);
	return ret;
}

static int isNumber (const char * s, const ElektraKey * suffixList)
{
	char * endPtr = NULL;
	int ret;
	ret = (int) strtol (s, &endPtr, 10);
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
		ret = (int) strtof (s, &endPtr);
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
static int compareStrings (const char * s1, const char * s2, const ElektraKey * suffixList)
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
				result = (float) fabs (s1Value - s2Value);
				if (result < EPSILON)
				{
					retval = 0;
				}
				else
				{
					retval = 1;
				}
			}
			else
			{
				retval = strcmp (s1, s2);
			}
		}
		else
		{
			int s1Value = (int) strtol (s1, &s1EndPtr, 10);
			int s2Value = (int) strtol (s2, &s2EndPtr, 10);
			if (!strcmp (s1EndPtr, s2EndPtr) || *s1EndPtr == 0 || *s2EndPtr == 0)
			{
				retval = (int) (s1Value - s2Value);
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

static CondResult evalCondition (const ElektraKey * curKey, const char * leftSide, Comparator cmpOp, const char * rightSide,
				 const char * condition, const ElektraKey * suffixList, ElektraKeyset * ks, ElektraKey * parentKey)
{
	char * lookupName = NULL;
	char * compareTo = NULL;
	ElektraKey * key;
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
			if (elektraRealloc ((void **) &compareTo, (size_t) (endPos - rightSide)) < 0)
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
				result = ERROR;
				goto Cleanup;
			}
			memset (compareTo, 0, (size_t) (endPos - rightSide));
			strncat (compareTo, rightSide + 1, (size_t) (endPos - rightSide - 1));
		}
		else if (rightSide && elektraStrLen (rightSide) > 1)
		{
			// not a literal, it has to be a key
			if (rightSide[0] == '@')
				len = (int) ((size_t) elektraKeyGetNameSize (parentKey) + elektraStrLen (rightSide));
			else if (!strncmp (rightSide, "..", 2) || (rightSide[0] == '.'))
				len = (int) ((size_t) elektraKeyGetNameSize (curKey) + elektraStrLen (rightSide));
			else
				len = (int) elektraStrLen (rightSide);

			if (elektraRealloc ((void **) &lookupName, (size_t) len) < 0)
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
				result = ERROR;
				goto Cleanup;
			}
			if (rightSide[0] == '@')
				snprintf (lookupName, (size_t) len, "%s/%s", elektraKeyName (parentKey), rightSide + 1);
			else if (rightSide[0] == '.') // either starts with . or .., doesn't matter at this point
				snprintf (lookupName, (size_t) len, "%s/%s", elektraKeyName (curKey), rightSide);
			else
				snprintf (lookupName, (size_t) len, "%s", rightSide);

			key = elektraKeysetLookupByName (ks, lookupName, 0);
			if (!key)
			{
				if (!elektraKeyGetMeta (parentKey, "error"))
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey,
										"Key %s not found but is required for the evaluation of %s",
										lookupName, condition);
				}
				result = FALSE;
				goto Cleanup;
			}
			if (elektraRealloc ((void **) &compareTo, (size_t) elektraKeyGetValueSize (key)) < 0)
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
				result = ERROR;
				goto Cleanup;
			}
			strcpy (compareTo, elektraKeyString (key));
		}
	}
	if (leftSide[0] == '@')
		len = (int) ((size_t) elektraKeyGetNameSize (parentKey) + elektraStrLen (leftSide));
	else if (!strncmp (leftSide, "..", 2) || (leftSide[0] == '.'))
		len = (int) ((size_t) elektraKeyGetNameSize (curKey) + elektraStrLen (leftSide));
	else
		len = (int) elektraStrLen (leftSide);

	if (elektraRealloc ((void **) &lookupName, (size_t) len) < 0)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		result = ERROR;
		goto Cleanup;
	}
	if (leftSide[0] == '@')
		snprintf (lookupName, (size_t) len, "%s/%s", elektraKeyName (parentKey), leftSide + 1);
	else if (leftSide[0] == '.') // either . or .., doesn't matter here
		snprintf (lookupName, (size_t) len, "%s/%s", elektraKeyName (curKey), leftSide);
	else
		snprintf (lookupName, (size_t) len, "%s", leftSide);
	key = elektraKeysetLookupByName (ks, lookupName, 0);
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
		if (!elektraKeyGetMeta (parentKey, "error"))
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Key %s not found but is required for the evaluation of %s",
								lookupName, condition);
		}
		result = FALSE;
		goto Cleanup;
	}
	long ret;
	if (cmpOp == OR || cmpOp == AND)
		ret = compareStrings (leftSide, rightSide, NULL);
	else
		ret = compareStrings (elektraKeyString (key), compareTo, suffixList);
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
		elektraKeySetString (key, compareTo);
		result = TRUE;
		break;
	case AND:
		if (ret == 0 && !strcmp (leftSide, "'1'")) result = TRUE;
		break;
	case OR:
		if (!strcmp (leftSide, "'1'") || (rightSide && !strcmp (rightSide, "'1'"))) result = TRUE;
		break;
	default:
		result = ERROR;
		break;
	}
// freeing allocated heap
Cleanup:
	if (lookupName) elektraFree (lookupName);
	if (compareTo) elektraFree (compareTo);
	return (CondResult) result;
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
		char * ptr = (char *) condition;
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

static CondResult parseSingleCondition (const ElektraKey * key, const char * condition, const ElektraKey * suffixList, ElektraKeyset * ks, ElektraKey * parentKey)
{
	Comparator cmpOp;
	char * opStr;
	opStr = condition2cmpOp (condition, &cmpOp);

	if (!opStr)
	{
		return ERROR;
	}

	size_t opLen;
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
	char * ptr = (char *) condition;
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
	int len = (int) ((unsigned long) (opStr - condition) - endPos - startPos + 2);
	char * leftSide = elektraMalloc ((size_t) len);
	char * rightSide = NULL;
	strncpy (leftSide, condition + startPos, (size_t) (len - 2));
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
	ptr = (char *) condition + (elektraStrLen (condition) - 2);
	while (isspace (*ptr))
	{
		--ptr;
		++endPos;
	}
	len = (int) (elektraStrLen (condition) - (unsigned long) (opStr - condition) - opLen - endPos - startPos);
	rightSide = elektraMalloc ((size_t) len);
	strncpy (rightSide, opStr + opLen + startPos, (size_t) (len - 1));
	rightSide[len - 1] = '\0';
	CondResult ret;

parseSingleEnd:
	ret = evalCondition (key, leftSide, cmpOp, rightSide, condition, suffixList, ks, parentKey);
	if (rightSide) elektraFree (rightSide);
	elektraFree (leftSide);
	return ret;
}

static const char * isAssign (ElektraKey * key, char * expr, ElektraKey * parentKey, ElektraKeyset * ks)
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
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", expr);
			return NULL;
		}
		*(lastPtr + 1) = '\0';
		ElektraKey * lookupKey;
		if (*firstPtr == '@')
		{
			lookupKey = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
			++firstPtr;
			elektraKeyAddName (lookupKey, firstPtr);
		}
		else if (!strncmp (firstPtr, "..", 2) || !strncmp (firstPtr, ".", 1))
		{
			lookupKey = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddName (lookupKey, firstPtr);
		}
		else
		{
			lookupKey = elektraKeyNew (firstPtr, ELEKTRA_KEY_END);
		}
		ElektraKey * assign = elektraKeysetLookup (ks, lookupKey, ELEKTRA_KDB_O_NONE);
		if (!assign)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Key %s not found", elektraKeyName (lookupKey));
			elektraKeyDel (lookupKey);
			return NULL;
		}
		else
		{
			elektraKeyDel (lookupKey);
			return elektraKeyString (assign);
		}
	}
	else
	{
		if (firstPtr == lastPtr) // only one quote in the assign string, invalid syntax
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", expr);
			return NULL;
		}
		char * nextMark = strchr (firstPtr + 1, '\'');
		if (nextMark != lastPtr) // more than two quotes, invalid syntax too
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", expr);
			return NULL;
		}
		*lastPtr = '\0';
		*firstPtr = '\0';
		++firstPtr;
		return firstPtr;
	}
}

static CondResult parseCondition (ElektraKey * key, const char * condition, const ElektraKey * suffixList, ElektraKeyset * ks, ElektraKey * parentKey)
{
	CondResult result = FALSE;
	const char * regexString = "((\\(([^\\(\\)]*)\\)))";
	regex_t regex;

	if ((regcomp (&regex, regexString, REG_EXTENDED | REG_NEWLINE)))
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey); // the regex compiles so the only
		// possible error would be out of
		// memory
		elektraKeysetDel (ks);
		return ERROR;
	}

	char * localCondition = elektraStrDup (condition);
	size_t subMatches = 4;
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
		startPos = (int) (m[3].rm_so + (ptr - localCondition));
		endPos = (int) (m[3].rm_eo + (ptr - localCondition));
		char * singleCondition = elektraMalloc ((size_t) (endPos - startPos + 1));
		strncpy (singleCondition, localCondition + startPos, (size_t) (endPos - startPos));
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


static CondResult parseConditionString (const ElektraKey * meta, const ElektraKey * suffixList, ElektraKey * parentKey, ElektraKey * key, ElektraKeyset * ks, Operation op)
{
	const char * conditionString = elektraKeyString (meta);
	const char * regexString1 = "(\\(((.*)?)\\))[[:space:]]*\\?";
	const char * regexString2 = "\\?[[:space:]]*(\\(((.*)?)\\))";
	const char * regexString3 = "[[:space:]]*:[[:space:]]*(\\(((.*)?)\\))";
	regex_t regex1, regex2, regex3;
	CondResult ret;
	if ((ret = regcomp (&regex1, regexString1, REGEX_FLAGS_CONDITION)))
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey); // the regex compiles so the only
		// possible error would be out of
		// memory
		elektraKeysetDel (ks);
		return ERROR;
	}
	if ((ret = regcomp (&regex2, regexString2, REGEX_FLAGS_CONDITION)))
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey); // the regex compiles so the only
		// possible error would be out of
		// memory
		regfree (&regex1);
		elektraKeysetDel (ks);
		return ERROR;
	}
	if ((ret = regcomp (&regex3, regexString3, REGEX_FLAGS_CONDITION)))
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey); // the regex compiles so the only
		// possible error would be out of
		// memory
		regfree (&regex1);
		regfree (&regex2);
		elektraKeysetDel (ks);
		return ERROR;
	}
	size_t subMatches = 6;
	regmatch_t m[subMatches];
	int nomatch = regexec (&regex1, conditionString, subMatches, m, 0);
	if (nomatch)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
			parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", conditionString);
		regfree (&regex1);
		regfree (&regex2);
		regfree (&regex3);
		elektraKeysetDel (ks);
		return ERROR;
	}
	if (m[1].rm_so == -1)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
			parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", conditionString);
		regfree (&regex1);
		regfree (&regex2);
		regfree (&regex3);
		elektraKeysetDel (ks);
		return ERROR;
	}
	int startPos = (int) m[1].rm_so;
	int endPos = (int) m[1].rm_eo;
	char * condition = elektraMalloc ((size_t) (endPos - startPos + 1));
	char * thenexpr = NULL;
	char * elseexpr = NULL;
	strncpy (condition, conditionString + startPos, (size_t) (endPos - startPos));
	condition[endPos - startPos] = '\0';
	nomatch = regexec (&regex2, conditionString, subMatches, m, 0);
	if (nomatch)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
			parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", conditionString);
		regfree (&regex1);
		regfree (&regex2);
		regfree (&regex3);
		elektraKeysetDel (ks);
		return ERROR;
	}
	if (m[1].rm_so == -1)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
			parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", conditionString);
		regfree (&regex1);
		regfree (&regex2);
		regfree (&regex3);
		elektraKeysetDel (ks);
		return ERROR;
	}

	startPos = (int) m[1].rm_so;
	endPos = (int) m[1].rm_eo;
	thenexpr = elektraMalloc ((size_t) (endPos - startPos + 1));
	strncpy (thenexpr, conditionString + startPos, (size_t) (endPos - startPos));
	thenexpr[endPos - startPos] = '\0';

	nomatch = regexec (&regex3, conditionString, subMatches, m, 0);
	if (!nomatch)
	{
		if (m[1].rm_so == -1)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
				parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information",
				conditionString);
			regfree (&regex1);
			regfree (&regex2);
			regfree (&regex3);
			elektraKeysetDel (ks);
			return ERROR;
		}
		thenexpr[strlen (thenexpr) - (size_t) ((m[0].rm_eo - m[0].rm_so))] = '\0';
		startPos = (int) m[1].rm_so;
		endPos = (int) m[1].rm_eo;
		elseexpr = elektraMalloc ((size_t) (endPos - startPos + 1));
		strncpy (elseexpr, conditionString + startPos, (size_t) (endPos - startPos));
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
				elektraKeySetString (key, assign);
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
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Validation of Key %s: %s failed. (%s failed)",
									elektraKeyName (key) + strlen (elektraKeyName (parentKey)) + 1, conditionString,
									thenexpr);
			}
			else if (ret == ERROR)
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information",
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
					elektraKeySetString (key, assign);
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
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (parentKey, "Validation of Key %s: %s failed. (%s failed)",
										elektraKeyName (key) + strlen (elektraKeyName (parentKey)) + 1,
										conditionString, elseexpr);
				}
				else if (ret == ERROR)
				{
					ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
						parentKey,
						"Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information",
						elseexpr);
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
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
			parentKey, "Invalid syntax: '%s'. Check kdb plugin-info conditionals for additional information", condition);
	}

CleanUp:
	elektraFree (condition);
	elektraFree (thenexpr);
	if (elseexpr) elektraFree (elseexpr);
	regfree (&regex1);
	regfree (&regex2);
	regfree (&regex3);
	elektraKeysetDel (ks);
	return ret;
}

static CondResult evaluateKey (const ElektraKey * meta, const ElektraKey * suffixList, ElektraKey * parentKey, ElektraKey * key, ElektraKeyset * ks, Operation op)
{
	CondResult result;
	result = parseConditionString (meta, suffixList, parentKey, key, elektraKeysetDup (ks), op);
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
	else if (result == NOEXPR)
	{
		return NOEXPR;
	}
	return TRUE;
}

static CondResult evalMultipleConditions (ElektraKey * key, const ElektraKey * meta, const ElektraKey * suffixList, ElektraKey * parentKey, ElektraKeyset * returned)
{
	int countSucceeded = 0;
	int countFailed = 0;
	int countNoexpr = 0;
	ElektraKeyset * condKS = elektraMetaArrayToKS (key, elektraKeyName (meta));
	ElektraKey * c;
	CondResult result = FALSE;
	while ((c = elektraKeysetNext (condKS)) != NULL)
	{
		if (!elektraKeyCmp (c, meta)) continue;
		result = evaluateKey (c, suffixList, parentKey, key, returned, CONDITION);
		if (result == TRUE)
			++countSucceeded;
		else if (result == ERROR)
			++countFailed;
		else if (result == NOEXPR)
			++countNoexpr;
	}
	elektraKeysetDel (condKS);
	if (!strcmp (elektraKeyBaseName (meta), "all"))
	{
		// all conditions must evaluate to TRUE
		if (countFailed || countNoexpr)
			return ERROR;
		else
			return TRUE;
	}
	else if (!strcmp (elektraKeyBaseName (meta), "any"))
	{
		// at least one conditional must evaluate to TRUE
		if (countSucceeded)
			return TRUE;
		else
			return ERROR;
	}
	else
	{
		// no condition must evaluate to FALSE
		if (countFailed)
			return ERROR;
		else
			return TRUE;
	}
}

int elektraConditionalsGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/conditionals"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30,
			elektraKeyNew ("system:/elektra/modules/conditionals", ELEKTRA_KEY_VALUE, "conditionals plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/conditionals/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/conditionals/exports/get", ELEKTRA_KEY_FUNC, elektraConditionalsGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/conditionals/exports/set", ELEKTRA_KEY_FUNC, elektraConditionalsSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/conditionals/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; /* success */
	}
	ElektraKey * cur;
	elektraKeysetRewind (returned);
	CondResult ret = FALSE;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		ElektraKey * conditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition");
		ElektraKey * assignMeta = (ElektraKey *) elektraKeyGetMeta (cur, "assign/condition");
		ElektraKey * suffixList = (ElektraKey *) elektraKeyGetMeta (cur, "condition/validsuffix");
		ElektraKey * anyConditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition/any");
		ElektraKey * allConditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition/all");
		ElektraKey * noneConditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition/none");

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
		else if (allConditionMeta)
		{
			CondResult result;
			result = evalMultipleConditions (cur, allConditionMeta, suffixList, parentKey, returned);
			ret |= result;
		}
		else if (anyConditionMeta)
		{
			CondResult result;
			result = evalMultipleConditions (cur, anyConditionMeta, suffixList, parentKey, returned);
			ret |= result;
		}
		else if (noneConditionMeta)
		{
			CondResult result;
			result = evalMultipleConditions (cur, noneConditionMeta, suffixList, parentKey, returned);
			ret |= result;
		}

		if (assignMeta)
		{
			if (elektraKeyString (assignMeta)[0] == '#')
			{
				ElektraKeyset * assignKS = elektraMetaArrayToKS (cur, "assign/condition");
				ElektraKey * a;
				while ((a = elektraKeysetNext (assignKS)) != NULL)
				{
					if (elektraKeyCmp (a, assignMeta) == 0) continue;
					CondResult result = evaluateKey (a, suffixList, parentKey, cur, returned, ASSIGN);
					if (result == TRUE)
					{
						ret |= TRUE;
						break;
					}
					else if (result == NOEXPR)
					{
						ret |= TRUE;
					}
					else
					{
						ret |= ERROR;
					}
				}
				elektraKeysetDel (assignKS);
			}
			else
			{
				ret |= evaluateKey (assignMeta, suffixList, parentKey, cur, returned, ASSIGN);
			}
		}
	}
	if (ret == TRUE) elektraKeySetMeta (parentKey, "error", 0);
	return ret; /* success */
}


int elektraConditionalsSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	ElektraKey * cur;
	elektraKeysetRewind (returned);
	CondResult ret = FALSE;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		ElektraKey * conditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition");
		ElektraKey * assignMeta = (ElektraKey *) elektraKeyGetMeta (cur, "assign/condition");
		ElektraKey * suffixList = (ElektraKey *) elektraKeyGetMeta (cur, "condition/validsuffix");
		ElektraKey * anyConditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition/any");
		ElektraKey * allConditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition/all");
		ElektraKey * noneConditionMeta = (ElektraKey *) elektraKeyGetMeta (cur, "check/condition/none");

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
		else if (allConditionMeta)
		{
			CondResult result;
			result = evalMultipleConditions (cur, allConditionMeta, suffixList, parentKey, returned);
			ret |= result;
		}
		else if (anyConditionMeta)
		{
			CondResult result;
			result = evalMultipleConditions (cur, anyConditionMeta, suffixList, parentKey, returned);
			ret |= result;
		}
		else if (noneConditionMeta)
		{
			CondResult result;
			result = evalMultipleConditions (cur, noneConditionMeta, suffixList, parentKey, returned);
			ret |= result;
		}

		if (assignMeta)
		{
			if (elektraKeyString (assignMeta)[0] == '#')
			{
				ElektraKeyset * assignKS = elektraMetaArrayToKS (cur, "assign/condition");
				ElektraKey * a;
				while ((a = elektraKeysetNext (assignKS)) != NULL)
				{
					if (elektraKeyCmp (a, assignMeta) == 0) continue;
					CondResult result = evaluateKey (a, suffixList, parentKey, cur, returned, ASSIGN);
					if (result == TRUE)
					{
						ret |= TRUE;
						break;
					}
					else if (result == NOEXPR)
					{
						ret |= TRUE;
					}
					else
					{
						ret |= ERROR;
					}
				}
				elektraKeysetDel (assignKS);
			}
			else
			{
				ret |= evaluateKey (assignMeta, suffixList, parentKey, cur, returned, ASSIGN);
			}
		}
	}
	if (ret == TRUE) elektraKeySetMeta (parentKey, "error", 0);
	return ret;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("conditionals",
	    ELEKTRA_PLUGIN_GET, &elektraConditionalsGet,
	    ELEKTRA_PLUGIN_SET, &elektraConditionalsSet,
	    ELEKTRA_PLUGIN_END);
}
