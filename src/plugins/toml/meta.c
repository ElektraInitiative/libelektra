/**
 * @file meta.h
 *
 * @brief Functions for reading and writing metakeys from and into comment strings
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <kdb.h>
#include <kdbassert.h>
#include <kdbhelper.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "error.h"
#include "meta.h"

#define METAKEY_COMMENT_PREFIX "!!ELEKTRA_META"

bool shouldWriteMetakey (const Key * meta)
{
	const char * blackList[] = { "order", "origvalue", "tomltype", NULL };
	for (size_t i = 0; blackList[i] != NULL; i++)
	{
		if (elektraStrCmp (keyName (meta), blackList[i]) == 0)
		{
			return false;
		}
	}
	if (elektraStrNCmp (keyName (meta), "comment/", 8) == 0)
	{
		return false;
	}
	else if (elektraStrCmp (keyName (meta), "type") && elektraStrCmp (keyString (meta), "binary") != 0)
	{
		return false;
	}
	return true;
}

bool isMetakeyComment (const char * comment)
{
	return elektraStrNCmp (comment, METAKEY_COMMENT_PREFIX, sizeof (METAKEY_COMMENT_PREFIX) - 1) == 0;
}

int writeMetakeyAsComment (const Key * meta, FILE * f)
{
	ELEKTRA_ASSERT (shouldWriteMetakey (meta), "Metakey should already have been checked for writing eligibility");
	int result = 0;
	if (meta != NULL && f != NULL)
	{
		result |= fputs ("#" METAKEY_COMMENT_PREFIX " ", f) == EOF;
		result |= fputs (keyName (meta), f) == EOF;
		result |= fputs (" ", f) == EOF;
		result |= fputs (keyString (meta), f) == EOF;
		result |= fputc ('\n', f) == EOF;
	}
	return result;
}

int assignMetakeyFromComment (Key * key, const char * comment)
{
	ELEKTRA_ASSERT (isMetakeyComment (comment), "Comment should already have been checked for metakey eligibility");

	const char * name = comment + sizeof (METAKEY_COMMENT_PREFIX);
	const char * nameEnd = strchr (name, ' ');
	const char * value = nameEnd + 1;
	if (value != NULL)
	{
		char * nameDup = elektraStrNDup (name, nameEnd - name + 1);
		if (nameDup == NULL)
		{
			return ERROR_MEMORY;
		}
		nameDup[nameEnd - name] = 0;

		if (keySetMeta (key, nameDup, value) == -1)
		{
			elektraFree (nameDup);
			return ERROR_INTERNAL;
		}
		elektraFree (nameDup);
	}
	else
	{
		return ERROR_INTERNAL;
	}
	return 0;
}
