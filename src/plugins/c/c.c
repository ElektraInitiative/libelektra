/**
 * @file
 *
 * @brief Source for c plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "c.h"

#include <kdberrors.h>
#include <kdbhelper.h>

#include <ctype.h>
#include <stdio.h>

static const char * toEscape = "\"'\\?\n\r\t";
static const char * escapes = "\"'\\?nrt";

static const char * hex = "0123456789abcdef";

static char * escapeString (char ** str)
{
	size_t size = 0;
	for (char * cur = *str; *cur != '\0'; ++cur)
	{
		unsigned char c = *cur;

		if (strchr (toEscape, c) != NULL)
		{
			size += 2;
		}
		else if (isprint (c))
		{
			++size;
		}
		else
		{
			size += 4;
		}
	}

	if (size == 0)
	{
		return *str;
	}

	char * newStr = elektraMalloc (size + 1);
	char * newCur = newStr;
	for (char * cur = *str; *cur != '\0'; ++cur)
	{
		unsigned char c = *cur;
		char * e = strchr (toEscape, c);

		if (e != NULL)
		{
			char escaped = escapes[e - toEscape];
			*newCur = '\\';
			++newCur;
			*newCur = escaped;
			++newCur;
		}
		else if (isprint (c))
		{
			*newCur = c;
			++newCur;
		}
		else
		{
			*newCur = '\\';
			++newCur;
			*newCur = 'x';
			++newCur;
			*newCur = hex[c >> 4u];
			++newCur;
			*newCur = hex[c & 0xFu];
			++newCur;
		}
	}
	*newCur = '\0';
	elektraFree (*str);
	*str = newStr;
	return newStr;
}

/**
 * Generate a C-Style key and stream it.
 *
 * This keyset can be used to include as c-code for
 * applikations using elektra.
 *
 * @param key the key object to work with
 * @param stream the file pointer where to send the stream
 * @retval 1 on success
 * @ingroup stream
 */
int keyGenerate (const ElektraKey * key, FILE * stream)
{
	size_t n = elektraKeyGetNameSize (key);
	if (n > 1)
	{
		char * nam = (char *) elektraMalloc (n);
		if (nam == NULL) return -1;
		elektraKeyGetName (key, nam, n);
		fprintf (stream, "\tkeyNew (\"%s\"", escapeString (&nam));
		elektraFree (nam);
	}
	else if (n == 1)
	{
		fprintf (stream, "\tkeyNew(\"\"");
	}

	size_t s = elektraKeyGetValueSize (key);
	if (s > 1)
	{
		char * str = (char *) elektraMalloc (s);
		if (str == NULL) return -1;
		if (elektraKeyIsBinary (key))
		{
			elektraKeyGetBinary (key, str, s);
			fprintf (stream, ", KEY_SIZE, \"%zd\"", elektraKeyGetValueSize (key));
		}
		else
		{
			elektraKeyGetString (key, str, s);
		}
		fprintf (stream, ", KEY_VALUE, \"%s\"", escapeString (&str));
		elektraFree (str);
	}

	const ElektraKey * meta;
	ElektraKey * dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
	elektraKeyRewindMeta (dup);
	while ((meta = elektraKeyNextMeta (dup)))
	{
		char * metaName = elektraStrDup (elektraKeyName (meta) + sizeof ("meta:/") - 1);
		char * metaStr = elektraStrDup (elektraKeyString (meta));
		fprintf (stream, ", KEY_META, \"%s\", \"%s\"", escapeString (&metaName), escapeString (&metaStr));
		elektraFree (metaName);
		elektraFree (metaStr);
	}
	elektraKeyDel (dup);

	fprintf (stream, ", KEY_END)");
	return 1;
}


/**
 * Generate a C-Style keyset and stream it.
 *
 * This keyset can be used to include as c-code for
 * applikations using elektra.
 *
 * @param ks the keyset to work with
 * @param stream the file pointer where to send the stream
 * @retval 1 on success
 * @ingroup stream
 */
int ksGenerate (const ElektraKeyset * ks, FILE * stream)
{
	ElektraKey * key;
	ElektraKeyset * cks = elektraKeysetDup (ks);

	elektraKeysetRewind (cks);

	fprintf (stream, "ksNew (%d,\n", (int) elektraKeysetGetSize (cks));
	while ((key = elektraKeysetNext (cks)) != 0)
	{
		keyGenerate (key, stream);
		fprintf (stream, ",\n");
	}
	fprintf (stream, "\tKS_END);\n");

	elektraKeysetDel (cks);
	return 1;
}

int elektraCGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/c"))
	{
		ElektraKeyset * contract = elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/c", ELEKTRA_KEY_VALUE, "c plugin waits for your orders", ELEKTRA_KEY_END),
					   elektraKeyNew ("system:/elektra/modules/c/exports", ELEKTRA_KEY_END),
					   elektraKeyNew ("system:/elektra/modules/c/exports/get", ELEKTRA_KEY_FUNC, elektraCGet, ELEKTRA_KEY_END),
					   elektraKeyNew ("system:/elektra/modules/c/exports/set", ELEKTRA_KEY_FUNC, elektraCSet, ELEKTRA_KEY_END),
					   elektraKeyNew ("system:/elektra/modules/c/exports/checkconf", ELEKTRA_KEY_FUNC, elektraCCheckConf, ELEKTRA_KEY_END),
#include ELEKTRA_README
					   elektraKeyNew ("system:/elektra/modules/c/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraCSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	FILE * fp = fopen (elektraKeyString (parentKey), "w");

	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	ksGenerate (returned, fp);

	fclose (fp);
	return 1; // success
}

int elektraCCheckConf (ElektraKey * errorKey ELEKTRA_UNUSED, ElektraKeyset * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("c",
		ELEKTRA_PLUGIN_GET,	&elektraCGet,
		ELEKTRA_PLUGIN_SET,	&elektraCSet,
		ELEKTRA_PLUGIN_END);
}

