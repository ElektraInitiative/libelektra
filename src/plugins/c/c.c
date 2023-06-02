/**
 * @file
 *
 * @brief Source for c plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./c.h"

#include <elektra/core/errors.h>

#include <internal/macros/attributes.h>
#include <internal/macros/plugin_errors.h>
#include <internal/utility/alloc.h>
#include <internal/utility/compare.h>

#include <ctype.h>
#include <stdio.h>

static const char * const toEscape = "\"'\\?\n\r\t";
static const char * const escapes = "\"'\\?nrt";

static const char * const hex = "0123456789abcdef";

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
int keyGenerate (const Key * key, FILE * stream)
{
	size_t n = keyGetNameSize (key);
	if (n > 1)
	{
		char * nam = (char *) elektraMalloc (n);
		if (nam == NULL) return -1;
		keyGetName (key, nam, n);
		fprintf (stream, "\tkeyNew (\"%s\"", escapeString (&nam));
		elektraFree (nam);
	}
	else if (n == 1)
	{
		fprintf (stream, "\tkeyNew(\"\"");
	}

	size_t s = keyGetValueSize (key);
	if (s > 1)
	{
		char * str = (char *) elektraMalloc (s);
		if (str == NULL) return -1;
		if (keyIsBinary (key))
		{
			keyGetBinary (key, str, s);
			fprintf (stream, ", KEY_SIZE, \"%zd\"", keyGetValueSize (key));
		}
		else
		{
			keyGetString (key, str, s);
		}
		fprintf (stream, ", KEY_VALUE, \"%s\"", escapeString (&str));
		elektraFree (str);
	}

	const Key * meta;
	Key * dup = keyDup (key, KEY_CP_ALL);
	KeySet * metaKeys = keyMeta (dup);

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		meta = ksAtCursor (metaKeys, it);
		char * metaName = elektraStrDup (keyName (meta) + sizeof ("meta:/") - 1);
		char * metaStr = elektraStrDup (keyString (meta));
		fprintf (stream, ", KEY_META, \"%s\", \"%s\"", escapeString (&metaName), escapeString (&metaStr));
		elektraFree (metaName);
		elektraFree (metaStr);
	}
	keyDel (dup);

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
int ksGenerate (const KeySet * ks, FILE * stream)
{
	Key * key;
	KeySet * cks = ksDup (ks);

	fprintf (stream, "ksNew (%d,\n", (int) ksGetSize (cks));

	for (elektraCursor it = 0; it < ksGetSize (cks); ++it)
	{
		key = ksAtCursor (cks, it);
		keyGenerate (key, stream);
		fprintf (stream, ",\n");
	}

	fprintf (stream, "\tKS_END);\n");

	ksDel (cks);
	return 1;
}

int elektraCGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/c"))
	{
		KeySet * contract = ksNew (30, keyNew ("system:/elektra/modules/c", KEY_VALUE, "c plugin waits for your orders", KEY_END),
					   keyNew ("system:/elektra/modules/c/exports", KEY_END),
					   keyNew ("system:/elektra/modules/c/exports/get", KEY_FUNC, elektraCGet, KEY_END),
					   keyNew ("system:/elektra/modules/c/exports/set", KEY_FUNC, elektraCSet, KEY_END),
					   keyNew ("system:/elektra/modules/c/exports/checkconf", KEY_FUNC, elektraCCheckConf, KEY_END),
#include ELEKTRA_README
					   keyNew ("system:/elektra/modules/c/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	return 1; // success
}

int elektraCSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	FILE * fp = fopen (keyString (parentKey), "w");

	if (!fp)
	{
		ELEKTRA_SET_ERROR_SET (parentKey);
		return -1;
	}

	ksGenerate (returned, fp);

	fclose (fp);
	return 1; // success
}

int elektraCCheckConf (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
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

