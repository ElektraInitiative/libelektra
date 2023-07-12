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

static const char * const toEscape = "\"'\\?\n\r\t";
static const char * const escapes = "\"'\\?nrt";

static const char * const hex = "0123456789abcdef";

/**
 * @internal
 *
 * @brief The function escapes the string @p str and replaces non-printable characters with their HEX values.
 *
 * @param str The string to escape
 *		Please note that the given pointer points to a different memory area after the function was executed.
 *		New memory is allocated for the escaped string and *str points to it then.
 *		The memory for the old string is freed by this function.
 *		You must free the returned string.
 * @return	The escaped string. (the same pointer as @p *str is set to)
 */
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
		{	/* convert to hex value, needs 4 chars */
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
			/* add '\' and escaped character */
			char escaped = escapes[e - toEscape];
			*newCur = '\\';
			++newCur;
			*newCur = escaped;
			++newCur;
		}
		else if (isprint (c))
		{
			/* just copy the printable character */
			*newCur = c;
			++newCur;
		}
		else
		{
			/* convert the character to a HEX value */
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
 *
 * @internal
 *
 * @brief Generate a C-style key and stream it.
 *
 * The result can be included in C-code for applications using Elektra.
 *
 * @param key The key to work with
 * @param stream The file pointer where to send the stream to
 *
 * @retval 1 on success
 * @retval -1 on error
 *
 * @ingroup stream
 */
static int keyGenerate (const Key * key, FILE * stream)
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
	else if (n == 1) /* size 1 because of \0 */
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

	/* Dup key because keyMeta() needs a non-const key */
	Key * dup = keyDup (key, KEY_CP_ALL);
	KeySet * metaKeys = keyMeta (dup);

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		const Key * meta = ksAtCursor (metaKeys, it);

		/* Dup the key-name without "meta:/" prefix (remove namespace) */
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
 * @internal
 *
 * Generate a C-Style KeySet and stream it.
 *
 * The result can be included in C-code for applications using Elektra.
 *
 * @param ks The KeySet to work with
 * @param stream The file pointer where to send the stream to
 *
 * @retval 1 on success
 * @retval -1 on error
 *
 * @ingroup stream
 */
static int ksGenerate (const KeySet * ks, FILE * stream)
{
	fprintf (stream, "ksNew (%d,\n", (int) ksGetSize (ks));

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		Key * key = ksAtCursor (ks, it);
		keyGenerate (key, stream);
		fprintf (stream, ",\n");
	}

	fprintf (stream, "\tKS_END);\n");

	return 1;
}

/**
 * @brief The get function of this plugin is only used to define the contract.
 *
 * @return 1
 */
int elektraCGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/c"))
	{
		KeySet * contract = ksNew (30, keyNew ("system:/elektra/modules/c", KEY_VALUE, "c plugin waits for your orders", KEY_END),
					   keyNew ("system:/elektra/modules/c/exports", KEY_END),
					   keyNew ("system:/elektra/modules/c/exports/get", KEY_FUNC, elektraCGet, KEY_END),
					   keyNew ("system:/elektra/modules/c/exports/set", KEY_FUNC, elektraCSet, KEY_END),
#include ELEKTRA_README
					   keyNew ("system:/elektra/modules/c/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}

	// no action if no contract is requested (write-only plugin)
	return 1; // success
}

/**
 * @brief Generate the C-code and put it in the file whose name is defined as the value of @p parentKey.
 *
 * @post The given KeySet and its keys are converted into C-code that can be used with Elektra
 * and the generated code is written into the file defined by the string-value of parent key.
 * Please note that any existing content in the given file is deleted.
 *
 * @param returned The KeySet for which the C-code should be generated
 * @param parentKey The value of the key is the name of the file where the generated code gets written to
 *
 * @retval 1 on success
 * @retval -1 on error
 */
int elektraCSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
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

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("c",
		ELEKTRA_PLUGIN_GET,	&elektraCGet,
		ELEKTRA_PLUGIN_SET,	&elektraCSet,
		ELEKTRA_PLUGIN_END);
}

