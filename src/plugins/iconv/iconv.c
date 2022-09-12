/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "conv.h"

static inline const char * getFrom (Plugin * handle)
{
	const char * from;
	ElektraKey * k;

	k = elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/from", 0);
	if (!k)
		from = nl_langinfo (CODESET);
	else
		from = elektraKeyString (k);

	return from;
}

static inline const char * getTo (Plugin * handle)
{
	const char * to;
	ElektraKey * k;

	k = elektraKeysetLookupByName (elektraPluginGetConfig (handle), "/to", 0);
	if (!k)
		to = "UTF-8";
	else
		to = elektraKeyString (k);

	return to;
}

/**
 * Checks if UTF-8 conversion is needed in current context.
 * if nl_langinfo() is not available, no conversion is ever needed.
 * If iconv usage is disabled there is no need to check if we need to convert.
 * Furthermore, some systems have nl_langinfo(), but lacks ability to get
 * CODESET through it.
 * Look at the comments by the kdbbUTF8Engine() function for more information.
 *
 * @return 0 if not needed
 * @return anything else if needed
 * @ingroup backendhelper
 */
int kdbbNeedsUTF8Conversion (Plugin * handle)
{
	return strcmp (getFrom (handle), getTo (handle));
}


/**
 * Converts string to (@p direction = @c UTF8_TO) and from
 * (@p direction = @c UTF8_FROM) UTF-8.
 *
 * Since Elektra provides portability for key names and string values between
 * different codesets, you should use this helper in your backend to convert
 * to and from universal UTF-8 strings, when storing key names, values and
 * comments.
 *
 * Broken locales in applications can cause problems too. Make sure to load
 * the environment locales in your application using
 * @code
setlocale (LC_ALL, "");
 * @endcode
 *
 * Otherwise kdbbUTF8Engine and this plugin will quit
 * with error when non-ascii characters appear.
 *
 * Binary values are not effected.
 *
 * If iconv() or nl_langinfo() is not available on your system, or if iconv()
 * this plugin can't be used.
 *
 * @param direction must be @c UTF8_TO (convert from current non-UTF-8 to
 * 	UTF-8) or @c UTF8_FROM (convert from UTF-8 to current non-UTF-8)
 * @param string before the call: the string to be converted; after the call:
 * 	reallocated to carry the converted string
 * @param inputOutputByteSize before the call: the size of the string including
 * 	leading NULL; after the call: the size of the converted string including
 * 	leading NULL
 * @retval 0 on success
 * @retval -1 on failure
 * @ingroup backendhelper
 *
 */
int kdbbUTF8Engine (Plugin * handle, int direction, char ** string, size_t * inputOutputByteSize)
{
	/* Current solution is not very complete.
	 * Iconv might well be available when a usable nl_langinfo is not.
	 * In this case we it should be possible to determine charset through other means
	 * See http://www.cl.cam.ac.uk/~mgk25/unicode.html#activate for more info on a possible solution */

	char * converted = 0;
	char *readCursor, *writeCursor;
	size_t bufferSize;
	iconv_t converter;

	if (!*inputOutputByteSize) return 0;
	if (!kdbbNeedsUTF8Conversion (handle)) return 0;

	if (direction == UTF8_TO)
		converter = iconv_open (getTo (handle), getFrom (handle));
	else
		converter = iconv_open (getFrom (handle), getTo (handle));

	if (converter == (iconv_t) (-1)) return -1;

	/* work with worst case, when all chars are wide */
	bufferSize = *inputOutputByteSize * 4;
	converted = elektraMalloc (bufferSize);
	if (!converted) return -1;

	readCursor = *string;
	writeCursor = converted;
	/* On some systems and with libiconv, arg1 is const char **.
	 * ICONV_CONST is defined by configure if the system needs this */
	if (iconv (converter, &readCursor, inputOutputByteSize, &writeCursor, &bufferSize) == (size_t) (-1))
	{
		elektraFree (converted);
		iconv_close (converter);
		return -1;
	}

	/* calculate the UTF-8 string byte size, that will be returned */
	*inputOutputByteSize = writeCursor - converted;
	/* store the current kdbbDecoded string for future free */
	readCursor = *string;
	/* allocate an optimal size area to store the converted string */
	*string = elektraMalloc (*inputOutputByteSize);
	/* copy all that matters for returning */
	memcpy (*string, converted, *inputOutputByteSize);
	/* release memory used by passed string */
	elektraFree (readCursor);
	/* release buffer memory */
	elektraFree (converted);
	/* release the conversor engine */
	iconv_close (converter);
	return 0;
}


int elektraIconvGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * cur;

	elektraKeysetRewind (returned);

	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/iconv"))
	{
		ElektraKeyset * pluginConfig =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/iconv", ELEKTRA_KEY_VALUE, "iconv plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/iconv/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/iconv/exports/get", ELEKTRA_KEY_FUNC, elektraIconvGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/iconv/exports/set", ELEKTRA_KEY_FUNC, elektraIconvSet, ELEKTRA_KEY_END),
#include "readme_iconv.c"
			       elektraKeyNew ("system:/elektra/modules/iconv/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, pluginConfig);
		elektraKeysetDel (pluginConfig);
		return 1;
	}

	if (!kdbbNeedsUTF8Conversion (handle)) return 0;

	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		if (elektraKeyIsString (cur))
		{
			/* String or similar type of value */
			size_t convertedDataSize = elektraKeyGetValueSize (cur);
			char * convertedData = elektraMalloc (convertedDataSize);

			memcpy (convertedData, elektraKeyString (cur), elektraKeyGetValueSize (cur));
			if (kdbbUTF8Engine (handle, UTF8_FROM, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey, "Could not convert string %s, got result %s, encoding settings are from %s to %s",
					elektraKeyString (cur), convertedData, getFrom (handle), getTo (handle));
				elektraFree (convertedData);
				return -1;
			}
			elektraKeySetString (cur, convertedData);
			elektraFree (convertedData);
		}
		const ElektraKey * meta = elektraKeyGetMeta (cur, "comment");
		if (meta)
		{
			/* String or similar type of value */
			size_t convertedDataSize = elektraKeyGetValueSize (meta);
			char * convertedData = elektraMalloc (convertedDataSize);

			memcpy (convertedData, elektraKeyString (meta), elektraKeyGetValueSize (meta));
			if (kdbbUTF8Engine (handle, UTF8_FROM, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (
					parentKey, "Could not convert string %s, got result %s, encoding settings are from %s to %s",
					elektraKeyString (meta), convertedData, getFrom (handle), getTo (handle));
				elektraFree (convertedData);
				return -1;
			}
			elektraKeySetMeta (cur, "comment", convertedData);
			elektraFree (convertedData);
		}
	}

	return 1; /* success */
}

int elektraIconvSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * cur;

	if (!kdbbNeedsUTF8Conversion (handle)) return 0;

	elektraKeysetRewind (returned);

	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		if (elektraKeyIsString (cur))
		{
			/* String or similar type of value */
			size_t convertedDataSize = elektraKeyGetValueSize (cur);
			char * convertedData = elektraMalloc (convertedDataSize);

			memcpy (convertedData, elektraKeyString (cur), elektraKeyGetValueSize (cur));
			if (kdbbUTF8Engine (handle, UTF8_TO, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Could not convert string %s, got result %s,"
									 " encoding settings are from %s to %s (but swapped for write)",
									 elektraKeyString (cur), convertedData, getFrom (handle), getTo (handle));
				elektraFree (convertedData);
				return -1;
			}
			elektraKeySetString (cur, convertedData);
			elektraFree (convertedData);
		}
		const ElektraKey * meta = elektraKeyGetMeta (cur, "comment");
		if (meta)
		{
			/* String or similar type of value */
			size_t convertedDataSize = elektraKeyGetValueSize (meta);
			char * convertedData = elektraMalloc (convertedDataSize);

			memcpy (convertedData, elektraKeyString (meta), elektraKeyGetValueSize (meta));
			if (kdbbUTF8Engine (handle, UTF8_TO, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey,
									 "Could not convert string %s, got result %s,"
									 " encodings settings are from %s to %s (but swapped for write)",
									 elektraKeyString (meta), convertedData, getFrom (handle), getTo (handle));
				elektraFree (convertedData);
				return -1;
			}
			elektraKeySetMeta (cur, "comment", convertedData);
			elektraFree (convertedData);
		}
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_GET,	&elektraIconvGet,
		ELEKTRA_PLUGIN_SET,	&elektraIconvSet,
		ELEKTRA_PLUGIN_END);
}

