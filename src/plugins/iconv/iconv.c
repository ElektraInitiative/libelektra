/***************************************************************************
          iconv.c  -  Skeleton of a plugin to be copied
                             -------------------
    begin                : Fri May 21 2010
    copyright            : (C) 2010 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/



/***************************************************************************
 *                                                                         *
 *   This is the skeleton of the methods you'll have to implement in order *
 *   to provide libelektra.so a valid plugin.                             *
 *   Simple fill the empty _iconv functions with your code and you are   *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "iconv.h"

static inline const char* getFrom(Plugin *handle)
{
	const char *from;
	Key *k;

	k = ksLookupByName(elektraPluginGetConfig(handle), "/from", 0);
	if (!k) from = nl_langinfo(CODESET);
	else from = keyString(k);

	return from;
}

static inline const char* getTo(Plugin *handle)
{
	const char *to;
	Key *k;

	k = ksLookupByName(elektraPluginGetConfig(handle), "/to", 0);
	if (!k) to = "UTF-8";
	else to = keyString(k);

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
int kdbbNeedsUTF8Conversion(Plugin *handle)
{
	return strcmp(getFrom(handle),getTo(handle));
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
int kdbbUTF8Engine(Plugin *handle, int direction, char **string, size_t *inputOutputByteSize)
{
/* Current solution is not very complete.
 * Iconv might well be available when a usable nl_langinfo is not.
 * In this case we it should be possible to determine charset through other means
 * See http://www.cl.cam.ac.uk/~mgk25/unicode.html#activate for more info on a possible solution */
 
	char *converted=0;
	char *readCursor, *writeCursor;
	size_t bufferSize;
	iconv_t converter;

	if (!*inputOutputByteSize) return 0;
	if (!kdbbNeedsUTF8Conversion(handle)) return 0;

	if (direction==UTF8_TO) converter=iconv_open(getTo(handle),getFrom(handle));
	else converter=iconv_open(getFrom(handle),getTo(handle));

	if (converter == (iconv_t)(-1)) return -1;

	/* work with worst case, when all chars are wide */
	bufferSize=*inputOutputByteSize * 4;
	converted=malloc(bufferSize);
	if (!converted) return -1;

	readCursor=*string;
	writeCursor=converted;
	/* On some systems and with libiconv, arg1 is const char **. 
	 * ICONV_CONST is defined by configure if the system needs this */
	if (iconv(converter,
			&readCursor,inputOutputByteSize,
			&writeCursor,&bufferSize) == (size_t)(-1)) {
		free(converted);
		iconv_close(converter);
		return -1;
	}

	/* calculate the UTF-8 string byte size, that will be returned */
	*inputOutputByteSize=writeCursor-converted;
	/* store the current kdbbDecoded string for future free */
	readCursor=*string;
	/* allocate an optimal size area to store the converted string */
	*string=malloc(*inputOutputByteSize);
	/* copy all that matters for returning */
	memcpy(*string,converted,*inputOutputByteSize);
	/* release memory used by passed string */
	free(readCursor);
	/* release buffer memory */
	free(converted);
	/* release the conversor engine */
	iconv_close(converter);
	return 0;
}


int elektraIconvGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	Key *cur;
	const Key *meta;

	ksRewind (returned);

	if (!strcmp (keyName(parentKey), "system/elektra/modules/iconv"))
	{
		KeySet *pluginConfig = ksNew (30,
			keyNew ("system/elektra/modules/iconv",
				KEY_VALUE, "iconv plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/iconv/exports", KEY_END),
			keyNew ("system/elektra/modules/iconv/exports/get",
				KEY_FUNC, elektraIconvGet, KEY_END),
			keyNew ("system/elektra/modules/iconv/exports/set",
				KEY_FUNC, elektraIconvSet, KEY_END),
#include "readme_iconv.c"
			keyNew ("system/elektra/modules/iconv/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);
		return 1;
	}

	if (!kdbbNeedsUTF8Conversion(handle)) return 0;

	while ((cur = ksNext(returned)) != 0)
	{
		if (keyIsString (cur))
		{
			/* String or similar type of value */
			size_t convertedDataSize=keyGetValueSize(cur);
			char *convertedData=malloc(convertedDataSize);

			memcpy(convertedData,keyString(cur),keyGetValueSize(cur));
			if (kdbbUTF8Engine(handle, UTF8_FROM, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_ERROR (46, parentKey, convertedData);
				free(convertedData);
				return -1;
			}
			keySetString(cur, convertedData);
			free(convertedData);
		}
		meta = keyGetMeta(cur, "comment");
		if (meta)
		{
			/* String or similar type of value */
			size_t convertedDataSize=keyGetValueSize(meta);
			char *convertedData=malloc(convertedDataSize);

			memcpy(convertedData,keyString(meta),keyGetValueSize(meta));
			if (kdbbUTF8Engine(handle, UTF8_FROM, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_ERROR (46, parentKey, convertedData);
				free(convertedData);
				return -1;
			}
			keySetMeta(cur, "comment", convertedData);
			free(convertedData);
		}
	}

	return 1; /* success */
}

int elektraIconvSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	Key *cur;
	const Key *meta;

	if (!kdbbNeedsUTF8Conversion(handle)) return 0;

	ksRewind (returned);

	while ((cur = ksNext(returned)) != 0)
	{
		if (keyIsString (cur))
		{
			/* String or similar type of value */
			size_t convertedDataSize=keyGetValueSize(cur);
			char *convertedData=malloc(convertedDataSize);

			memcpy(convertedData,keyString(cur),keyGetValueSize(cur));
			if (kdbbUTF8Engine(handle, UTF8_TO, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_ERROR (46, parentKey, convertedData);
				free(convertedData);
				return -1;
			}
			keySetString(cur, convertedData);
			free(convertedData);
		}
		meta = keyGetMeta(cur, "comment");
		if (meta)
		{
			/* String or similar type of value */
			size_t convertedDataSize=keyGetValueSize(meta);
			char *convertedData=malloc(convertedDataSize);

			memcpy(convertedData,keyString(meta),keyGetValueSize(meta));
			if (kdbbUTF8Engine(handle, UTF8_TO, &convertedData, &convertedDataSize))
			{
				ELEKTRA_SET_ERROR (46, parentKey, convertedData);
				free(convertedData);
				return -1;
			}
			keySetMeta(cur, "comment", convertedData);
			free(convertedData);
		}
	}

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(iconv)
{
	return elektraPluginExport(BACKENDNAME,
		ELEKTRA_PLUGIN_GET,	&elektraIconvGet,
		ELEKTRA_PLUGIN_SET,	&elektraIconvSet,
		ELEKTRA_PLUGIN_END);
}

