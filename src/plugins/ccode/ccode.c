/***************************************************************************
                     ccode.c  -  Skeleton of a plugin
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
 *   to provide a valid plugin.                                            *
 *   Simple fill the empty functions with your code and you are            *
 *   ready to go.                                                          *
 *                                                                         *
 ***************************************************************************/


#include "ccode.h"

#include <stdlib.h>
#include <string.h>

/** Reads the value of the key and decodes all escaping
  * codes into the buffer.
  * @pre the buffer needs to be as large as value's size.
  * @param cur the key holding the value to decode
  * @param buf the buffer to write to
  */
void elektraCcodeDecode (Key *cur, char* buf)
{
	size_t valsize = keyGetValueSize(cur);
	const char *val = keyValue(cur);

	size_t out=0;
	for (size_t in=0; in<valsize-1; ++in)
	{
		char c = val[in];
		char *n = buf+out;

		if (c == '\\')
		{
			++in; /* Advance twice */
			c = val[in];

			if    (c == '0') *n = '\0';
			else if (c=='n') *n = '\n';
			else if (c=='b') *n = '\\';
			else if (c=='w') *n = ' ';
			else if (c=='e') *n = '=';
			else if (c=='s') *n = ';';
			else if (c=='r') *n = '#';
			else *n = '\0'; /* Unknown escape char */
		} else {
			*n = c;
		}
		++out; /* Only one char is written */
	}

	buf[out] = 0; // null termination for keyString()

	keySetRaw(cur, buf, out+1);
}


int elektraCcodeGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	if (!strcmp (keyName(parentKey), "system/elektra/modules/ccode"))
	{
		KeySet *pluginConfig = ksNew (30,
			keyNew ("system/elektra/modules/ccode",
				KEY_VALUE, "ccode plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/ccode/exports", KEY_END),
			keyNew ("system/elektra/modules/ccode/exports/get",
				KEY_FUNC, elektraCcodeGet, KEY_END),
			keyNew ("system/elektra/modules/ccode/exports/set",
				KEY_FUNC, elektraCcodeSet, KEY_END),
			keyNew ("system/elektra/modules/ccode/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/description",
				KEY_VALUE, "Decoding/Encoding engine which escapes unwanted characters.", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/provides",
				KEY_VALUE, "code", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/placements",
				KEY_VALUE, "postgetstorage presetstorage", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/ccode/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);
		return 1;
	}

	Key *cur;
	char *buf = malloc (1000);
	size_t bufalloc = 1000;

	ksRewind(returned);
	while ((cur = ksNext(returned)) != 0)
	{
		size_t valsize = keyGetValueSize(cur);
		if (valsize > bufalloc)
		{
			bufalloc = valsize;
			buf = realloc (buf, bufalloc);
		}

		elektraCcodeDecode (cur, buf);
	}

	free (buf);

	return 1; /* success */
}


/** Reads the value of the key and encodes it in
  * c-style in the buffer.
  *
  * @param cur the key which value is to encode
  * @param buf the buffer
  * @pre the buffer needs to have twice as much space as the value's size
  */
void elektraCcodeEncode (Key *cur, char* buf)
{
	size_t valsize = keyGetValueSize(cur);
	const char *val = keyValue(cur);

	size_t out=0;
	for (size_t in=0; in<valsize-1; ++in)
	{
		char c = val[in];
		char *n = buf+out+1;

		if    (c == '\0') *n = '0';
		else if (c=='\n') *n = 'n';
		else if (c=='\\') *n = 'b';
		else if (c==' ') *n = 'w';
		else if (c=='=') *n = 'e';
		else if (c==';') *n = 's';
		else if (c=='#') *n = 'r';
		else
		{
			// just copy one character
			buf[out] = val[in];
			// advance out cursor
			out ++;
			// go to next char
			continue;
		}
		buf[out] = '\\'; //Escape char
		out += 2;
	}

	buf[out] = 0; // null termination for keyString()

	keySetRaw(cur, buf, out+1);
}


int elektraCcodeSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	Key *cur;
	char *buf = malloc (1000);
	size_t bufalloc = 1000;

	ksRewind(returned);
	while ((cur = ksNext(returned)) != 0)
	{
		size_t valsize = keyGetValueSize(cur);
		if (valsize*2 > bufalloc)
		{
			bufalloc = valsize*2;
			buf = realloc (buf, bufalloc);
		}

		elektraCcodeEncode (cur, buf);
	}

	free (buf);

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(ccode)
{
	return elektraPluginExport("ccode",
		ELEKTRA_PLUGIN_GET,	&elektraCcodeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCcodeSet,
		ELEKTRA_PLUGIN_END);
}

