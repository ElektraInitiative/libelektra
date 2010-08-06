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

void elektraCcodeEncode (Key *cur, char* buf)
{
	size_t valsize = keyGetValueSize(cur);
	const char *val = keyValue(cur);

	for (size_t in=0, out=0; in<valsize; ++in)
	{
		char c = val[in];
		char *n = buf+out+1;

		if (c == '\0') *n = '0';
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

	keySetString(cur, buf);
}

int elektraCcodeGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	Key *cur;
	char *buf = malloc (1000);
	size_t bufsize = 1000;

	ksRewind(returned);
	while ((cur = ksNext(returned)) != 0)
	{
		size_t valsize = keyGetValueSize(cur);
		if (valsize*2 > bufsize)
		{
			bufsize = valsize*2;
			buf = realloc (buf, bufsize);
		}

		elektraCcodeEncode (cur, buf);
	}

	free (buf);

	return 1; /* success */
}

int elektraCcodeSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */

	return 1; /* success */
}

Plugin *ELEKTRA_PLUGIN_EXPORT(ccode)
{
	return elektraPluginExport("ccode",
		ELEKTRA_PLUGIN_GET,	&elektraCcodeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCcodeSet,
		ELEKTRA_PLUGIN_END);
}

