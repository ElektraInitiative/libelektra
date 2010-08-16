/***************************************************************************
                     hexcode.c  -  Skeleton of a plugin
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


#include "hexcode.h"

#include <stdlib.h>
#include <string.h>

/**
  * Gives the integer number 0-15 to a corresponding
  * hex character '0'-'9', 'a'-'f' or 'A'-'F'.
  */
static inline int elektraHexcodeConvFromHex(char c)
{
	if    (c == '0') return 0;
	else if (c=='1') return 1;
	else if (c=='2') return 2;
	else if (c=='3') return 3;
	else if (c=='4') return 4;
	else if (c=='5') return 5;
	else if (c=='6') return 6;
	else if (c=='7') return 7;
	else if (c=='8') return 8;
	else if (c=='9') return 9;
	else if (c=='a' || c=='A') return 10;
	else if (c=='b' || c=='B') return 11;
	else if (c=='c' || c=='C') return 12;
	else if (c=='d' || c=='D') return 13;
	else if (c=='e' || c=='E') return 14;
	else if (c=='f' || c=='F') return 15;
	else return 0; /* Unknown escape char */
}

/** Reads the value of the key and decodes all escaping
  * codes into the buffer.
  * @pre the buffer needs to be as large as value's size.
  * @param cur the key holding the value to decode
  * @param buf the buffer to write to
  */
void elektraHexcodeDecode (Key *cur, CHexData *hd)
{
	size_t valsize = keyGetValueSize(cur);
	const char *val = keyValue(cur);

	size_t out=0;
	for (size_t in=0; in<valsize-1; ++in)
	{
		char c = val[in];
		char *n = hd->buf+out;

		if (c == '\\')
		{
			in+=2; /* Advance twice (2 hex numbers) */
			char first = val[in-1];
			char second = val[in];
			int res;

			res = elektraHexcodeConvFromHex(second);
			res += elektraHexcodeConvFromHex(first)*16;
			*n = res & 255;
		} else {
			*n = c;
		}
		++out; /* Only one char is written */
	}

	hd->buf[out] = 0; // null termination for keyString()

	keySetRaw(cur, hd->buf, out+1);
}


int elektraHexcodeGet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* get all keys */

	if (!strcmp (keyName(parentKey), "system/elektra/modules/hexcode"))
	{
		KeySet *pluginConfig = ksNew (30,
			keyNew ("system/elektra/modules/hexcode",
				KEY_VALUE, "hexcode plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/hexcode/exports", KEY_END),
			keyNew ("system/elektra/modules/hexcode/exports/get",
				KEY_FUNC, elektraHexcodeGet, KEY_END),
			keyNew ("system/elektra/modules/hexcode/exports/set",
				KEY_FUNC, elektraHexcodeSet, KEY_END),
			keyNew ("system/elektra/modules/hexcode/exports/open",
				KEY_FUNC, elektraHexcodeOpen, KEY_END),
			keyNew ("system/elektra/modules/hexcode/exports/close",
				KEY_FUNC, elektraHexcodeClose, KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos",
				KEY_VALUE, "All information you want to know", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/author",
				KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/licence",
				KEY_VALUE, "BSD", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/description",
				KEY_VALUE, "Decoding/Encoding engine which escapes unwanted characters.", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/provides",
				KEY_VALUE, "code", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/placements",
				KEY_VALUE, "postgetstorage presetstorage", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/needs",
				KEY_VALUE, "", KEY_END),
			keyNew ("system/elektra/modules/hexcode/infos/version",
				KEY_VALUE, PLUGINVERSION, KEY_END),
			KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);
		return 1;
	}

	CHexData *hd = elektraPluginGetData (handle);
	if (!hd->buf)
	{
		hd->buf = malloc (1000);
		hd->bufalloc = 1000;
	}

	Key *cur;
	ksRewind(returned);
	while ((cur = ksNext(returned)) != 0)
	{
		size_t valsize = keyGetValueSize(cur);
		if (valsize > hd->bufalloc)
		{
			hd->bufalloc = valsize;
			hd->buf = realloc (hd->buf, hd->bufalloc);
		}

		elektraHexcodeDecode (cur, hd);
	}

	return 1; /* success */
}


/**
  * Gives the integer number 0-15 to a corresponding
  * hex character '0'-'9', 'a'-'f' or 'A'-'F'.
  */
static inline char elektraHexcodeConvToHex(int c)
{
	switch (c)
	{
		case 0: return '0';
		case 1: return '1';
		case 2: return '2';
		case 3: return '3';
		case 4: return '4';
		case 5: return '5';
		case 6: return '6';
		case 7: return '7';
		case 8: return '8';
		case 9: return '9';
		case 10: return 'A';
		case 11: return 'B';
		case 12: return 'C';
		case 13: return 'D';
		case 14: return 'E';
		case 15: return 'F';
		default: return '0';
	}
}


/** Reads the value of the key and encodes it in
  * c-style in the buffer.
  *
  * @param cur the key which value is to encode
  * @param buf the buffer
  * @pre the buffer needs to have thrice as much space as the value's size
  */
void elektraHexcodeEncode (Key *cur, CHexData *hd)
{
	size_t valsize = keyGetValueSize(cur);
	const char *val = keyValue(cur);

	size_t out=0;
	for (size_t in=0; in<valsize-1; ++in)
	{
		unsigned char c = val[in];

		// need to encode char?
		if (hd->hd[c & 255])
		{
			hd->buf[out] = '\\'; out ++;
			hd->buf[out] = elektraHexcodeConvToHex(c/16); out ++;
			hd->buf[out] = elektraHexcodeConvToHex(c%16); out ++;
		}
		else
		{
			// just copy one character
			hd->buf[out] = val[in];
			// advance out cursor
			out ++;
		}
	}

	hd->buf[out] = 0; // null termination for keyString()

	keySetRaw(cur, hd->buf, out+1);
}


int elektraHexcodeSet(Plugin *handle, KeySet *returned, Key *parentKey)
{
	/* set all keys */
	CHexData *hd = elektraPluginGetData (handle);
	if (!hd->buf)
	{
		hd->buf = malloc (1000);
		hd->bufalloc = 1000;
	}

	Key *cur;
	ksRewind(returned);
	while ((cur = ksNext(returned)) != 0)
	{
		size_t valsize = keyGetValueSize(cur);
		if (valsize*3 > hd->bufalloc)
		{
			hd->bufalloc = valsize*3;
			hd->buf = realloc (hd->buf, hd->bufalloc);
		}

		elektraHexcodeEncode (cur, hd);
	}

	return 1; /* success */
}

int elektraHexcodeOpen(Plugin *handle, Key *k)
{
	CHexData *hd = calloc (1, sizeof(CHexData));

	/* Store for later use...*/
	elektraPluginSetData (handle, hd);

	KeySet *config = elektraPluginGetConfig (handle);
	Key *root = ksLookupByName (config, "/chars", 0);

	Key *cur = 0;
	if (!root)
	{
		/* Some default config */
		hd->hd['\0'] = 1;
		hd->hd['\n'] = 1;
		hd->hd['\\'] = 1;
		hd->hd[' '] = 1;
	} else {
		while ((cur = ksNext(config)) != 0)
		{
			/* ignore all keys not direct below */
			if (keyRel (root, cur) == 1)
			{
				/* ignore invalid size */
				if (keyGetBaseNameSize(cur) != 3) continue;

				int res;
				res = elektraHexcodeConvFromHex(keyBaseName(cur)[1]);
				res += elektraHexcodeConvFromHex(keyBaseName(cur)[0])*16;

				/* Hexencode this character! */
				hd->hd [res & 255] = 1;
			}
		}
	}

	return 0;
}

int elektraHexcodeClose(Plugin *handle, Key *k)
{
	CHexData *hd = elektraPluginGetData (handle);

	free (hd->buf);
	free (hd);

	return 0;
}

Plugin *ELEKTRA_PLUGIN_EXPORT(hexcode)
{
	return elektraPluginExport("hexcode",
		ELEKTRA_PLUGIN_GET,	&elektraHexcodeGet,
		ELEKTRA_PLUGIN_SET,	&elektraHexcodeSet,
		ELEKTRA_PLUGIN_OPEN,	&elektraHexcodeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraHexcodeClose,
		ELEKTRA_PLUGIN_END);
}

