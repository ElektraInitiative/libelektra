/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./hexcode.h"

#ifndef HAVE_KDBCONFIG
#include <internal/config.h>
#endif

#include <internal/utility/old_helper.h>

#include <stdlib.h>
#include <string.h>

/**
 * Gives the integer number 0-15 to a corresponding
 * hex character '0'-'9', 'a'-'f' or 'A'-'F'.
 */
static inline int elektraHexcodeConvFromHex (char c)
{
	if (c == '0')
		return 0;
	else if (c == '1')
		return 1;
	else if (c == '2')
		return 2;
	else if (c == '3')
		return 3;
	else if (c == '4')
		return 4;
	else if (c == '5')
		return 5;
	else if (c == '6')
		return 6;
	else if (c == '7')
		return 7;
	else if (c == '8')
		return 8;
	else if (c == '9')
		return 9;
	else if (c == 'a' || c == 'A')
		return 10;
	else if (c == 'b' || c == 'B')
		return 11;
	else if (c == 'c' || c == 'C')
		return 12;
	else if (c == 'd' || c == 'D')
		return 13;
	else if (c == 'e' || c == 'E')
		return 14;
	else if (c == 'f' || c == 'F')
		return 15;
	else
		return 0; /* Unknown escape char */
}

/** Reads the value of the key and decodes all escaping
 * codes into the buffer.
 * @pre the buffer needs to be as large as value's size.
 * @param cur the key holding the value to decode
 * @param buf the buffer to write to
 */
void elektraHexcodeDecode (Key * cur, CHexData * hd)
{
	size_t valsize = keyGetValueSize (cur);
	const char * val = keyValue (cur);

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		char c = val[in];
		char * n = hd->buf + out;

		if (c == hd->escape)
		{
			in += 2; /* Advance twice (2 hex numbers) */
			char first = val[in - 1];
			char second = val[in];
			int res;

			res = elektraHexcodeConvFromHex (second);
			res += elektraHexcodeConvFromHex (first) * 16;
			*n = res & 255;
		}
		else
		{
			*n = c;
		}
		++out; /* Only one char is written */
	}

	hd->buf[out] = 0; // null termination for keyString()

	keySetRaw (cur, hd->buf, out + 1);
}


int elektraHexcodeGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system:/elektra/modules/hexcode"))
	{
		KeySet * pluginConfig =
			ksNew (30, keyNew ("system:/elektra/modules/hexcode", KEY_VALUE, "hexcode plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/hexcode/exports", KEY_END),
			       keyNew ("system:/elektra/modules/hexcode/exports/get", KEY_FUNC, elektraHexcodeGet, KEY_END),
			       keyNew ("system:/elektra/modules/hexcode/exports/set", KEY_FUNC, elektraHexcodeSet, KEY_END),
			       keyNew ("system:/elektra/modules/hexcode/exports/open", KEY_FUNC, elektraHexcodeOpen, KEY_END),
			       keyNew ("system:/elektra/modules/hexcode/exports/close", KEY_FUNC, elektraHexcodeClose, KEY_END),
#include "./readme_hexcode.c"
			       keyNew ("system:/elektra/modules/hexcode/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);
		return 1;
	}

	CHexData * hd = elektraPluginGetData (handle);
	if (!hd->buf)
	{
		hd->buf = elektraMalloc (1000);
		hd->bufalloc = 1000;
	}

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		size_t valsize = keyGetValueSize (cur);
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
static inline char elektraHexcodeConvToHex (int c)
{
	switch (c)
	{
	case 0:
		return '0';
	case 1:
		return '1';
	case 2:
		return '2';
	case 3:
		return '3';
	case 4:
		return '4';
	case 5:
		return '5';
	case 6:
		return '6';
	case 7:
		return '7';
	case 8:
		return '8';
	case 9:
		return '9';
	case 10:
		return 'A';
	case 11:
		return 'B';
	case 12:
		return 'C';
	case 13:
		return 'D';
	case 14:
		return 'E';
	case 15:
		return 'F';
	default:
		return '0';
	}
}


/** Reads the value of the key and encodes it in
 * c-style in the buffer.
 *
 * @param cur the key which value is to encode
 * @param buf the buffer
 * @pre the buffer needs to have thrice as much space as the value's size
 */
void elektraHexcodeEncode (Key * cur, CHexData * hd)
{
	size_t valsize = keyGetValueSize (cur);
	const char * val = keyValue (cur);

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char c = val[in];

		// need to encode char?
		if (hd->hd[c & 255])
		{
			hd->buf[out] = hd->escape;
			out++;
			hd->buf[out] = elektraHexcodeConvToHex (c / 16);
			out++;
			hd->buf[out] = elektraHexcodeConvToHex (c % 16);
			out++;
		}
		else
		{
			// just copy one character
			hd->buf[out] = val[in];
			// advance out cursor
			out++;
		}
	}

	hd->buf[out] = 0; // null termination for keyString()

	keySetRaw (cur, hd->buf, out + 1);
}


int elektraHexcodeSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	CHexData * hd = elektraPluginGetData (handle);
	if (!hd->buf)
	{
		hd->buf = elektraMalloc (1000);
		hd->bufalloc = 1000;
	}

	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
		size_t valsize = keyGetValueSize (cur);
		if (valsize * 3 > hd->bufalloc)
		{
			hd->bufalloc = valsize * 3;
			hd->buf = realloc (hd->buf, hd->bufalloc);
		}

		elektraHexcodeEncode (cur, hd);
	}

	return 1; /* success */
}

int elektraHexcodeOpen (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CHexData * hd = calloc (1, sizeof (CHexData));

	/* Store for later use...*/
	elektraPluginSetData (handle, hd);

	KeySet * config = elektraPluginGetConfig (handle);

	Key * escape = ksLookupByName (config, "/escape", 0);
	hd->escape = '\\';
	if (escape && keyGetBaseNameSize (escape) && keyGetValueSize (escape) == 3)
	{
		int res;
		res = elektraHexcodeConvFromHex (keyString (escape)[1]);
		res += elektraHexcodeConvFromHex (keyString (escape)[0]) * 16;

		hd->escape = res & 255;
	}


	Key * charsKey = keyNew ("/chars", KEY_END);
	Key * root = ksLookup (config, charsKey, 0);
	elektraCursor it = ksSearch (config, charsKey) + 1;
	keyDel (charsKey);

	if (!root)
	{
		/* Some default config */
		hd->hd['\0'] = 1;
		hd->hd['\n'] = 1;
		hd->hd['\\'] = 1;
		hd->hd[' '] = 1;
	}
	else
	{
		for (; it < ksGetSize (config); ++it)
		{
			Key * cur = ksAtCursor (config, it);
			/* ignore all keys not direct below */
			if (keyIsDirectlyBelow (root, cur) == 1)
			{
				/* ignore invalid size */
				if (keyGetBaseNameSize (cur) != 3) continue;

				int res;
				res = elektraHexcodeConvFromHex (keyBaseName (cur)[1]);
				res += elektraHexcodeConvFromHex (keyBaseName (cur)[0]) * 16;

				/* Hexencode this character! */
				hd->hd[res & 255] = 1;
			}
		}
	}

	return 0;
}

int elektraHexcodeClose (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CHexData * hd = elektraPluginGetData (handle);

	elektraFree (hd->buf);
	elektraFree (hd);

	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport("hexcode",
		ELEKTRA_PLUGIN_GET,	&elektraHexcodeGet,
		ELEKTRA_PLUGIN_SET,	&elektraHexcodeSet,
		ELEKTRA_PLUGIN_OPEN,	&elektraHexcodeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraHexcodeClose,
		ELEKTRA_PLUGIN_END);
}

