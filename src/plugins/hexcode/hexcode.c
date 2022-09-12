/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "hexcode.h"

#ifndef HAVE_KDBCONFIG
#include "kdbconfig.h"
#endif

#include <kdbhelper.h>

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
void elektraHexcodeDecode (ElektraKey * cur, CHexData * hd)
{
	size_t valsize = elektraKeyGetValueSize (cur);
	const char * val = elektraKeyValue (cur);

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

	elektraKeySetRaw (cur, hd->buf, out + 1);
}


int elektraHexcodeGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey)
{
	/* get all keys */

	if (!strcmp (elektraKeyName (parentKey), "system:/elektra/modules/hexcode"))
	{
		ElektraKeyset * pluginConfig =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/hexcode", ELEKTRA_KEY_VALUE, "hexcode plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/hexcode/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/hexcode/exports/get", ELEKTRA_KEY_FUNC, elektraHexcodeGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/hexcode/exports/set", ELEKTRA_KEY_FUNC, elektraHexcodeSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/hexcode/exports/open", ELEKTRA_KEY_FUNC, elektraHexcodeOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/hexcode/exports/close", ELEKTRA_KEY_FUNC, elektraHexcodeClose, ELEKTRA_KEY_END),
#include "readme_hexcode.c"
			       elektraKeyNew ("system:/elektra/modules/hexcode/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, pluginConfig);
		elektraKeysetDel (pluginConfig);
		return 1;
	}

	CHexData * hd = elektraPluginGetData (handle);
	if (!hd->buf)
	{
		hd->buf = elektraMalloc (1000);
		hd->bufalloc = 1000;
	}

	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		size_t valsize = elektraKeyGetValueSize (cur);
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
void elektraHexcodeEncode (ElektraKey * cur, CHexData * hd)
{
	size_t valsize = elektraKeyGetValueSize (cur);
	const char * val = elektraKeyValue (cur);

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

	elektraKeySetRaw (cur, hd->buf, out + 1);
}


int elektraHexcodeSet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	CHexData * hd = elektraPluginGetData (handle);
	if (!hd->buf)
	{
		hd->buf = elektraMalloc (1000);
		hd->bufalloc = 1000;
	}

	ElektraKey * cur;
	elektraKeysetRewind (returned);
	while ((cur = elektraKeysetNext (returned)) != 0)
	{
		size_t valsize = elektraKeyGetValueSize (cur);
		if (valsize * 3 > hd->bufalloc)
		{
			hd->bufalloc = valsize * 3;
			hd->buf = realloc (hd->buf, hd->bufalloc);
		}

		elektraHexcodeEncode (cur, hd);
	}

	return 1; /* success */
}

int elektraHexcodeOpen (Plugin * handle, ElektraKey * key ELEKTRA_UNUSED)
{
	CHexData * hd = calloc (1, sizeof (CHexData));

	/* Store for later use...*/
	elektraPluginSetData (handle, hd);

	ElektraKeyset * config = elektraPluginGetConfig (handle);

	ElektraKey * escape = elektraKeysetLookupByName (config, "/escape", 0);
	hd->escape = '\\';
	if (escape && elektraKeyGetBaseNameSize (escape) && elektraKeyGetValueSize (escape) == 3)
	{
		int res;
		res = elektraHexcodeConvFromHex (elektraKeyString (escape)[1]);
		res += elektraHexcodeConvFromHex (elektraKeyString (escape)[0]) * 16;

		hd->escape = res & 255;
	}

	ElektraKey * root = elektraKeysetLookupByName (config, "/chars", 0);
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
		ElektraKey * cur = 0;
		while ((cur = elektraKeysetNext (config)) != 0)
		{
			/* ignore all keys not direct below */
			if (elektraKeyIsDirectlyBelow (root, cur) == 1)
			{
				/* ignore invalid size */
				if (elektraKeyGetBaseNameSize (cur) != 3) continue;

				int res;
				res = elektraHexcodeConvFromHex (elektraKeyBaseName (cur)[1]);
				res += elektraHexcodeConvFromHex (elektraKeyBaseName (cur)[0]) * 16;

				/* Hexencode this character! */
				hd->hd[res & 255] = 1;
			}
		}
	}

	return 0;
}

int elektraHexcodeClose (Plugin * handle, ElektraKey * key ELEKTRA_UNUSED)
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

