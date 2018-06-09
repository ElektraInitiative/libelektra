/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.h"

#include "kdbconfig.h"

#include <kdbhelper.h>

#include <kdblogger.h>
#include <stdlib.h>
#include <string.h>

/**
 * Gives the integer number 0-15 to a corresponding
 * hex character '0'-'9', 'a'-'f' or 'A'-'F'.
 */
static inline int elektraHexcodeConvFromHex (char c)
{
	if (c >= '0' && c <= '9') return c - '0';
	if (c >= 'a' && c <= 'f') return c - 'a' + 10;
	if (c >= 'A' && c <= 'F') return c - 'A' + 10;

	return 0; /* Unknown escape char */
}

int elektraCcodeOpen (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CCodeData * d = elektraCalloc (sizeof (CCodeData));

	/* Store for later use...*/
	elektraPluginSetData (handle, d);

	KeySet * config = elektraPluginGetConfig (handle);

	Key * escape = ksLookupByName (config, "/escape", 0);
	d->escape = '\\';
	if (escape && keyGetBaseNameSize (escape) && keyGetValueSize (escape) == 3)
	{
		int res;
		res = elektraHexcodeConvFromHex (keyString (escape)[1]);
		res += elektraHexcodeConvFromHex (keyString (escape)[0]) * 16;

		d->escape = res & 255;
	}
	ELEKTRA_LOG_DEBUG ("Use “%c” as escape character", d->escape);

	Key * root = ksLookupByName (config, "/chars", 0);

	if (!root)
	{
		/* Some default config */

		d->encode['\b'] = 'b';
		d->encode['\t'] = 't';
		d->encode['\n'] = 'n';
		d->encode['\v'] = 'v';
		d->encode['\f'] = 'f';
		d->encode['\r'] = 'r';
		d->encode['\\'] = '\\';
		d->encode['\''] = '\'';
		d->encode['\"'] = '"';
		d->encode['\0'] = '0';

		d->decode['b'] = '\b';
		d->decode['t'] = '\t';
		d->decode['n'] = '\n';
		d->decode['v'] = '\v';
		d->decode['f'] = '\f';
		d->decode['r'] = '\r';
		d->decode['\\'] = '\\';
		d->decode['\''] = '\'';
		d->decode['"'] = '\"';
		d->decode['0'] = '\0';

		return 0;
	}

	Key * cur = 0;
	while ((cur = ksNext (config)) != 0)
	{
		/* Ignore keys that are not directly below the config root key or have an incorrect size */
		if (keyRel (root, cur) != 1 || keyGetBaseNameSize (cur) != 3 || keyGetValueSize (cur) != 3) continue;

		int res;
		res = elektraHexcodeConvFromHex (keyBaseName (cur)[1]);
		res += elektraHexcodeConvFromHex (keyBaseName (cur)[0]) * 16;

		int val;
		val = elektraHexcodeConvFromHex (keyString (cur)[1]);
		val += elektraHexcodeConvFromHex (keyString (cur)[0]) * 16;

		/* Hexencode this character! */
		d->encode[res & 255] = val;
		d->decode[val & 255] = res;
	}

	return 0;
}

int elektraCcodeClose (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CCodeData * d = elektraPluginGetData (handle);

	elektraFree (d->buf);
	elektraFree (d);

	return 0;
}

/** Reads the value of the key and decodes all escaping
 * codes into the buffer.
 * @pre the buffer needs to be as large as value's size.
 * @param cur the key holding the value to decode
 * @param buf the buffer to write to
 */
void elektraCcodeDecode (Key * cur, CCodeData * d)
{
	size_t valsize = keyGetValueSize (cur);
	const char * val = keyValue (cur);

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char c = val[in];
		char * n = d->buf + out;

		if (c == d->escape)
		{
			++in; /* Advance twice */
			c = val[in];

			*n = d->decode[c & 255];
		}
		else
		{
			*n = c;
		}
		++out; /* Only one char is written */
	}

	d->buf[out] = 0; // null termination for keyString()

	keySetRaw (cur, d->buf, out + 1);
}


int elektraCcodeGet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	/* get all keys */

	if (!strcmp (keyName (parentKey), "system/elektra/modules/ccode"))
	{
		KeySet * pluginConfig =
			ksNew (30, keyNew ("system/elektra/modules/ccode", KEY_VALUE, "ccode plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/ccode/exports", KEY_END),
			       keyNew ("system/elektra/modules/ccode/exports/open", KEY_FUNC, elektraCcodeOpen, KEY_END),
			       keyNew ("system/elektra/modules/ccode/exports/close", KEY_FUNC, elektraCcodeClose, KEY_END),
			       keyNew ("system/elektra/modules/ccode/exports/get", KEY_FUNC, elektraCcodeGet, KEY_END),
			       keyNew ("system/elektra/modules/ccode/exports/set", KEY_FUNC, elektraCcodeSet, KEY_END),
#include "readme_ccode.c"
			       keyNew ("system/elektra/modules/ccode/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, pluginConfig);
		ksDel (pluginConfig);
		return 1;
	}

	CCodeData * d = elektraPluginGetData (handle);
	if (!d->buf)
	{
		d->buf = elektraMalloc (1000);
		d->bufalloc = 1000;
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		size_t valsize = keyGetValueSize (cur);
		if (valsize > d->bufalloc)
		{
			d->bufalloc = valsize;
			d->buf = realloc (d->buf, d->bufalloc);
		}

		elektraCcodeDecode (cur, d);
	}

	return 1; /* success */
}


/** Reads the value of the key and encodes it in
 * c-style in the buffer.
 *
 * @param cur the key which value is to encode
 * @param buf the buffer
 * @pre the buffer needs to have twice as much space as the value's size
 */
void elektraCcodeEncode (Key * cur, CCodeData * d)
{
	size_t valsize = keyGetValueSize (cur);
	const char * val = keyValue (cur);

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char c = val[in];
		char * n = d->buf + out + 1;

		if (d->encode[c])
		{
			*n = d->encode[c];
			// Escape char
			d->buf[out] = d->escape;
			out += 2;
		}
		else
		{
			// just copy one character
			d->buf[out] = val[in];
			// advance out cursor
			out++;
			// go to next char
		}
	}

	d->buf[out] = 0; // null termination for keyString()

	keySetRaw (cur, d->buf, out + 1);
}


int elektraCcodeSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	CCodeData * d = elektraPluginGetData (handle);
	if (!d->buf)
	{
		d->buf = elektraMalloc (1000);
		d->bufalloc = 1000;
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		size_t valsize = keyGetValueSize (cur);
		if (valsize * 2 > d->bufalloc)
		{
			d->bufalloc = valsize * 2;
			d->buf = realloc (d->buf, d->bufalloc);
		}

		elektraCcodeEncode (cur, d);
	}

	return 1; /* success */
}

Plugin * ELEKTRA_PLUGIN_EXPORT (ccode)
{
	// clang-format off
	return elektraPluginExport("ccode",
		ELEKTRA_PLUGIN_OPEN,	&elektraCcodeOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCcodeClose,
		ELEKTRA_PLUGIN_GET,	&elektraCcodeGet,
		ELEKTRA_PLUGIN_SET,	&elektraCcodeSet,
		ELEKTRA_PLUGIN_END);
}

