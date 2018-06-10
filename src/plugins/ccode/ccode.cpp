/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include "kdbconfig.h"

#include <kdbhelper.h>

#include <kdblogger.h>
#include <stdlib.h>
#include <string.h>

inline constexpr unsigned char operator"" _uc (char character) noexcept
{
	return static_cast<unsigned char> (character);
}

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

using namespace ckdb;
extern "C" {

int elektraCcodeOpen (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CCodeData * d = new CCodeData ();

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

		d->encode['\b'_uc] = 'b'_uc;
		d->encode['\t'_uc] = 't'_uc;
		d->encode['\n'_uc] = 'n'_uc;
		d->encode['\v'_uc] = 'v'_uc;
		d->encode['\f'_uc] = 'f'_uc;
		d->encode['\r'_uc] = 'r'_uc;
		d->encode['\\'_uc] = '\\'_uc;
		d->encode['\''_uc] = '\''_uc;
		d->encode['\"'_uc] = '"'_uc;
		d->encode['\0'_uc] = '0'_uc;

		d->decode['b'_uc] = '\b'_uc;
		d->decode['t'_uc] = '\t'_uc;
		d->decode['n'_uc] = '\n'_uc;
		d->decode['v'_uc] = '\v'_uc;
		d->decode['f'_uc] = '\f'_uc;
		d->decode['r'_uc] = '\r'_uc;
		d->decode['\\'_uc] = '\\'_uc;
		d->decode['\''_uc] = '\''_uc;
		d->decode['"'_uc] = '\"'_uc;
		d->decode['0'_uc] = '\0'_uc;

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
	CCodeData * d = static_cast<CCodeData *> (elektraPluginGetData (handle));

	delete[](d->buf);
	delete (d);

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
	const char * val = static_cast<const char *> (keyValue (cur));

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char c = val[in];

		if (c == d->escape)
		{
			++in; /* Advance twice */
			c = val[in];

			d->buf[out] = d->decode[c & 255];
		}
		else
		{
			d->buf[out] = c;
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

	CCodeData * d = static_cast<CCodeData *> (elektraPluginGetData (handle));
	if (!d->buf)
	{
		d->bufalloc = 1000;
		d->buf = new unsigned char[d->bufalloc];
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		size_t valsize = keyGetValueSize (cur);
		if (valsize > d->bufalloc)
		{
			d->bufalloc = valsize;
			d->buf = new unsigned char[d->bufalloc];
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
	const char * val = static_cast<const char *> (keyValue (cur));

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char c = val[in];

		if (d->encode[c])
		{
			d->buf[out + 1] = d->encode[c];
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
	CCodeData * d = static_cast<CCodeData *> (elektraPluginGetData (handle));
	if (!d->buf)
	{
		d->bufalloc = 1000;
		d->buf = new unsigned char[d->bufalloc];
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		size_t valsize = keyGetValueSize (cur);
		if (valsize * 2 > d->bufalloc)
		{
			d->bufalloc = valsize * 2;
			d->buf = new unsigned char[d->bufalloc];
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

} // end extern "C"
