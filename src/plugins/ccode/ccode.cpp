/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include <kdblogger.h>

inline constexpr unsigned char operator"" _uc (char character) noexcept
{
	return static_cast<unsigned char> (character);
}

/**
 * @brief This function maps hex characters to integer numbers.
 *
 * @pre The specified character has to be between
 *
 *      - `'0'`–`'9'`,
 *      - `'a'`-`'f'`, or
 *      - `'A'`-`'F'`
 *
 *     .
 *
 * @param character This argument specifies the (hexadecimal) character this function converts.
 *
 * @return An integer number between `0` and `15` if the precondition is valid or `0` otherwise
 */
static inline int elektraHexcodeConvFromHex (char character)
{
	if (character >= '0' && character <= '9') return character - '0';
	if (character >= 'a' && character <= 'f') return character - 'a' + 10;
	if (character >= 'A' && character <= 'F') return character - 'A' + 10;

	return 0; /* Unknown escape char */
}

using namespace ckdb;
extern "C" {

void setDefaultConfig (CCodeData * mapping)
{
	mapping->encode['\b'_uc] = 'b'_uc;
	mapping->encode['\t'_uc] = 't'_uc;
	mapping->encode['\n'_uc] = 'n'_uc;
	mapping->encode['\v'_uc] = 'v'_uc;
	mapping->encode['\f'_uc] = 'f'_uc;
	mapping->encode['\r'_uc] = 'r'_uc;
	mapping->encode['\\'_uc] = '\\'_uc;
	mapping->encode['\''_uc] = '\''_uc;
	mapping->encode['\"'_uc] = '"'_uc;
	mapping->encode['\0'_uc] = '0'_uc;

	mapping->decode['b'_uc] = '\b'_uc;
	mapping->decode['t'_uc] = '\t'_uc;
	mapping->decode['n'_uc] = '\n'_uc;
	mapping->decode['v'_uc] = '\v'_uc;
	mapping->decode['f'_uc] = '\f'_uc;
	mapping->decode['r'_uc] = '\r'_uc;
	mapping->decode['\\'_uc] = '\\'_uc;
	mapping->decode['\''_uc] = '\''_uc;
	mapping->decode['"'_uc] = '\"'_uc;
	mapping->decode['0'_uc] = '\0'_uc;
}

void readConfig (CCodeData * const mapping, KeySet * const config, Key const * const root)
{
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
		mapping->encode[res & 255] = val;
		mapping->decode[val & 255] = res;
	}
}

int elektraCcodeOpen (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CCodeData * mapping = new CCodeData ();

	/* Store for later use...*/
	elektraPluginSetData (handle, mapping);

	KeySet * config = elektraPluginGetConfig (handle);

	Key * escape = ksLookupByName (config, "/escape", 0);
	mapping->escape = '\\';
	if (escape && keyGetBaseNameSize (escape) && keyGetValueSize (escape) == 3)
	{
		int res;
		res = elektraHexcodeConvFromHex (keyString (escape)[1]);
		res += elektraHexcodeConvFromHex (keyString (escape)[0]) * 16;

		mapping->escape = res & 255;
	}
	ELEKTRA_LOG_DEBUG ("Use “%c” as escape character", mapping->escape);

	Key * root = ksLookupByName (config, "/chars", 0);

	if (root)
	{
		readConfig (mapping, config, root);
	}
	else
	{
		setDefaultConfig (mapping);
	}

	return 0;
}

int elektraCcodeClose (Plugin * handle, Key * key ELEKTRA_UNUSED)
{
	CCodeData * mapping = static_cast<CCodeData *> (elektraPluginGetData (handle));

	delete[](mapping->buf);
	delete (mapping);

	return 0;
}

/** Reads the value of the key and decodes all escaping
 * codes into the buffer.
 * @pre the buffer needs to be as large as value's size.
 * @param cur the key holding the value to decode
 * @param mapping the buffer to write to
 */
void elektraCcodeDecode (Key * cur, CCodeData * mapping)
{
	size_t valsize = keyGetValueSize (cur);
	const char * val = static_cast<const char *> (keyValue (cur));

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char character = val[in];

		if (character == mapping->escape)
		{
			++in; /* Advance twice */
			character = val[in];

			mapping->buf[out] = mapping->decode[character & 255];
		}
		else
		{
			mapping->buf[out] = character;
		}
		++out; /* Only one char is written */
	}

	mapping->buf[out] = 0; // null termination for keyString()

	keySetRaw (cur, mapping->buf, out + 1);
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

	CCodeData * mapping = static_cast<CCodeData *> (elektraPluginGetData (handle));
	if (!mapping->buf)
	{
		mapping->bufalloc = 1000;
		mapping->buf = new unsigned char[mapping->bufalloc];
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		size_t valsize = keyGetValueSize (cur);
		if (valsize > mapping->bufalloc)
		{
			mapping->bufalloc = valsize;
			mapping->buf = new unsigned char[mapping->bufalloc];
		}

		elektraCcodeDecode (cur, mapping);
	}

	return 1; /* success */
}


/** Reads the value of the key and encodes it in
 * c-style in the buffer.
 *
 * @param cur the key which value is to encode
 * @param mapping the buffer
 * @pre the buffer needs to have twice as much space as the value's size
 */
void elektraCcodeEncode (Key * cur, CCodeData * mapping)
{
	size_t valsize = keyGetValueSize (cur);
	const char * val = static_cast<const char *> (keyValue (cur));

	if (!val) return;

	size_t out = 0;
	for (size_t in = 0; in < valsize - 1; ++in)
	{
		unsigned char character = val[in];

		if (mapping->encode[character])
		{
			mapping->buf[out + 1] = mapping->encode[character];
			// Escape char
			mapping->buf[out] = mapping->escape;
			out += 2;
		}
		else
		{
			// just copy one character
			mapping->buf[out] = val[in];
			// advance out cursor
			out++;
			// go to next char
		}
	}

	mapping->buf[out] = 0; // null termination for keyString()

	keySetRaw (cur, mapping->buf, out + 1);
}


int elektraCcodeSet (Plugin * handle, KeySet * returned, Key * parentKey ELEKTRA_UNUSED)
{
	/* set all keys */
	CCodeData * mapping = static_cast<CCodeData *> (elektraPluginGetData (handle));
	if (!mapping->buf)
	{
		mapping->bufalloc = 1000;
		mapping->buf = new unsigned char[mapping->bufalloc];
	}

	Key * cur;
	ksRewind (returned);
	while ((cur = ksNext (returned)) != 0)
	{
		size_t valsize = keyGetValueSize (cur);
		if (valsize * 2 > mapping->bufalloc)
		{
			mapping->bufalloc = valsize * 2;
			mapping->buf = new unsigned char[mapping->bufalloc];
		}

		elektraCcodeEncode (cur, mapping);
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
