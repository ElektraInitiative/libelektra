/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "ccode.hpp"

#include <kdblogger.h>

namespace
{

/**
 * @brief Cast a character to an unsigned character.
 *
 * @param character This parameter specifies the character this function casts to an unsigned value.
 *
 * @return A unsigned character corresponding to the given argument
 */
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
inline int elektraHexcodeConvFromHex (char character)
{
	if (character >= '0' && character <= '9') return character - '0';
	if (character >= 'a' && character <= 'f') return character - 'a' + 10;
	if (character >= 'A' && character <= 'F') return character - 'A' + 10;

	return 0; /* Unknown escape char */
}

} // end namespace

using namespace ckdb;
extern "C" {

/**
 * @brief This function sets default values for the encoding and decoding character mapping.
 *
 * @param mapping This parameter stores the encoding and decoding table this function fills with default values.
 */
void setDefaultConfig (CCodeData * const mapping)
{
	unsigned char pairs[][2] = { { '\b'_uc, 'b'_uc }, { '\t'_uc, 't'_uc }, { '\n'_uc, 'n'_uc },  { '\v'_uc, 'v'_uc },
				     { '\f'_uc, 'f'_uc }, { '\r'_uc, 'r'_uc }, { '\\'_uc, '\\'_uc }, { '\''_uc, '\''_uc },
				     { '\"'_uc, '"'_uc }, { '\0'_uc, '0'_uc } };

	for (size_t pair = 0; pair < sizeof (pairs) / sizeof (pairs[0]); pair++)
	{
		unsigned char character = pairs[pair][0];
		unsigned char replacement = pairs[pair][1];

		mapping->encode[character] = replacement;
		mapping->decode[replacement] = character;
	}
}

/**
 * @brief This function sets values for the encoding and decoding character mapping.
 *
 * @param mapping This parameter stores the encoding and decoding table this function fills with the values specified in `config`.
 * @param config This key set stores the character mappings this function stores inside `mappings`.
 * @param root This key stores the root key for the character mapping stored in `config`.
 */
void readConfig (CCodeData * const mapping, KeySet * const config, Key const * const root)
{
	Key const * key = 0;
	while ((key = ksNext (config)) != 0)
	{
		/* Ignore keys that are not directly below the config root key or have an incorrect size */
		if (keyRel (root, key) != 1 || keyGetBaseNameSize (key) != 3 || keyGetValueSize (key) != 3) continue;

		int character = elektraHexcodeConvFromHex (keyBaseName (key)[1]);
		character += elektraHexcodeConvFromHex (keyBaseName (key)[0]) * 16;

		int replacement = elektraHexcodeConvFromHex (keyString (key)[1]);
		replacement += elektraHexcodeConvFromHex (keyString (key)[0]) * 16;

		/* Hexencode this character! */
		mapping->encode[character & 255] = replacement;
		mapping->decode[replacement & 255] = character;
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
