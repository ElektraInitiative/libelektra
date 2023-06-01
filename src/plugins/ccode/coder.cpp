/**
 * @file
 *
 * @brief Implementation of string encoding and decoding class
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <vector>

#include "./coder.hpp"

namespace
{
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
int elektraHexcodeConvFromHex (char const character)
{
	if (character >= '0' && character <= '9') return character - '0';
	if (character >= 'a' && character <= 'f') return character - 'a' + 10;
	if (character >= 'A' && character <= 'F') return character - 'A' + 10;

	return 0; /* Unknown escape char */
}
} // end namespace

namespace elektra
{
using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

/**
 * @brief This function sets default values for the encoding and decoding character mapping.
 */
void Coder::setDefaultConfig ()
{
	unsigned char pairs[][2] = { { '\b', 'b' }, { '\t', 't' },  { '\n', 'n' },  { '\v', 'v' }, { '\f', 'f' },
				     { '\r', 'r' }, { '\\', '\\' }, { '\'', '\'' }, { '\"', '"' }, { '\0', '0' } };

	for (size_t pair = 0; pair < sizeof (pairs) / sizeof (pairs[0]); pair++)
	{
		unsigned char character = pairs[pair][0];
		unsigned char replacement = pairs[pair][1];

		encode[character] = replacement;
		decode[replacement] = character;
	}
}

/**
 * @brief This function sets values for the encoding and decoding character mapping.
 *
 * @param config This key set stores configuration values for the character mapping.
 * @param root This key stores the root key for the character mapping stored in `config`.
 */
void Coder::readConfig (CppKeySet const & config, CppKey const & root)
{
	for (auto key : config)
	{
		/* Ignore keys that are not directly below the config root key or have an incorrect size */
		if (!key->isDirectBelow (root) || key->getBaseNameSize () != 3 || key->getBinarySize () != 3) continue;

		int character = elektraHexcodeConvFromHex (key->getBaseName ()[1]);
		character += elektraHexcodeConvFromHex (key->getBaseName ()[0]) * 16;

		int replacement = elektraHexcodeConvFromHex (key->get<string> ()[1]);
		replacement += elektraHexcodeConvFromHex (key->get<string> ()[0]) * 16;

		/* Hexencode this character! */
		encode[character & 255] = replacement;
		decode[replacement & 255] = character;
	}
}

/**
 * @brief This function replaces unescaped characters in a string with escaped characters.
 *
 * @param text This variable stores the string this function escapes.
 *
 * @return The encoded string
 */
string Coder::encodeString (string const & text)
{
	vector<unsigned char> encoded;
	encoded.reserve (text.size ());

	for (unsigned char character : text)
	{
		const auto & encodedChar = encode[character];
		if (encodedChar == '\0')
		{
			encoded.push_back (character);
		}
		else
		{
			encoded.push_back (escapeCharacter);
			encoded.push_back (encodedChar);
		}
	}

	return { encoded.begin (), encoded.end () };
}

/**
 * @brief This function replaces escaped characters in a string with unescaped characters.
 *
 * @param text This string stores the string this function decodes.
 *
 * @return The decoded string
 */
string Coder::decodeString (string const & text)
{
	vector<char> decoded;
	decoded.reserve (text.size () / 2); // minimum required size, if everything is escaped

	auto character = text.begin ();

	while (character != text.end ())
	{
		if (*character == escapeCharacter)
		{
			++character;
			decoded.push_back (decode[*character]);
		}
		else
		{
			decoded.push_back (*character);
		}
		++character;
	}

	return { decoded.begin (), decoded.end () };
}

/**
 * @brief This function replaces unescaped characters in a key value with escaped characters.
 *
 * The function only modifies the key value if it has type `string`.
 *
 * @param key This key stores the value this function encodes.
 */
void Coder::encodeValue (CppKey & key)
{
	if (key.isString ()) key.setString (encodeString (key.get<string> ()));
}

/**
 * @brief This function replaces escaped characters in a key value with unescaped characters.
 *
 * The function only modifies the key value if it has type `string`.
 *
 * @param key This key holds the value this function decodes.
 */
void Coder::decodeValue (CppKey & key)
{
	if (key.isString ()) key.setString (decodeString (key.get<string> ()));
}

/**
 * @brief This function replaces unescaped characters in a key name with escaped characters.
 *
 * @param key This `Key` stores a name possibly containing unescaped special characters.
 *
 * @return A copy of `key` containing an escaped version of the name of `key`
 */
CppKey Coder::encodeName (CppKey const & key)
{
	CppKey escaped = key.dup ();
	auto nspace = key.getNamespace ();
	escaped.setName ("/");
	escaped.setNamespace (nspace);
	auto keyIterator = key.begin ();

	while (++keyIterator != key.end ())
	{
		escaped.addBaseName (encodeString (*keyIterator));
	}
	ELEKTRA_LOG_DEBUG ("Encoded name of “%s” is “%s”", key.getName ().c_str (), escaped.getName ().c_str ());
	return escaped;
}

/**
 * @brief This function replaces escaped characters in a key name with unescaped characters.
 *
 * @param key This `Key` stores a name possibly containing escaped special characters.
 *
 * @return A copy of `key` containing an unescaped version of the name of `key`
 */
CppKey Coder::decodeName (CppKey const & key)
{
	CppKey unescaped = key.dup ();
	auto nspace = key.getNamespace ();
	unescaped.setName ("/");
	unescaped.setNamespace (nspace);
	auto keyIterator = key.begin ();

	while (++keyIterator != key.end ())
	{
		unescaped.addBaseName (decodeString (*keyIterator));
	}
	ELEKTRA_LOG_DEBUG ("Decoded name of “%s” is “%s”", key.getName ().c_str (), unescaped.getName ().c_str ());
	return unescaped;
}

/**
 * @brief This constructor creates a new object used to decode and encode key sets.
 *
 * @param config This key set contains configuration values for decoding and encoding such as mapping data and the escape character.
 */
Coder::Coder (CppKeySet config)
{
	encode = vector<unsigned char> (256);
	decode = vector<unsigned char> (256);

	escapeCharacter = '\\';
	CppKey const escape = config.lookup ("/escape", 0);
	if (escape && escape.getBaseNameSize () && escape.getBinarySize () == 3)
	{
		int escapeChar = elektraHexcodeConvFromHex (escape.get<string> ()[1]);
		escapeChar += elektraHexcodeConvFromHex (escape.get<string> ()[0]) * 16;

		escapeCharacter = escapeChar & 255;
	}
	ELEKTRA_LOG_DEBUG ("Use “%c” as escape character", escapeCharacter);

	CppKey const root = CppKey{ "/chars", KEY_END };
	CppKeySet mappingConfig{ config.cut (root) };

#ifdef HAVE_LOGGER
	for (auto key : mappingConfig)
	{
		ELEKTRA_LOG_DEBUG ("Decode 0x%s as 0x%s ", key->getBaseName ().c_str (), key.getString ().c_str ());
	}
#endif

	if (mappingConfig.size () > 0)
	{
		readConfig (mappingConfig, root);
	}
	else
	{
		setDefaultConfig ();
	}
}

/**
 * @brief This function escapes special characters in key names and values.
 *
 * @param keys This key set stores keys that possibly contain special characters.
 *
 * @return A copy of the given KeySet containing only escaped keys
 */
CppKeySet Coder::encodeKeySet (CppKeySet const & keys)
{
	CppKeySet escaped{};
	for (auto key : keys)
	{
		CppKey encoded = encodeName (*key);
		encodeValue (encoded);
		escaped.append (encoded);
	}
	return escaped;
}

/**
 * @brief This function unescapes characters in key names and values.
 *
 * @param keys This key set stores keys that possibly contain escaped special characters.
 *
 * @return A copy of the given KeySet containing unescaped keys
 */
CppKeySet Coder::decodeKeySet (CppKeySet const & keys)
{
	CppKeySet unescaped{};
	for (auto key : keys)
	{
		CppKey decoded = decodeName (*key);
		decodeValue (decoded);
		unescaped.append (decoded);
	}

	return unescaped;
}

} // end namespace elektra
