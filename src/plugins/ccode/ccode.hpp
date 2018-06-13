/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_CCODE_H
#define ELEKTRA_PLUGIN_CCODE_H

#include <kdbplugin.h>

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

}

using namespace ckdb;
extern "C" {

typedef struct
{
	unsigned char encode[256];
	unsigned char decode[256];

	char escape;

	unsigned char * buffer;
	size_t bufferSize;
} CCodeData;

ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize);

void encodeKey (Key * cur, CCodeData * h);
void decodeKey (Key * cur, CCodeData * h);

int elektraCcodeOpen (Plugin * handle, Key * k);
int elektraCcodeClose (Plugin * handle, Key * k);
int elektraCcodeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCcodeSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (ccode);
} // end extern "C"

#endif
