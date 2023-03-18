/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_HEXCODE_H
#define ELEKTRA_PLUGIN_HEXCODE_H

#include <elektra/plugin/plugin.h>

typedef struct
{
	/* Which chars to hex-encode */
	char hd[256];

	char escape;

	char * buf;
	size_t bufalloc;
} CHexData;

ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize);

void elektraHexcodeEncode (Key * cur, CHexData * hd);
void elektraHexcodeDecode (Key * cur, CHexData * hd);

int elektraHexcodeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexcodeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexcodeOpen (Plugin * handle, Key *);
int elektraHexcodeClose (Plugin * handle, Key * k);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
