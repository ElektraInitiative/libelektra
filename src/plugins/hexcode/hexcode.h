/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_HEXCODE_H
#define ELEKTRA_PLUGIN_HEXCODE_H

#include <kdbplugin.h>

typedef struct
{
	/* Which chars to hex-encode */
	char hd[256];

	char escape;

	char * buf;
	size_t bufalloc;
} CHexData;

ssize_t keySetRaw (ElektraKey * key, const void * newBinary, size_t dataSize);

void elektraHexcodeEncode (ElektraKey * cur, CHexData * hd);
void elektraHexcodeDecode (ElektraKey * cur, CHexData * hd);

int elektraHexcodeGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraHexcodeSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraHexcodeOpen (Plugin * handle, ElektraKey *);
int elektraHexcodeClose (Plugin * handle, ElektraKey * k);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif
