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

#ifdef __cplusplus
using namespace ckdb;
extern "C" {
#endif

typedef struct
{
	unsigned char encode[256];
	unsigned char decode[256];

	char escape;

	unsigned char * buffer;
	size_t bufalloc;
} CCodeData;

ssize_t keySetRaw (Key * key, const void * newBinary, size_t dataSize);

void elektraCcodeEncode (Key * cur, CCodeData * h);
void elektraCcodeDecode (Key * cur, CCodeData * h);

int elektraCcodeOpen (Plugin * handle, Key * k);
int elektraCcodeClose (Plugin * handle, Key * k);
int elektraCcodeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCcodeSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (ccode);
#ifdef __cplusplus
} // end extern "C"
#endif

#endif
