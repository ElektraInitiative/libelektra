/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_CCODE_H
#define ELEKTRA_PLUGIN_CCODE_H

#include <kdbplugin.h>

typedef struct
{
	char encode [256];
	char decode [256];

	char escape;

	char *buf;
	size_t bufalloc;
} CCodeData;

ssize_t keySetRaw(Key *key, const void *newBinary, size_t dataSize);

void elektraCcodeEncode (Key *cur, CCodeData *h);
void elektraCcodeDecode (Key *cur, CCodeData *h);

int elektraCcodeOpen(Plugin *handle, Key *k);
int elektraCcodeClose(Plugin *handle, Key *k);
int elektraCcodeGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraCcodeSet(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(ccode);

#endif
