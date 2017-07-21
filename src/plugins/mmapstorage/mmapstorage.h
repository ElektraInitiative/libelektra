/**
 * @file
 *
 * @brief Header for mmapstorage plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MMAPSTORAGE_H
#define ELEKTRA_PLUGIN_MMAPSTORAGE_H

#include <kdbplugin.h>

typedef struct _mmapInfo MmapInfo;

struct _mmapInfo
{
	char * addr;
};

typedef struct _mmapSize MmapSize;

struct _mmapSize
{
	size_t mmapSize;
	size_t ksSize;
	size_t metaKeys;
};

typedef struct _dynArray DynArray;

struct _dynArray
{
	size_t size;
	int * array;
};


int elektraMmapstorageOpen (Plugin * handle, Key * errorKey);
int elektraMmapstorageClose (Plugin * handle, Key * errorKey);
int elektraMmapstorageGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMmapstorageCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (mmapstorage);

#endif
