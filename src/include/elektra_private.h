/**
 * @file
 *
 * @brief Private declarations.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PRIVATE_H
#define ELEKTRA_PRIVATE_H

#include "elektra.h"

struct _Elektra
{
	KDB * kdb;
	Key * parentKey;
	KeySet * config;
	Key * lookupKey;
	bool enforceType;
	ElektraErrorHandler fatalErrorHandler;
};

void elektraSaveKey (Elektra * elektra, Key * key, ElektraError ** error);

void elektraSetLookupKey (Elektra * elektra, const char * name);
void elektraSetArrayLookupKey (Elektra * elektra, const char * name, size_t index);

#endif // ELEKTRA_PRIVATE_H
