#ifndef ELEKTRA_HIGHLEVEL_ELEKTRA_H
#define ELEKTRA_HIGHLEVEL_ELEKTRA_H

#include <elektra/highlevel.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>

struct _Elektra
{
	KDB * kdb;
	Key * parentKey;
	KeySet * config;
	KeySet * defaults;
	Key * lookupKey;
	ElektraErrorHandler fatalErrorHandler;
	char * resolvedReference;
	size_t parentKeyLength;
};

struct _ElektraError
{
	char * code;
	char * codeFromKey;
	char * description;
	char * module;
	char * file;
	kdb_long_t line;
	kdb_long_t warningCount;
	kdb_long_t warningAlloc;
	struct _ElektraError ** warnings;
	Key * errorKey;
};

/* high-level API */
void elektraSaveKey (Elektra * elektra, Key * key, ElektraError ** error);
void elektraSetLookupKey (Elektra * elektra, const char * name);
void elektraSetArrayLookupKey (Elektra * elektra, const char * name, kdb_long_long_t index);

ElektraError * elektraErrorCreate (const char * code, const char * description, const char * module, const char * file, kdb_long_t line);
void elektraErrorAddWarning (ElektraError * error, ElektraError * warning);
ElektraError * elektraErrorFromKey (Key * key);

ElektraError * elektraErrorKeyNotFound (const char * keyname);
ElektraError * elektraErrorWrongType (const char * keyname, KDBType expectedType, KDBType actualType);
ElektraError * elektraErrorNullError (const char * function);
ElektraError * elektraErrorEnsureFailed (const char * reason);

#endif // ELEKTRA_HIGHLEVEL_ELEKTRA_H
