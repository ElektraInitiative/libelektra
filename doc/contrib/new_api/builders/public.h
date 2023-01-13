#ifndef ELEKTRA_BUILDERS_PUBLIC_H
#define ELEKTRA_BUILDERS_PUBLIC_H

#include "../core/private.h"
#include "../core/public.h"

#include <stdlib.h>
#include <string.h>

#define ELEKTRA_KEYSET(...)                                                                                                                \
	(elektraKeysetBuild ((ElektraKey *[]){ __VA_ARGS__ }, sizeof ((ElektraKey *[]){ __VA_ARGS__ }) / sizeof (ElektraKey *)))

#define ELEKTRA_KEYNAME(ns_, name_)                                                                                                        \
	(ElektraKeyname)                                                                                                                   \
	{                                                                                                                                  \
		.ns = (ns_), .name = (name_), .size = sizeof (name_)                                                                       \
	}
#define ELEKTRA_METANAME(name) ELEKTRA_KEYNAME (ELEKTRA_NS_META, (name))
#define ELEKTRA_KEYVALUE_STRING(s)                                                                                                         \
	(ElektraKeyvalue)                                                                                                                  \
	{                                                                                                                                  \
		.value = (s), .size = strlen ((s))                                                                                         \
	}
#define ELEKTRA_KEYVALUE_PTR(v)                                                                                                            \
	(ElektraKeyvalue)                                                                                                                  \
	{                                                                                                                                  \
		.value = &(v), .size = sizeof ((v))                                                                                        \
	}

/**
 * CAUTION releases reference to @p meta
 */
ElektraKey * elektraKeyBuild (const ElektraKeyname * name, const ElektraKeyvalue * value, ElektraKeyset * meta)
{
	ElektraKey * key = elektraKeyNew (name);
	elektraKeySetValue (key, value);
	elektraKeysetInsertAll ((ElektraKeyset *) elektraKeyGetMeta (key), meta);
	elektraKeysetRelease (meta);
	return key;
}

ElektraKey * elektraKeyBuildMeta (const ElektraKeyname * name, const ElektraKeyvalue * value)
{
	if (name->ns != ELEKTRA_NS_META)
	{
		return NULL;
	}
	return elektraKeyBuild (name, value, NULL);
}

/**
 * DOES NOT retain new references to @p keys, instead moves existing reference into returned keyset
 */
ElektraKeyset * elektraKeysetBuild (ElektraKey ** keys, size_t size)
{
	ElektraKeyset * ks = elektraKeysetNew (size);

	// build fake ElektraKeyset with everything that elektraKeysetInsertAll needs
	// depending on elektraKeysetInsertAll this may require copying/sorting keys
	ElektraKeyset fake = { .data = &(struct ElektraKeysetDataCow){
				       .array = keys,
				       .size = size,
			       } };
	elektraKeysetInsertAll (ks, &fake);

	return ks;
}

#endif // ELEKTRA_BUILDERS_PUBLIC_H
