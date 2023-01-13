#ifndef ELEKTRA_BUILDERS_PUBLIC_H
#define ELEKTRA_BUILDERS_PUBLIC_H

#include "../core/private.h"
#include "../core/public.h"

#include <stdlib.h>
#include <string.h>

#define ELEKTRA_KEYSET(...)                                                                                                                \
	(ElektraSetBuild ((ElektraEntry *[]){ __VA_ARGS__ }, sizeof ((ElektraEntry *[]){ __VA_ARGS__ }) / sizeof (ElektraEntry *)))

#define ELEKTRA_KEYNAME(ns_, name_)                                                                                                        \
	(ElektraName)                                                                                                                      \
	{                                                                                                                                  \
		.ns = (ns_), .name = (name_), .size = sizeof (name_)                                                                       \
	}
#define ELEKTRA_METANAME(name) ELEKTRA_KEYNAME (ELEKTRA_NS_META, (name))
#define ELEKTRA_KEYVALUE_STRING(s)                                                                                                         \
	(ElektraValue)                                                                                                                     \
	{                                                                                                                                  \
		.value = (s), .size = strlen ((s))                                                                                         \
	}
#define ELEKTRA_KEYVALUE_PTR(v)                                                                                                            \
	(ElektraValue)                                                                                                                     \
	{                                                                                                                                  \
		.value = &(v), .size = sizeof ((v))                                                                                        \
	}

/**
 * CAUTION releases reference to @p meta
 */
ElektraEntry * ElektraEntryBuild (const ElektraName * name, const ElektraValue * value, ElektraSet * meta)
{
	ElektraEntry * key = ElektraEntryNew (name);
	ElektraSetValue (key, value);
	ElektraSetInsertAll ((ElektraSet *) ElektraEntryGetMeta (key), meta);
	ElektraSetRelease (meta);
	return key;
}

ElektraEntry * ElektraEntryBuildMeta (const ElektraName * name, const ElektraValue * value)
{
	if (name->ns != ELEKTRA_NS_META)
	{
		return NULL;
	}
	return ElektraEntryBuild (name, value, NULL);
}

/**
 * DOES NOT retain new references to @p keys, instead moves existing reference into returned keyset
 */
ElektraSet * ElektraSetBuild (ElektraEntry ** keys, size_t size)
{
	ElektraSet * ks = ElektraSetNew (size);

	// build fake ElektraSet with everything that ElektraSetInsertAll needs
	// depending on ElektraSetInsertAll this may require copying/sorting keys
	ElektraSet fake = { .data = &(struct ElektraSetDataCow){
				    .array = keys,
				    .size = size,
			    } };
	ElektraSetInsertAll (ks, &fake);

	return ks;
}

#endif // ELEKTRA_BUILDERS_PUBLIC_H
