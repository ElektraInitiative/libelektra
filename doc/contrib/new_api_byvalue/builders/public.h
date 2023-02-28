#ifndef ELEKTRA_BUILDERS_PUBLIC_H
#define ELEKTRA_BUILDERS_PUBLIC_H

#include "../core/private.h"
#include "../core/public.h"

#include <stdlib.h>
#include <string.h>

#define ELEKTRA_SET(...)                                                                                                                   \
	(elektraSetBuild ((ElektraEntry *[]){ __VA_ARGS__ }, sizeof ((ElektraEntry *[]){ __VA_ARGS__ }) / sizeof (ElektraEntry *)))

#define ELEKTRA_NAME(name_)                                                                                                                \
	(ElektraName)                                                                                                                      \
	{                                                                                                                                  \
		.name = (name_), .size = sizeof (name_)                                                                                    \
	}
#define ELEKTRA_VALUE_STRING(s)                                                                                                            \
	(ElektraValue)                                                                                                                     \
	{                                                                                                                                  \
		.value = (s), .size = strlen ((s))                                                                                         \
	}
#define ELEKTRA_VALUE_PTR(v)                                                                                                               \
	(ElektraValue)                                                                                                                     \
	{                                                                                                                                  \
		.value = &(v), .size = sizeof ((v))                                                                                        \
	}
#define ELEKTRA_VALUE_NULL                                                                                                                 \
	(ElektraValue)                                                                                                                     \
	{                                                                                                                                  \
		.value = NULL, .size = 0                                                                                                   \
	}

/**
 * CAUTION calls `elektraSetDel(meta)`, i.e., destroys @p meta, if there are no external references
 */
ElektraEntry * elektraEntryBuild (ElektraNamespace ns, ElektraName name, ElektraValue value, ElektraSet * meta)
{
	ElektraEntry * key = elektraEntryNew (ns, name);
	elektraSetValue (key, value);
	elektraSetInsertAll ((ElektraSet *) elektraEntryGetMeta (key), meta);
	elektraSetDel (meta);
	return key;
}

ElektraEntry * elektraEntryBuildMeta (ElektraName name, ElektraValue value)
{
	return elektraEntryBuild (ELEKTRA_NS_META, name, value, NULL);
}

/**
 * DOES NOT retain new references to @p keys, instead moves existing reference into returned keyset
 */
ElektraSet * elektraSetBuild (ElektraEntry ** keys, size_t size)
{
	ElektraSet * ks = elektraSetNew (size);

	// build fake ElektraSet with everything that ElektraSetInsertAll needs
	// depending on ElektraSetInsertAll this may require copying/sorting keys
	ElektraSet fake = { .data = &(struct ElektraSetDataCow){
				    .array = keys,
				    .size = size,
			    } };
	elektraSetInsertAll (ks, &fake);

	return ks;
}

#endif // ELEKTRA_BUILDERS_PUBLIC_H
