#include "../core/public.h"

#include <stdlib.h>

// cuts single key out of keyset
ElektraEntry * elektraSetCut (ElektraSet * ks, size_t index)
{
	ElektraEntry * key = elektraSetGet (ks, index);
	elektraSetRemove (ks, index);
	return key;
}

// Migration Note: replacement for ksLookup(ks, key, KDB_O_POP) would be
/*
elektraSetCut (ks, elektraSetLookup (ks, elektraEntryGetName (key)));
*/

// cuts arbitrary range out of keyset
ElektraSet * elektraSetCutRange (ElektraSet * ks, size_t start, size_t end)
{
	ElektraSet * range = elektraSetGetRange (ks, start, end);
	elektraSetRemoveRange (ks, start, end);
	return range;
}

// cuts hierarchy out of keyset
// Migration Note: replaces current ksCut
/**
 * @note consumes @p root, i.e., calls `elektraNameDel (root)`.
 *       This means the function is safe to call as `elektraSetCutHierarchy (ks, elektraEntryGetName (entry))`
 *       or as `elektraSetCutHierarchy (ks, elektraNameNew (ns, name, size))`, but when you use
 *       `elektraSetCutHierarchy (ks, root)` you may need to `elektraNameIncRef (root)` & `elektraNameDecRef (root)`.
 */
ElektraSet * elektraSetCutHierarchy (ElektraSet * ks, const ElektraName * root)
{
	size_t end;
	size_t start = elektraSetFindHierarchy (ks, root, &end);
	return elektraSetCutRange (ks, start, end);
}