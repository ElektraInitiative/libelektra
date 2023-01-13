#include "../core/public.h"

#include <stdlib.h>

// cuts single key out of keyset
ElektraEntry * ElektraSetCut (ElektraSet * ks, size_t index)
{
	ElektraEntry * key = ElektraSetGet (ks, index);
	ElektraSetRemove (ks, index);
	return key;
}

// Migration Note: replacement for ksLookup(ks, key, KDB_O_POP) would be
/*
ElektraSetCut (ks, ElektraSetLookup (ks, ElektraEntryGetName (key)));
*/

// cuts arbitrary range out of keyset
ElektraSet * ElektraSetCutRange (ElektraSet * ks, size_t start, size_t end)
{
	ElektraSet * range = ElektraSetGetRange (ks, start, end);
	ElektraSetRemoveRange (ks, start, end);
	return range;
}

// cuts hierarchy out of keyset
// Migration Note: replaces current ksCut
ElektraSet * ElektraSetCutHierarchy (ElektraSet * ks, const ElektraName * root)
{
	size_t end;
	size_t start = ElektraSetFindHierarchy (ks, root, &end);
	return ElektraSetCutRange (ks, start, end);
}