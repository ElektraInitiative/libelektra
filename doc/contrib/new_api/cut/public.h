#include "../core/public.h"

#include <stdlib.h>

// cuts single key out of keyset
ElektraKey * elektraKeysetCut (ElektraKeyset * ks, size_t index)
{
	ElektraKey * key = elektraKeysetGet (ks, index);
	elektraKeysetRemove (ks, index);
	return key;
}

// Migration Note: replacement for ksLookup(ks, key, KDB_O_POP) would be
/*
elektraKeysetCut (ks, elektraKeysetLookup (ks, elektraKeyGetName (key)));
*/

// cuts arbitrary range out of keyset
ElektraKeyset * elektraKeysetCutRange (ElektraKeyset * ks, size_t start, size_t end)
{
	ElektraKeyset * range = elektraKeysetGetRange (ks, start, end);
	elektraKeysetRemoveRange (ks, start, end);
	return range;
}

// cuts hierarchy out of keyset
// Migration Note: replaces current ksCut
ElektraKeyset * elektraKeysetCutHierarchy (ElektraKeyset * ks, const ElektraKeyname * root)
{
	size_t end;
	size_t start = elektraKeysetFindHierarchy (ks, root, &end);
	return elektraKeysetCutRange (ks, start, end);
}