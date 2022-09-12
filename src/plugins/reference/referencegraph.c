#include "referencegraph.h"
#include <kdb.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <stdlib.h>

struct _RefGraph
{
	ElektraKeyset * inner;
	ElektraKeyset * leaves;
};

RefGraph * rgNew (void)
{
	RefGraph * graph = elektraCalloc (sizeof (struct _RefGraph));
	graph->inner = elektraKeysetNew (0, ELEKTRA_KS_END);
	graph->leaves = elektraKeysetNew (0, ELEKTRA_KS_END);
	return graph;
}

RefGraph * rgDup (const RefGraph * source)
{
	RefGraph * graph = elektraCalloc (sizeof (struct _RefGraph));
	graph->inner = elektraKeysetDup (source->inner);
	graph->leaves = elektraKeysetDup (source->leaves);
	return graph;
}

bool rgHasLeaf (const RefGraph * graph)
{
	return elektraKeysetGetSize (graph->leaves) > 0;
}

bool rgContains (const RefGraph * graph, const char * nodeName)
{
	return elektraKeysetLookupByName (graph->inner, nodeName, 0) != NULL;
}

bool rgEmpty (const RefGraph * graph)
{
	return elektraKeysetGetSize (graph->inner) == 0 && elektraKeysetGetSize (graph->leaves) == 0;
}

bool rgAddEdge (RefGraph * graph, const char * fromNode, const char * toNode)
{
	ElektraKey * node = elektraKeysetLookupByName (graph->leaves, fromNode, ELEKTRA_KDB_O_POP);
	if (node != NULL)
	{
		elektraKeySetMeta (node, "last", "#0");
		elektraKeySetMeta (node, "#0", toNode);

		elektraKeysetAppendKey (graph->inner, node);
		return true;
	}

	node = elektraKeysetLookupByName (graph->inner, fromNode, 0);
	if (node == NULL)
	{
		return false;
	}

	ElektraKey * lastKey = elektraKeyDup (elektraKeyGetMeta (node, "last"), ELEKTRA_KEY_CP_ALL);
	if (elektraArrayIncName (lastKey) < 0)
	{
		elektraKeyDel (lastKey);
		return false;
	}

	elektraKeySetMeta (node, "last", elektraKeyName (lastKey));
	elektraKeySetMeta (node, elektraKeyName (lastKey), toNode);
	elektraKeyDel (lastKey);

	return true;
}

void rgAddNode (RefGraph * graph, const char * nodeName)
{
	ElektraKey * node = elektraKeyNew (nodeName, ELEKTRA_KEY_END);
	elektraKeysetAppendKey (graph->leaves, node);
}

const char * rgGetEdge (RefGraph * graph, const char * fromNode, int index)
{
	ElektraKey * node = elektraKeysetLookupByName (graph->inner, fromNode, 0);
	if (node == NULL)
	{
		return NULL;
	}

	char elem[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (elem, index);

	const ElektraKey * k = elektraKeyGetMeta (node, elem);
	return k == NULL ? NULL : elektraKeyString (k);
}

void rgRemoveLeaves (RefGraph * graph)
{
	ElektraKeyset * newLeaves = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * newInner = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * cur;
	while ((cur = elektraKeysetPop (graph->inner)) != NULL)
	{
		const char * last = elektraKeyString (elektraKeyGetMeta (cur, "last"));
		last++;
		while (*last == '_')
		{
			last++;
		}
		long size = strtol (last, NULL, 10);

		char elem[ELEKTRA_MAX_ARRAY_SIZE];
		for (int i = 0; i < size; ++i)
		{
			elektraWriteArrayNumber (elem, i);
			const ElektraKey * toNode = elektraKeyGetMeta (cur, elem);
			if (elektraKeysetLookupByName (graph->leaves, elektraKeyString (toNode), 0) != NULL)
			{
				elektraKeySetMeta (cur, elem, NULL);
			}
		}

		int write = 0;
		for (int read = 0; read < size; ++read, ++write)
		{
			elektraWriteArrayNumber (elem, read);

			const ElektraKey * toNode;
			while ((toNode = elektraKeyGetMeta (cur, elem)) == NULL && read < size)
			{
				read++;
			}

			if (read >= size)
			{
				break;
			}

			elektraWriteArrayNumber (elem, write);
			elektraKeySetMeta (cur, elem, elektraKeyString (toNode));
		}
		elektraWriteArrayNumber (elem, write);
		elektraKeySetMeta (cur, "last", elem);

		if (write == 0)
		{
			elektraKeysetAppendKey (newLeaves, cur);
		}
		else
		{
			elektraKeysetAppendKey (newInner, cur);
		}

		elektraKeyDel (cur);
	}

	elektraKeysetClear (graph->leaves);
	elektraKeysetAppend (graph->leaves, newLeaves);
	elektraKeysetClear (graph->inner);
	elektraKeysetAppend (graph->inner, newInner);

	elektraKeysetDel (newLeaves);
	elektraKeysetDel (newInner);
}

void rgDel (RefGraph * graph)
{
	elektraKeysetDel (graph->inner);
	elektraKeysetDel (graph->leaves);
	elektraFree (graph);
}
