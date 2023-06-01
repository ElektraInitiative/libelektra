#include "./referencegraph.h"
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/array.h>
#include <internal/utility/array.h>
#include <internal/utility/alloc.h>
#include <stdlib.h>

struct _RefGraph
{
	KeySet * inner;
	KeySet * leaves;
};

RefGraph * rgNew (void)
{
	RefGraph * graph = elektraCalloc (sizeof (struct _RefGraph));
	graph->inner = ksNew (0, KS_END);
	graph->leaves = ksNew (0, KS_END);
	return graph;
}

RefGraph * rgDup (const RefGraph * source)
{
	RefGraph * graph = elektraCalloc (sizeof (struct _RefGraph));
	graph->inner = ksDup (source->inner);
	graph->leaves = ksDup (source->leaves);
	return graph;
}

bool rgHasLeaf (const RefGraph * graph)
{
	return ksGetSize (graph->leaves) > 0;
}

bool rgContains (const RefGraph * graph, const char * nodeName)
{
	return ksLookupByName (graph->inner, nodeName, 0) != NULL;
}

bool rgEmpty (const RefGraph * graph)
{
	return ksGetSize (graph->inner) == 0 && ksGetSize (graph->leaves) == 0;
}

bool rgAddEdge (RefGraph * graph, const char * fromNode, const char * toNode)
{
	Key * node = ksLookupByName (graph->leaves, fromNode, KDB_O_POP);
	if (node != NULL)
	{
		keySetMeta (node, "last", "#0");
		keySetMeta (node, "#0", toNode);

		ksAppendKey (graph->inner, node);
		return true;
	}

	node = ksLookupByName (graph->inner, fromNode, 0);
	if (node == NULL)
	{
		return false;
	}

	Key * lastKey = keyDup (keyGetMeta (node, "last"), KEY_CP_ALL);
	if (elektraArrayIncName (lastKey) < 0)
	{
		keyDel (lastKey);
		return false;
	}

	keySetMeta (node, "last", keyName (lastKey));
	keySetMeta (node, keyName (lastKey), toNode);
	keyDel (lastKey);

	return true;
}

void rgAddNode (RefGraph * graph, const char * nodeName)
{
	Key * node = keyNew (nodeName, KEY_END);
	ksAppendKey (graph->leaves, node);
}

const char * rgGetEdge (RefGraph * graph, const char * fromNode, int index)
{
	Key * node = ksLookupByName (graph->inner, fromNode, 0);
	if (node == NULL)
	{
		return NULL;
	}

	char elem[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (elem, index);

	const Key * k = keyGetMeta (node, elem);
	return k == NULL ? NULL : keyString (k);
}

void rgRemoveLeaves (RefGraph * graph)
{
	KeySet * newLeaves = ksNew (0, KS_END);
	KeySet * newInner = ksNew (0, KS_END);

	Key * cur;
	while ((cur = ksPop (graph->inner)) != NULL)
	{
		const char * last = keyString (keyGetMeta (cur, "last"));
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
			const Key * toNode = keyGetMeta (cur, elem);
			if (ksLookupByName (graph->leaves, keyString (toNode), 0) != NULL)
			{
				keySetMeta (cur, elem, NULL);
			}
		}

		int write = 0;
		for (int read = 0; read < size; ++read, ++write)
		{
			elektraWriteArrayNumber (elem, read);

			const Key * toNode;
			while ((toNode = keyGetMeta (cur, elem)) == NULL && read < size)
			{
				read++;
			}

			if (read >= size)
			{
				break;
			}

			elektraWriteArrayNumber (elem, write);
			keySetMeta (cur, elem, keyString (toNode));
		}
		elektraWriteArrayNumber (elem, write);
		keySetMeta (cur, "last", elem);

		if (write == 0)
		{
			ksAppendKey (newLeaves, cur);
		}
		else
		{
			ksAppendKey (newInner, cur);
		}

		keyDel (cur);
	}

	ksClear (graph->leaves);
	ksAppend (graph->leaves, newLeaves);
	ksClear (graph->inner);
	ksAppend (graph->inner, newInner);

	ksDel (newLeaves);
	ksDel (newInner);
}

void rgDel (RefGraph * graph)
{
	ksDel (graph->inner);
	ksDel (graph->leaves);
	elektraFree (graph);
}
