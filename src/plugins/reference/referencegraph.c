#include "referencegraph.h"
#include <kdb.h>
#include <kdbhelper.h>

struct _EdgeList
{
	char ** toNodes;
	size_t size;
	size_t alloc;
};

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

ssize_t rgAddEdge (RefGraph * graph, const char * fromNode, const char * toNode)
{
	Key * node = ksLookupByName (graph->leaves, fromNode, KDB_O_POP);
	if (node == NULL)
	{
		node = ksLookupByName (graph->inner, fromNode, 0);
	}
	else
	{
		struct _EdgeList edges;
		edges.size = 0;
		edges.alloc = 2;
		edges.toNodes = elektraCalloc (edges.alloc * sizeof (char *));
		keySetBinary (node, &edges, sizeof (struct _EdgeList));

		ksAppendKey (graph->inner, node);
	}
	if (node == NULL)
	{
		return -1;
	}

	struct _EdgeList edges;
	keyGetBinary (node, &edges, sizeof (struct _EdgeList));

	if (edges.size >= edges.alloc)
	{
		edges.alloc *= 2;
		elektraRealloc ((void **) &edges.toNodes, edges.alloc);
	}
	edges.toNodes[edges.size] = elektraStrDup (toNode);

	return edges.size++;
}

size_t rgGetEdgeCount (RefGraph * graph, const char * fromNode)
{
	Key * node = ksLookupByName (graph->inner, fromNode, 0);
	if (node == NULL)
	{
		return 0;
	}

	struct _EdgeList edges;
	keyGetBinary (node, &edges, sizeof (struct _EdgeList));

	return edges.size;
}

void rgAddNode (RefGraph * graph, const char * nodeName)
{
	Key * node = keyNew (nodeName, KEY_END);
	ksAppendKey (graph->leaves, node);
}

const char * rgGetEdge (RefGraph * graph, const char * fromNode, size_t index)
{
	Key * node = ksLookupByName (graph->inner, fromNode, 0);
	if (node == NULL)
	{
		return NULL;
	}

	struct _EdgeList edges;
	keyGetBinary (node, &edges, sizeof (struct _EdgeList));

	return index < edges.size ? edges.toNodes[index] : NULL;
}

void rgRemoveLeaves (RefGraph * graph)
{
	KeySet * newLeaves = ksNew (0, KS_END);
	KeySet * newInner = ksNew (0, KS_END);

	Key * cur;
	while ((cur = ksPop (graph->inner)) != NULL)
	{
		struct _EdgeList edges;
		keyGetBinary (cur, &edges, sizeof (struct _EdgeList));

		for (size_t i = 0; i < edges.size; ++i)
		{
			if (ksLookupByName (graph->leaves, edges.toNodes[i], 0) != NULL)
			{
				elektraFree (edges.toNodes[i]);
				edges.toNodes[i] = NULL;
			}
		}

		size_t write = 0;
		for (size_t read = 0; read < edges.size; ++read, ++write)
		{
			while (edges.toNodes[read] == NULL && read < edges.size)
			{
				read++;
			}

			if (read >= edges.size)
			{
				break;
			}

			edges.toNodes[write] = edges.toNodes[read];
		}
		edges.size = write;

		if (edges.size == 0)
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
}

void rgDel (RefGraph * graph)
{
	ksRewind (graph->inner);
	Key * cur;
	while ((cur = ksPop (graph->inner)) != NULL)
	{
		struct _EdgeList edges;
		keyGetBinary (cur, &edges, sizeof (struct _EdgeList));

		for (size_t i = 0; i < edges.size; ++i)
		{
			elektraFree (edges.toNodes[i]);
		}

		keyDel (cur);
	}
	ksDel (graph->inner);

	ksDel (graph->leaves);
}
