#ifndef ELEKTRA_REFERENCEGRAPH_H
#define ELEKTRA_REFERENCEGRAPH_H

#include <elektra/kdb.h>
#include <stdbool.h>

typedef struct _RefGraph RefGraph;

RefGraph * rgNew (void);

RefGraph * rgDup (const RefGraph * source);

void rgDel (RefGraph * graph);

bool rgEmpty (const RefGraph * graph);

bool rgHasLeaf (const RefGraph * graph);

bool rgContains (const RefGraph * graph, const char * nodeName);

void rgAddNode (RefGraph * graph, const char * nodeName);

bool rgAddEdge (RefGraph * graph, const char * fromNode, const char * toNode);

const char * rgGetEdge (RefGraph * graph, const char * fromNode, int index);

size_t rgGetEdgeCount (RefGraph * graph, const char * fromNode);

void rgRemoveLeaves (RefGraph * graph);

#endif // ELEKTRA_REFERENCEGRAPH_H
