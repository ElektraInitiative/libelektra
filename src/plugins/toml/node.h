#ifndef ELEKTRA_PLUGIN_TOML_NODE_H
#define ELEKTRA_PLUGIN_TOML_NODE_H

#include <kdb.h>

typedef enum
{
	NT_ARRAY,
	NT_SIMPLE_TABLE,
	NT_INLINE_TABLE,
	NT_TABLE_ARRAY,
	NT_LIST_ELEMENT,
	NT_LEAF,
	NT_ROOT
} NodeType;

typedef struct Node_
{
	Key * key;
	NodeType type;
	struct Node_ * parent;
	char * relativeName;
	size_t childCount;
	size_t childSize;
	struct Node_ ** children;
} Node;


Node * buildTree (Node * parent, Key * root, KeySet * keys);
void destroyTree (Node * node);

#endif // ELEKTRA_PLUGIN_TOML_NODE_H
