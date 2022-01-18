/**
 * @file node.h
 *
 * @brief Used for building a tree of a key set, which is used on writing.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_TOML_NODE_H
#define ELEKTRA_PLUGIN_TOML_NODE_H

#include <kdb.h>
#include <stdbool.h>

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

/*
 * @brief Recursively builds a tree out of the given keyset.
 *
 * The built tree contains keys as well as additional information gathered from the keys,
 * in order to make the correct writing of a TOML file easier.
 *
 * @params parent The root of the (sub-)tree to be created.
 * @params root The root key for the given key set.
 * @params keys All keys which may be a member of the tree.
 *
 * @retval Pointer The root of the created tree
 * @retval NULL On Error, the root key contains additional error information.
 * */
Node * buildTree (Node * parent, Key * root, KeySet * keys);

/*
 * @brief Frees up any memory allocated within the tree and all it's children.
 *
 * @params node Tree to be freed
 * */
void destroyTree (Node * node);

bool isFirstChildren (const Node * node);

#endif // ELEKTRA_PLUGIN_TOML_NODE_H
