/**
 * @file node.c
 *
 * @brief Used for building a tree of a key set, which is used on writing.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include "node.h"
#include "utility.h"
#include <kdbassert.h>
#include <elektra/kdbhelper.h>
#include <elektra/kdbmeta.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static Node * createNode (Key * key, Node * parent);
static bool addChild (Node * parent, Node * child);
static Node * buildTreeTableArray (Node * parent, Key * root, KeySet * keys, elektraCursor * ksPosition);
static Node * buildTreeArray (Node * parent, Key * root, KeySet * keys, elektraCursor * ksPosition);
static void sortChildren (Node * node);
static int nodeCmpWrapper (const void * a, const void * b);
static NodeType getNodeType (Key * key);
static bool isTable (const Node * node);

Node * buildTree (Node * parent, Key * root, KeySet * keys, elektraCursor * ksPosition)
{
	ELEKTRA_ASSERT (ksPosition && *ksPosition >= 0, "ksPosition must be >=0, but wasn't");

	if (isTableArray (root))
	{
		return buildTreeTableArray (parent, root, keys, ksPosition);
	}
	else if (isArray (root))
	{
		return buildTreeArray (parent, root, keys, ksPosition);
	}
	else
	{
		Node * node = createNode (root, parent);
		Key * key;

		if (node->type != NT_LEAF)
		{
			while ((key = ksAtCursor (keys, *ksPosition)) != NULL && keyIsBelow (root, key) == 1)
			{
				++(*ksPosition);
				Node * child = buildTree (node, key, keys, ksPosition);
				if (child == NULL)
				{
					destroyTree (node);
					return NULL;
				}
				if (!addChild (node, child))
				{
					destroyTree (node);
					NULL;
				}
			}
			sortChildren (node);
		}
		return node;
	}
}

static Node * buildTreeTableArray (Node * parent, Key * root, KeySet * keys, elektraCursor * ksPosition)
{
	ELEKTRA_ASSERT (isTableArray (root), "Root must be a table array, but wasn't");
	ELEKTRA_ASSERT (ksPosition && *ksPosition >= 0, "*ksPosition must be >=0, but wasn't");
	Node * node = createNode (root, parent);

	size_t max = getArrayMax (root);
	for (size_t i = 0; i <= max; i++)
	{
		Key * elementName = keyAppendIndex (i, root);

		// Check if, we have got the array element root in the keyset
		// This happens, if comments are associated to the table array declaration in a TOML file.
		// If we have, use this key as root instead.and forward to the next key in the keyset
		if (keyCmp (ksAtCursor (keys, *ksPosition), elementName) == 0)
		{
			keyDel (elementName);
			elementName = ksAtCursor (keys, *ksPosition);
			++(*ksPosition);
		}
		Node * element = buildTree (node, elementName, keys, ksPosition);
		if (!addChild (node, element))
		{
			destroyTree (node);
			return NULL;
		}
	}
	return node;
}

static Node * buildTreeArray (Node * parent, Key * root, KeySet * keys, elektraCursor * ksPosition)
{
	ELEKTRA_ASSERT (isArray (root) && !isTableArray (root), "Root must be array, but no table array, but wasn't");
	ELEKTRA_ASSERT (ksPosition && *ksPosition >= 0, "*ksPosition must be >=0, but wasn't");

	Node * node = createNode (root, parent);

	size_t max = getArrayMax (root);
	for (size_t i = 0; i <= max; i++)
	{
		Key * elementName = keyAppendIndex (i, root);
		Key * elementKey = ksLookup (keys, elementName, 0);
		if (elementKey != NULL)
		{
			if (!isLeaf (elementKey, keys))
			{			 // true for array that contains inline tables
				++(*ksPosition); // we need to go to the first sub key of the element, since buildTree
			}			 // loops while ksAtCursor(keys, ksPosition) is below root key (and root != below root)
						 // TODO: maybe make a cheaper check for leaf, eg. test if element has a value?
			if (!addChild (node, buildTree (node, elementKey, keys, ksPosition)))
			{
				destroyTree (node);
				return NULL;
			}
		} // else { TODO: Handle array holes }

		keyDel (elementName);
	}

	Key * key;
	while ((key = ksAtCursor (keys, *ksPosition)) != NULL && keyIsBelow (root, key) == 1)
	{
		++(*ksPosition);
	}

	return node;
}

void destroyTree (Node * node)
{
	if (node != NULL)
	{
		if (node->type == NT_LIST_ELEMENT)
		{
			keyDel (node->key);
		}
		if (node->relativeName != NULL)
		{
			elektraFree (node->relativeName);
		}
		for (size_t i = 0; i < node->childCount; i++)
		{
			destroyTree (node->children[i]);
		}
		elektraFree (node->children);
		elektraFree (node);
	}
}

static Node * createNode (Key * key, Node * parent)
{
	Node * node = (Node *) elektraCalloc (sizeof (Node));
	if (node == NULL)
	{
		return NULL;
	}
	node->key = key;
	node->parent = parent;
	if (parent == NULL)
	{
		node->relativeName = NULL; // elektraStrDup ("");
		node->type = NT_ROOT;
	}
	else
	{
		node->type = getNodeType (key);
		node->relativeName = getRelativeName (parent->key, key);
		if (node->relativeName == NULL)
		{
			destroyTree (node);
			return NULL;
		}
		if (node->type == NT_SIMPLE_TABLE || node->type == NT_TABLE_ARRAY)
		{
			Node * tableAncestor = node;
			while ((tableAncestor = tableAncestor->parent) != NULL)
			{
				if (tableAncestor->type == NT_SIMPLE_TABLE || tableAncestor->type == NT_TABLE_ARRAY)
				{
					if (!(node->type == NT_SIMPLE_TABLE && tableAncestor->type == NT_TABLE_ARRAY))
					{
						size_t split = elektraStrLen (tableAncestor->relativeName) - 1;
						size_t len = split + elektraStrLen (node->relativeName) + 1;
						char * concatName = (char *) elektraCalloc (len);
						if (concatName == NULL)
						{
							destroyTree (node);
							return NULL;
						}
						strncpy (concatName, tableAncestor->relativeName, len);
						concatName[split] = '.';
						strncat (&concatName[split + 1], node->relativeName, len - split - 1);
						elektraFree (node->relativeName);
						node->relativeName = concatName;
						break;
					}
				}
			}
		}

		/*printf (">> CREATE NODE\nparent =\t%s\nkey =\t\t%s\nrelative =\t%s\ntype =\t\t%d\n",
			parent != NULL ? keyName (parent->key) : "<NONE>", keyName (key), node->relativeName, node->type);*/
	}
	return node;
}

static bool addChild (Node * parent, Node * child)
{
	if (parent->childSize == 0)
	{
		parent->childSize = 4;
		parent->children = (Node **) elektraCalloc (sizeof (Node *) * parent->childSize);
		if (parent->children == NULL)
		{
			destroyTree (parent);
			return NULL;
		}
	}
	else if (parent->childCount == parent->childSize)
	{
		parent->childSize *= 2;
		if (elektraRealloc ((void **) &parent->children, sizeof (Node *) * parent->childSize) < 0)
		{
			parent->childSize /= 2;
			return false;
		}
	}
	parent->children[parent->childCount++] = child;
	return true;
}

static void sortChildren (Node * node)
{
	if (node->childCount > 1)
	{
		qsort (node->children, node->childCount, sizeof (Node *), nodeCmpWrapper);
	}
}

static int nodeCmpWrapper (const void * a, const void * b)
{
	const Node * na = *((const Node **) a);
	const Node * nb = *((const Node **) b);
	if (!isTable (na) && isTable (nb))
	{
		return -1;
	}
	else if (!isTable (nb) && isTable (na))
	{
		return 1;
	}
	return elektraKeyCmpOrder (na->key, nb->key);
}

static bool isTable (const Node * node)
{
	return node->type == NT_SIMPLE_TABLE || node->type == NT_TABLE_ARRAY;
}

static NodeType getNodeType (Key * key)
{
	if (isArray (key))
	{
		if (isTableArray (key))
		{
			return NT_TABLE_ARRAY;
		}
		else
		{
			return NT_ARRAY;
		}
	}
	else if (isSimpleTable (key))
	{
		return NT_SIMPLE_TABLE;
	}
	else if (isInlineTable (key))
	{
		return NT_INLINE_TABLE;
	}
	else if (isArrayIndex (keyBaseName (key)))
	{
		return NT_LIST_ELEMENT;
	}
	else
	{
		return NT_LEAF;
	}
}

bool isFirstChildren (const Node * node)
{
	return node->parent != NULL && node->parent->children[0] == node;
}
