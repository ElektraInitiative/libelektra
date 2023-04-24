/******************************************************************************
 * Nickel - a library for hierarchical maps and .ini files
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: nickel.c 339 2008-01-18 19:27:01Z chaz $
 ******************************************************************************/


#include "internal.h"
#include <bohr/ds_hash.h>
#include <bohr/ni.h>

#include <internal/macros/attributes.h>
#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


// How many child buckets to pre-allocate for each node.  32 seems appropriate
// because each bucket is only a pointer in size, and resizing the table is
// time-consuming.
#define INITIAL_BUCKETS 32

// How big to initialize the buffer for reading values in elektraNi_ReadStream().
#define INITIAL_VALUE_BUFFER 1024


// A node in our Ni tree.
struct elektraNi_node_struct
{
	struct elektraNi_node_struct * root;   // root node in this tree (ALWAYS set for valid nodes)
	struct elektraNi_node_struct * parent; // the immediate parent of this node (set for all except root)

	char name[elektraNi_KEY_SIZE]; // this node's name (set for all except root)
	int name_len;		       // strlen of name
	Ds_hash_t hash;		       // the hash value of name and thus this node

	Ds_str value; // this node's value (only set for nodes that have a value)
	int modified; // whether this value has been "modified", which the application can use however they want

	Ds_hash_table children; // a hash table of children
};
#define NODE_STRUCT_INIT                                                                                                                   \
	{                                                                                                                                  \
		NULL, NULL, { '\0' }, 0, Ds_HASH_C (0), Ds_STR_INIT, 0, Ds_HASH_TABLE_INIT                                                 \
	}


// Returns a Ds_hash_entry's item as a elektraNi_node.
#define GetItem(e) ((elektraNi_node) ((e)->item))

// Returns the Ds_hash_entry the node belongs to.
#define GetEntry(n) ((Ds_hash_entry *) ((unsigned char *) (n) -offsetof (Ds_hash_entry, item)))

// Inits the node and adds it as a child of the parent.
static elektraNi_node AddNode (elektraNi_node restrict n, elektraNi_node restrict parent, const char * restrict name, int name_len,
			       Ds_hash_t hash);

// Initializes internals of a node.
static int InitNode (elektraNi_node restrict n, elektraNi_node restrict parent);

// Frees internals of a node.
static void FreeNode (elektraNi_node restrict n);

// Frees internals of this node and all its children.
static void RecursiveFree (elektraNi_node restrict n);

// Sets the modified state for this node and all its children.
static void RecursiveSetModified (elektraNi_node restrict n, int modified);

// Recursively outputs to the stream.
static int RecursiveWrite (elektraNi_node restrict n, FILE * restrict stream, int modified_only, int level);

// Compares two hash entries.
static int Compare (const void * restrict key, size_t key_size, const void * restrict item, size_t item_size);


/* Returns the version of Ni this library was compiled with.  Compare this
 * value to elektraNi_VERSION to see if the header matches.
 */
elektraNi_PUBLIC uint32_t elektraNi_GetVersion (void)
{
	return elektraNi_HEADER_VERSION;
}

/* Allocates an entirely new node, not connected to any others, and returns it.
 * This new node is the root of ... well, so far, just itself, but anything you
 * add to it will be its children.  Returns NULL if it fails.  Note that to
 * allocate a node that will be a child of another node, you must use
 * elektraNi_GetChild() instead of this function; this only allocates entirely new
 * trees.
 */
elektraNi_PUBLIC elektraNi_node elektraNi_New (void)
{
	elektraNi_node n;

	if ((n = (elektraNi_node) malloc (sizeof (struct elektraNi_node_struct))) != NULL)
	{
		if (!InitNode (n, NULL))
		{
			elektraFree (n);
			n = NULL;
		}
	}

	return n;
}

/* Frees a node and any of the node's children, and their children, etc.  It
 * also checks if it has a parent and severs itself as a branch, so the entire
 * tree is kept synchronized.
 */
elektraNi_PUBLIC void elektraNi_Free (elektraNi_node restrict n)
{
	if (n)
	{
		RecursiveFree (n);

		// if it was root, it was created with malloc, so free it
		if (n == n->root)
		{
			elektraFree (n);
		}
		else
		{
			// otherwise, we just remove it from its parent's hash table
			assert (n->parent != NULL);
			if (!Ds_RemoveHashEntry (&n->parent->children, GetEntry (n)))
				assert (!"Ds_RemoveHashEntry() should never fail in this case!");
			// more descriptive than assert(0)
		}
	}
}

/* Returns the name string of a node, or NULL if you pass an invalid node or a
 * root node (roots can't have names).  If len_out is non-NULL, it sticks the
 * returned string's length in len_out.  Note that all nodes have a name except
 * the root--but names can be "".
 */
elektraNi_PUBLIC const char * elektraNi_GetName (elektraNi_node restrict n, int * restrict len_out)
{
	const char * name = NULL;
	int len = 0;

	if (n && n != n->root)
	{
		name = n->name;
		len = n->name_len;
	}

	if (len_out)
	{
		*len_out = len;
	}
	return name;
}

/* Returns the root of the tree the node belongs to.  You can check if a node
 * is a root node if n == elektraNi_GetRoot(n) is nonzero.  Returns NULL if you pass
 * an invalid node.
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetRoot (elektraNi_node restrict n)
{
	return (n ? n->root : NULL);
}

/* Returns the parent node of a node, or NULL if you pass an invalid node or a
 * root node (as they have no parent).
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetParent (elektraNi_node restrict n)
{
	return (n ? n->parent : NULL);
}

/* Returns the number of children a node has, or 0 if you pass an invalid node.
 */
elektraNi_PUBLIC int elektraNi_GetNumChildren (elektraNi_node restrict n)
{
	return (n ? n->children.num : 0);
}

/* Useful for enumerating children--pass NULL as child to retrieve the node's
 * first child, then pass the previous return value as child to get the next,
 * etc.  Returns NULL if there's an error or there are no more children.
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetNextChild (elektraNi_node restrict n, elektraNi_node restrict child)
{
	elektraNi_node next = NULL;
	Ds_hash_entry * e;

	if (n)
	{
		if ((e = Ds_NextHashEntry (&n->children, (child ? GetEntry (child) : NULL))) != NULL) next = GetItem (e);
	}

	return next;
}

/* Returns the named child node of the node.  Specify the length of the name in
 * name_len, or use a negative number to have it calculated for you using
 * strlen().  Note that a NULL name is treated as "".  If add_if_new is
 * nonzero, if the named node is not found, it will be allocated as a child of
 * the node and returned.  You can see whether it was found or added if you
 * specify a non-NULL added_out.  Returns NULL if the node wasn't found AND
 * either add_if_new was 0 or it failed to allocate a new node (in either case,
 * added_out will be 0).
 */
elektraNi_PUBLIC elektraNi_node elektraNi_GetChild (elektraNi_node restrict n, const char * restrict name, int name_len, int add_if_new,
						    int * restrict added_out)
{
	elektraNi_node child = NULL;
	struct elektraNi_node_struct c;
	Ds_hash_entry * e;
	int added = 0;
	Ds_hash_t hash;

	if (n)
	{
		if (!name)
		{
			name = "";
		}
		if (name_len < 0)
		{
			name_len = strlen (name);
		}
		if (name_len > elektraNi_KEY_SIZE - 1) name_len = elektraNi_KEY_SIZE - 1;

		hash = Hash (name, (size_t) name_len, 0xbadc0de5);

		if ((e = Ds_SearchHashTable (&n->children, name, name_len, hash, Compare)) != NULL)
			child = GetItem (e);
		else if (add_if_new)
		{
			if ((child = AddNode (&c, n, name, name_len, hash)) != NULL) added = 1;
		}
	}

	if (added_out)
	{
		*added_out = added;
	}
	return child;
}

/* Returns the modified state of a node.  When nodes are created, they are "not
 * modified".  As soon as you call a elektraNi_SetValue() (or elektraNi_ValuePrint())
 * function, its modified state changes to "modified".  Use this to check
 * whether it's modified or not.  You can tell elektraNi_WriteFile() or -Stream() to
 * only write modified values--this is useful for having a global options file
 * with a local override file (you'd read the global options in, set all nodes
 * to "not modified", then read in the local options over top of it, and any
 * values the local file set will then be "modified", and when you write it
 * out, you only get the options set by the override file or ones you set since
 * it was loaded).
 */
elektraNi_PUBLIC int elektraNi_GetModified (elektraNi_node restrict n)
{
	return (n ? n->modified : 0);
}

/* Explicitly sets the modified state for a node, and if recurse is nonzero,
 * all the node's children and their children, etc.  See the note in the above
 * function how this is useful.
 */
elektraNi_PUBLIC void elektraNi_SetModified (elektraNi_node restrict n, int modified, int recurse)
{
	if (n)
	{
		if (!recurse)
		{
			n->modified = modified;
		}
		else
		{
			RecursiveSetModified (n, modified);
		}
	}
}

/* Returns a node's value.  Any node except a root node can have a value, but
 * not all of them do.  Until a node's value is set with a elektraNi_SetValue() (or
 * elektraNi_PrintValue()) function, it does NOT have a value.  Returns the value as a
 * string, or NULL if the function doesn't have a value or you pass an invalid
 * or root node.  If you care about the length of the value string, pass a non-
 * NULL len_out and its length is returned there.
 */
elektraNi_PUBLIC const char * elektraNi_GetValue (elektraNi_node restrict n, int * restrict len_out)
{
	const char * value = NULL;
	int len = 0;

	if (n && n != n->root)
	{
		value = n->value.str;
		len = n->value.len;
	}

	if (len_out)
	{
		*len_out = len;
	}
	return value;
}

/* Returns a node's value interpreted as a long, or 0 if the node doesn't have
 * a value (see elektraNi_GetValue()).  Note that it uses strtol()'s base detection so
 * strings starting with 0 are considered octal, and 0x are considered hex.
 */
elektraNi_PUBLIC long elektraNi_GetValueInt (elektraNi_node restrict n)
{
	long i = 0L;
	const char * v;

	if ((v = elektraNi_GetValue (n, NULL)) != NULL)
	{
		i = strtol (v, NULL, 0);
	}

	return i;
}

/* Returns the node's value interpreted as a double, or 0.0 if the node doesn't
 * have a value (see elektraNi_GetValue()).
 */
elektraNi_PUBLIC double elektraNi_GetValueFloat (elektraNi_node restrict n)
{
	double d = 0.0;
	const char * v;

	if ((v = elektraNi_GetValue (n, NULL)) != NULL)
	{
		d = strtod (v, NULL);
	}

	return d;
}

/* Returns the node's value interpreted as a boolean integer (0/1), or 0 if the
 * node doesn't have a value (see elektraNi_GetValue()).  The following strings are
 * considered "true" and have a nonzero return value from this function: any
 * string starting with T or Y, the string "on", (case is ignored in all those
 * cases), or any nonzero integer.  Everything else is considered "false" and
 * will result in a 0 return value.
 */
elektraNi_PUBLIC int elektraNi_GetValueBool (elektraNi_node restrict n)
{
	int b = 0;
	const char * v;
	int len;

	if ((v = elektraNi_GetValue (n, &len)) != NULL)
	{
		if (*v == 'T' || *v == 't' || *v == 'Y' || *v == 'y' || strtol (v, NULL, 0) ||
		    (len == 2 && (*v == 'o' || *v == 'O') && (*(v + 1) == 'n' || *(v + 1) == 'N')))
		{
			b = 1;
		}
	}

	return b;
}

/* Calls vsscanf() on the node's value string directly.  format is the scanf()-
 * formatted argument string, and any arguments for scanf() are passed after
 * format.  Returns what scanf() returns: the number of translated items.
 */
elektraNi_PUBLIC int elektraNi_ValueScan (elektraNi_node restrict n, const char * restrict format, ...)
{
	int rc;
	va_list args;

	va_start (args, format);
	rc = elektraNi_ValueVScan (n, format, args);
	va_end (args);

	return rc;
}

/* Same as above, except you pass a va_list instead of the args directly.
 */
elektraNi_PUBLIC int elektraNi_ValueVScan (elektraNi_node restrict n, const char * restrict format, va_list args)
{
	int items = 0;
	const char * v;

	if ((v = elektraNi_GetValue (n, NULL)) != NULL)
	{
		items = vsscanf (v, format, args);
	}

	return items;
}

/* Sets or removes a node's value.  If value is non-NULL, the value is set to
 * that string (which can be any length--specify its length in value_len, or
 * pass a negative value in value_len and it'll be calculated automatically
 * using strlen()).  If value is NULL (value_len is ignored in this case), its
 * value is removed--subsequent calls to elektraNi_GetValue() will return NULL (until
 * you set its value to something non-NULL, anyway).  Returns the length of
 * value, either as passed or calculated, or -1 if it fails, or 0 if you're
 * removing a value (so a negative return value always indicates error).  If
 * the value setting or removing succeeds, the node's modified state is set to
 * 1.  If setting the value fails, the contents of the value will NOT have
 * changed (and its modified state won't have changed either).  Note that for
 * setting the value, the value string you pass need not persist after the
 * call--its contents are copied.
 */
elektraNi_PUBLIC int elektraNi_SetValue (elektraNi_node restrict n, const char * restrict value, int value_len)
{
	int len = -1;

	// if it's a valid node and this isn't the root node (root can't have a
	// value)
	if (n && n != n->root)
	{
		// if they're specifying a new value
		if (value)
		{
			int old_len;

			old_len = n->value.len;
			n->value.len = 0; // don't concatenate, but copy

			// Ds_StrCat() handles name_len being negative, so we don't need to fix
			// it here
			if ((len = Ds_StrCat (&n->value, value, value_len)) < 0)
			{
				// Ds_StrCat() returns a negative number if it fails, so we
				// don't need to do anything to len here

				n->value.len = old_len;
			}
			else
				n->modified = 1;
		}
		else // they're deleting the value
		{
			// so we free it and re-init it to NULL
			Ds_FreeStr (&n->value);
			// the string will have a null value now, and can still be copied/
			// printed to just fine

			n->modified = 1;
			len = 0;
		}
	}

	return len;
}

/* Sets a node's value to the value of a long.  Semantics are similar to those
 * of elektraNi_SetValue(), except you can't remove a node's value with this function.
 */
elektraNi_PUBLIC int elektraNi_SetValueInt (elektraNi_node restrict n, long value)
{
	return elektraNi_ValuePrint (n, "%ld", value);
}

/* Sets a node's value to the value of a double.  Semantics are similar to
 * those of elektraNi_SetValue(), except you can't remove a node's value with this
 * function.
 */
elektraNi_PUBLIC int elektraNi_SetValueFloat (elektraNi_node restrict n, double value)
{
	return elektraNi_ValuePrint (n, "%.17g", value);
}

/* Sets a node's value to "true" or "false" based on a boolean integer.
 * Semantics are similar to those of elektraNi_SetValue(), except you can't remove a
 * node's value with this function.
 */
elektraNi_PUBLIC int elektraNi_SetValueBool (elektraNi_node restrict n, int value)
{
	return elektraNi_SetValue (n, (value ? "true" : "false"), (value ? 4 : 5));
}

/* Uses printf() formatting to set the node's value.  You can't remove a node's
 * value with this function.  format is the printf()-formatted string, and any
 * arguments are passed after it.  Returns the resulting string length, or -1
 * if an error occurs (which may be as mundane as you passing an invalid or
 * root node).  If this function fails, unfortunately the contents of the value
 * MAY have changed due to printf() having complicated internal workings.  If
 * it fails, though, the modified state won't have changed, so you won't write
 * garbage if you're only writing modified values.  I don't know what else to
 * do, really.
 */
elektraNi_PUBLIC int elektraNi_ValuePrint (elektraNi_node restrict n, const char * restrict format, ...)
{
	int rc;
	va_list args;

	va_start (args, format);
	rc = elektraNi_ValueVPrint (n, format, args);
	va_end (args);

	return rc;
}

/* Same as above, except it expects a va_list instead of the args passed after
 * the format string.
 */
elektraNi_PUBLIC int elektraNi_ValueVPrint (elektraNi_node restrict n, const char * restrict format, va_list args)
{
	int len = -1;

	// if it's a valid node and this isn't the root node (root can't have a
	// value)
	if (n && n != n->root)
	{
		int old_len;
		old_len = n->value.len;
		n->value.len = 0; // don't concatenate, but copy

		// Ds_StrCatVPrint() handles format being NULL, so we don't need to fix it
		// here
		if ((len = Ds_StrCatVPrint (&n->value, format, args)) < 0)
		{
			// Ds_StrCatVPrint() returns a negative number if it fails, so we
			// don't need to do anything to len here

			n->value.len = old_len;
		}
		else
			n->modified = 1;
	}

	return len;
}

/* Writes the contents of the tree starting at the node out to a file, in a
 * format that is parsable by elektraNi_ReadFile() or -Stream(), and roughly
 * compatible with .ini files.  Note that you can pass any node of a tree to
 * this function--only its children and downward are output.  If you pass
 * modified_only as nonzero, values are only output if the node's modified
 * state is true (either way, you must manually set the nodes' modified states
 * to 0 after calling this function, if you want to keep track of it that way).
 * Returns 0 on error, or nonzero on success.  The file is opened with
 * fopen(filename, "w"), so its contents will be erased.
 */
elektraNi_PUBLIC int elektraNi_WriteFile (elektraNi_node restrict n, const char * restrict filename, int modified_only)
{
	int rc = 0;
	FILE * fp = NULL;

	if (filename)
	{
		if ((fp = fopen (filename, "w")) != NULL)
		{
			rc = elektraNi_WriteStream (n, fp, modified_only);
			fclose (fp);
		}
	}

	return rc;
}

/* Same as above, except instead of a filename, you can pass an already-open
 * file or a stream (like stdout).  The file must be writable, but need not be
 * seekable.
 */
elektraNi_PUBLIC int elektraNi_WriteStream (elektraNi_node restrict n, FILE * restrict stream, int modified_only)
{
	int success = 0;
	do
	{
		if (!n || !stream) break;

		if (fprintf (stream,
			     ";Ni1\n"
			     "; Generated by the ni plugin using Elektra (see libelektra.org).\n\n") < 0)
		{
			break;
		}

		if (!RecursiveWrite (n, stream, modified_only, 0)) break;

		success = 1;
	} while (0);

	return success;
}

/* Reads the contents of the file into the node and its children.  The node is
 * treated as the root of the resulting tree, but need not actually be the root
 * of the tree.  Any existing values in the tree are overwritten by the ones
 * from the file, or are created if they don't exist.  Returns 0 if it fails
 * (the contents of the node's children are undefined in this case), or nonzero
 * if it succeeds (invalid lines in a file won't make the function fail--
 * they'll just be skipped).  If fold_case is nonzero, all node names will be
 * converted to lowercase as they're read in.  Since "Name" and "name" are
 * different names, this makes the files less strict with case.
 */
elektraNi_PUBLIC int elektraNi_ReadFile (elektraNi_node restrict n, const char * restrict filename, int fold_case)
{
	int rc = 0;
	FILE * fp = NULL;

	if (filename)
	{
		if ((fp = fopen (filename, "r")) != NULL)
		{
			rc = elektraNi_ReadStream (n, fp, fold_case);
			fclose (fp);
		}
	}

	return rc;
}

/* Same as above, except instead of a filename, you pass an already-open file
 * or stream (like stdin).  The file must be readable, but need not be
 * seekable.
 */
elektraNi_PUBLIC int elektraNi_ReadStream (elektraNi_node restrict n, FILE * restrict stream, int fold_case)
{
	file_buf fb = FILE_BUF_INIT;		 // the file buffer we're reading
	char key[elektraNi_KEY_SIZE] = { '\0' }; // section/key name for GetNextIdentifier
	int key_len;				 // length of string in 'key' buffer
	int key_level;				 // how many ['s were in front of key, if section
	int cur_level = 0;			 // where we currently are in the tree
	Ds_str value = Ds_STR_INIT;		 // value holder string
	int result;				 // the result of internal operations
	elektraNi_node child;			 // a child
	int i;

	int success = 0;
	do
	{
		if (!n || !stream) break;

		if (!InitFileBuf (&fb, stream)) break;
		if (!Ds_InitStr (&value, INITIAL_VALUE_BUFFER)) break;

		// do this until eof
		while ((result = GetNextIdentifier (&fb, key, &key_len, &key_level)) != 0)
		{
			if (result < 0) break;

			if (fold_case)
			{
				// TODO: this breaks valid UTF-8 and is locale-dependent...

				for (i = 0; i < key_len; ++i)
					key[i] = tolower (key[i]);
			}

			if (result == 1) // if section name
			{
				// if key_level is more deeply nested than we are currently, by more
				// than 1 level
				while (key_level - cur_level > 1)
				{
					// get or add nameless children, as necessary
					if (!(n = elektraNi_GetChild (n, "", 0, 1, NULL))) break;
					++cur_level;
				}
				// if key_level is less deeply nested than we are currently, by more
				// than 1
				while (key_level - cur_level < 1)
				{
					if (!(n = elektraNi_GetParent (n))) break;
					--cur_level;
				}

				if (key_level - cur_level != 1) result = -1;
			}
			if (result < 0) break;

			// get/add the child
			if (!(child = elektraNi_GetChild (n, key, key_len, 1, NULL)))
			{
				result = -1;
				break;
			}

			if (result == 1) // if it was a section
			{
				n = child;   // we've got to start from there next time
				++cur_level; // and say we're a level deeper
			}
			else // it was a key=value pair
			{
				// then get the upcoming value into it
				if (!GetValue (&fb, &value))
				{
					result = -1;
					break;
				}

				// set the new child's value
				if (elektraNi_SetValue (child, value.str, value.len) < 0)
				{
					result = -1;
					break;
				}
			}
		}
		if (result < 0) // if we dipped out early
			break;

		success = 1;
	} while (0);

	FreeFileBuf (&fb);
	Ds_FreeStr (&value);

	return success;
}

/* Initializes the node pointed to by n, makes sure there's space in the
 * parent's table, and adds the node as its child.  Returns NULL if it fails,
 * or the added node if it succeeds.
 */
static elektraNi_node AddNode (elektraNi_node restrict n, elektraNi_node restrict parent, const char * restrict name, int name_len,
			       Ds_hash_t hash)
{
	int success = 0;
	elektraNi_node child = NULL;
	Ds_hash_entry * e = NULL;

	assert (n != NULL);
	assert (parent != NULL);
	assert (name_len < elektraNi_KEY_SIZE);

	do
	{
		// init the node
		if (!InitNode (n, parent)) break;

		// give it a name
		memcpy (n->name, name, name_len * sizeof (char));
		n->name[n->name_len = name_len] = '\0';

		// check for space (requires 25% free space in the table) and grow the
		// table by a factor of 2 if it's too small
		if (parent->children.num >= ((parent->children.cap >> 2) + (parent->children.cap >> 1)) &&
		    !Ds_ResizeHashTable (&parent->children, parent->children.cap << 1))
			break;

		// insert it
		if (!(e = Ds_InsertHashItem (&parent->children, n, sizeof (struct elektraNi_node_struct), hash))) break;
		child = GetItem (e); // get the inserted item

		success = 1;
	} while (0);

	if (!success)
	{
		if (e) Ds_RemoveHashEntry (&parent->children, e);
		FreeNode (n);
	}

	return child;
}

/* Initializes the contents of the node.
 */
static int InitNode (elektraNi_node restrict n, elektraNi_node restrict parent)
{
	assert (n);

	*n = (struct elektraNi_node_struct) NODE_STRUCT_INIT;

	n->root = (parent ? parent->root : n);
	n->parent = parent;

	// make space for children
	return Ds_InitHashTable (&n->children, INITIAL_BUCKETS);
}

/* Frees the contents of the node.
 */
static void FreeNode (elektraNi_node restrict n)
{
	assert (n != NULL);

	Ds_FreeStr (&n->value);		 // free its value
	Ds_FreeHashTable (&n->children); // free its array of children
}

/* Calls the above on the node and all its children.
 */
static void RecursiveFree (elektraNi_node restrict n)
{
	Ds_hash_entry * e = NULL;

	assert (n != NULL);

	while ((e = Ds_NextHashEntry (&n->children, e)) != NULL)
		RecursiveFree (GetItem (e)); // free it

	FreeNode (n);
}

/* Sets a node's modified state recursively.
 */
static void RecursiveSetModified (elektraNi_node restrict n, int modified)
{
	Ds_hash_entry * e = NULL;

	assert (n != NULL);

	while ((e = Ds_NextHashEntry (&n->children, e)) != NULL)
		RecursiveSetModified (GetItem (e), modified); // set that shit

	n->modified = modified;
}

/* Writes all the node's children that have values (and maybe are modified)
 * out, then writes section names and calls itself recursively for any children
 * that have children.  Returns 0 if it fails, nonzero on success.
 */
static int RecursiveWrite (elektraNi_node restrict n, FILE * restrict stream, int modified_only, int level)
{
	elektraNi_node child;
	const char * name;
	int name_len;
	const char * value;
	int value_len;
	int success = 0;

	assert (n != NULL);

	do
	{
		// loop through all children
		child = NULL;
		while ((child = elektraNi_GetNextChild (n, child)) != NULL)
		{
			// get its name
			name = elektraNi_GetName (child, &name_len);
			assert (name != NULL);

			// get its value and only do anything if it's modified or we're writing
			// all children
			if ((value = elektraNi_GetValue (child, &value_len)) != NULL && (!modified_only || elektraNi_GetModified (child)))
			{
				// put the actual key/value pair
				if (!PutEntry (stream, name, name_len, value, value_len, level + 1)) break;
			}
		}
		if (child) // if we broke out early
			break;

		// go through all children again
		child = NULL;
		while ((child = elektraNi_GetNextChild (n, child)) != NULL)
		{
			// if this child has children
			if (elektraNi_GetNumChildren (child) > 0)
			{
				// get the child's name
				name = elektraNi_GetName (child, &name_len);
				assert (name != NULL);

				// put it as a section name
				if (!PutSection (stream, name, name_len, level + 1)) break;

				// recurse
				if (!RecursiveWrite (child, stream, modified_only, level + 1)) break;
			}
		}
		if (child) break;

		success = 1;
	} while (0);

	return success;
}

/* Compares a key with a elektraNi_node_struct's name for Ds_SearchHashTable().
 */
static int Compare (const void * restrict key, size_t key_size, const void * restrict item, size_t item_size ELEKTRA_UNUSED)
{
	const struct elektraNi_node_struct * n;
	n = (const struct elektraNi_node_struct *) item;

	assert (item_size == sizeof (struct elektraNi_node_struct));
	assert (key != NULL);
	assert (n->name != NULL);
	assert (key_size < elektraNi_KEY_SIZE);
	assert (n->name_len < elektraNi_KEY_SIZE);
	const size_t n_name_len = n->name_len;

	return (key_size != n_name_len || memcmp (key, n->name, key_size));
}
