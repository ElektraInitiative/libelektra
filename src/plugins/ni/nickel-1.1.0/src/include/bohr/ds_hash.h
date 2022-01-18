/******************************************************************************
 * Darmstadtium - a library of data structures
 * Ds_hash - a hash table library
 * One of the Bohr Game Libraries (see chaoslizard.org/devel/bohr)
 * Copyright (C) 2008 Charles Lindsay.  Some rights reserved; see COPYING.
 * $Id: ds_hash.h 317 2008-01-05 21:45:34Z chaz $
 ******************************************************************************/


#ifndef __bohr_ds_hash_h__
#define __bohr_ds_hash_h__

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif

#include <inttypes.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>


// Controls inlining of this library's functions.
#ifndef Ds_HASH_INLINE
#ifdef Ds_INLINE
#define Ds_HASH_INLINE Ds_INLINE
#else
#define Ds_HASH_INLINE inline static
#endif
#endif

// Nix non-critical C99 keywords in compilers that don't support them.
#if ((!defined(__STDC_VERSION__) || __STDC_VERSION__ < 199901L) && !defined(restrict))
#define restrict
#define _Ds_HASH_DEFINED_RESTRICT
#endif


// A type to hold a hash value.  We also define the standard constant-
// declaration macro, max value, and printf/scanf specifiers.
#ifndef Ds_HASH_T_DEFINED
#define Ds_HASH_T_DEFINED
typedef uint32_t Ds_hash_t;
#define Ds_HASH_C UINT32_C
#define Ds_HASH_MAX UINT32_MAX
#define PRIdDs_HASH PRId32
#define PRIiDs_HASH PRIi32
#define PRIoDs_HASH PRIo32
#define PRIuDs_HASH PRIu32
#define PRIxDs_HASH PRIx32
#define PRIXDs_HASH PRIX32
#define SCNdDs_HASH SCNd32
#define SCNiDs_HASH SCNi32
#define SCNoDs_HASH SCNo32
#define SCNuDs_HASH SCNu32
#define SCNxDs_HASH SCNx32
#endif


// A chainable entry in a Ds_hash_table.
typedef struct Ds_hash_entry
{
	struct Ds_hash_entry * next; // linked list next pointer

	Ds_hash_t hash; // the item's hash value
	size_t bucket;	// which bucket it's in
	size_t size;	// how big the item is
	uint8_t item[]; // the item itself (struct hack, valid C99)

} Ds_hash_entry;


// Define a vector that works with our Ds_hash_entries.
#define Ds_VECTOR_BEHAVIOR 2
#define Ds_VECTOR_TYPE Ds_hash_entry *
#define Ds_VECTOR_SUFFIX _Dsh
#include <bohr/ds_vector.h>


// A hash table (an alias for a specific kind of Ds_vector).
typedef Ds_vector_Dsh Ds_hash_table;
#define Ds_HASH_TABLE_INIT Ds_VECTOR_INIT // initializer


// Function to compare two hash items; return similar to strcmp--declare as:
//   int MyCompare(const void * restrict key, size_t key_size,
//                 const void * restrict item, size_t item_size);
typedef int (*Ds_hash_compare_fn) (const void * key, size_t key_size, const void * item, size_t item_size);


/* Reserves initial space in the Ds_hash_table.  size must be a power of 2.
 */
Ds_HASH_INLINE int Ds_InitHashTable (Ds_hash_table * restrict table, size_t size)
{
	if (!Ds_InitVector_Dsh ((Ds_vector_Dsh *) table, size)) return 0;

	memset (table->buf, 0, size * sizeof (Ds_hash_entry *));
	return 1;
}

/* Frees memory associated with the Ds_hash_table.
 */
Ds_HASH_INLINE void Ds_FreeHashTable (Ds_hash_table * restrict table)
{
	for (size_t i = 0; i < table->cap; ++i)
	{
		Ds_hash_entry * head;

		head = table->buf[i];
		while (head)
		{
			Ds_hash_entry * t;

			t = head;
			head = head->next;
			elektraFree (t);
		}
	}

	Ds_FreeVector_Dsh ((Ds_vector_Dsh *) table);
}

/* Iterates over all the entries in the Ds_hash_table.  Specify NULL for prev to
 * return the first Ds_hash_entry.  Subsequently pass the previous return value.
 * Returns NULL when there are no remaining entries.
 */
Ds_HASH_INLINE Ds_hash_entry * Ds_NextHashEntry (const Ds_hash_table * restrict table, const Ds_hash_entry * restrict prev)
{
	Ds_hash_entry * n = NULL;
	size_t bucket = 0;

	if (prev)
	{
		n = prev->next;
		bucket = prev->bucket + 1;
	}

	while (!n && bucket < table->cap)
		n = table->buf[bucket++];

	return n;
}

/* Inserts an item into the Ds_hash_table.  The item has the given hash value
 * and size.  Returns the new Ds_hash_entry or NULL if it fails.
 */
Ds_HASH_INLINE Ds_hash_entry * Ds_InsertHashItem (Ds_hash_table * restrict table, const void * restrict item, size_t size, Ds_hash_t hash)
{
	Ds_hash_entry * n;

	if ((n = (Ds_hash_entry *) malloc (sizeof (Ds_hash_entry) + size)) != NULL)
	{
		n->hash = hash;
		n->bucket = hash & (table->cap - 1);
		n->size = size;
		memcpy (n->item, item, size);

		n->next = table->buf[n->bucket];
		table->buf[n->bucket] = n;
		table->num++;
	}

	return n;
}

/* Removes a Ds_hash_entry from the Ds_hash_table.  The entry must belong to the
 * table.  Returns 0/nonzero on failure/success.
 */
Ds_HASH_INLINE int Ds_RemoveHashEntry (Ds_hash_table * restrict table, Ds_hash_entry * restrict entry)
{
	int found = 0;
	Ds_hash_entry * t;

	if (entry == (t = table->buf[entry->bucket]))
	{
		table->buf[entry->bucket] = entry->next;
		found = 1;
	}
	else if (t)
	{
		while (t->next)
		{
			if (entry == t->next)
			{
				t->next = entry->next;
				found = 1;
				break;
			}
		}
	}

	if (found)
	{
		elektraFree (entry);
		table->num--;
	}

	return found;
}

/* Searches the Ds_hash_table for a given key item that has the given hash
 * value.  Returns the first entry matching the hash value, or if you specify a
 * compare function, returns the first entry the compare function returns 0 on.
 * Returns NULL if it's not found.  key and size are only used to pass to
 * compare().
 */
Ds_HASH_INLINE Ds_hash_entry * Ds_SearchHashTable (const Ds_hash_table * restrict table, const void * restrict key, size_t size,
						   Ds_hash_t hash, Ds_hash_compare_fn compare)
{
	Ds_hash_entry * t;

	t = table->buf[hash & (table->cap - 1)];
	while (t && hash != t->hash && (!compare || compare (key, size, &t->item, t->size)))
	{
		t = t->next;
	}

	return t;
}

/* Resizes the Ds_hash_table, moving entries around.  size must be a power of 2.
 */
Ds_HASH_INLINE int Ds_ResizeHashTable (Ds_hash_table * restrict table, size_t size)
{
	if (size > table->cap)
	{
		size_t old_size;

		old_size = table->cap;
		if (!Ds_ResizeVector_Dsh ((Ds_vector_Dsh *) table, size)) return 0;
		memset (table->buf + old_size, 0, (size - old_size) * sizeof (Ds_hash_entry *));

		for (size_t i = 0; i < old_size; ++i)
		{
			size_t bucket;
			Ds_hash_entry *e, *t;

			e = table->buf[i];
			while (e && (bucket = e->hash & (size - 1)) != i)
			{
				t = e->next;
				e->next = table->buf[e->bucket = bucket];
				table->buf[bucket] = e;
				table->buf[i] = e = t;
			}
			while (e && (t = e->next) != NULL)
			{
				if ((bucket = t->hash & (size - 1)) != i)
				{
					e->next = t->next;
					t->next = table->buf[t->bucket = bucket];
					table->buf[bucket] = t;
				}
				else
					e = t;
			}
		}
	}
	else if (size < table->cap)
	{
		size_t old_num;

		for (size_t i = size; i < table->cap; ++i)
		{
			Ds_hash_entry * t;

			if ((t = table->buf[i]) != NULL)
			{
				size_t bucket;

				bucket = i & (size - 1);
				while (t->next)
				{
					t->bucket = bucket;
					t = t->next;
				}
				t->next = table->buf[t->bucket = bucket];
				table->buf[bucket] = table->buf[i];
			}
		}

		old_num = table->num;
		if (!Ds_ResizeVector_Dsh ((Ds_vector_Dsh *) table, size)) return 0;
		table->num = old_num;
	}

	return 1;
}


#ifdef _Ds_HASH_DEFINED_RESTRICT
#undef _Ds_HASH_DEFINED_RESTRICT
#undef restrict
#endif

#endif
