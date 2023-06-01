#include <elektra/glib/gelektra-keyset.h>
#include <string.h>

G_DEFINE_TYPE (GElektraKeySet, gelektra_keyset, G_TYPE_OBJECT)
static KeySet * gelektra_keyset_swap (GElektraKeySet * ks, KeySet * newks);

static void gelektra_keyset_init (GElektraKeySet * self)
{
	/* initialize the object */
	self->keyset = ksNew (0, KS_END);
}

static void gelektra_keyset_finalize (GObject * object)
{
	GElektraKeySet * self = GELEKTRA_KEYSET (object);

	ksDel (self->keyset);

	/* Always chain up to the parent class; as with dispose(), finalize()
	 * is guaranteed to exist on the parent's class virtual function table
	 */
	G_OBJECT_CLASS (gelektra_keyset_parent_class)->finalize (object);
}

static void gelektra_keyset_class_init (GElektraKeySetClass * klass)
{
	GObjectClass * gobject_class = G_OBJECT_CLASS (klass);
	gobject_class->finalize = gelektra_keyset_finalize;
}

/*
 * Methods
 */

/* constructor */
/**
 * gelektra_keyset_new: (skip)
 * @size initial allocation size of the keyset
 * @... optional parameters closed by GELEKTRA_KEYSET_END, see ksNew
 *
 * Returns: A new #GElektraKeySet
 * see ksNew
 */
GElektraKeySet * gelektra_keyset_new (gsize alloc, ...)
{
	va_list va;

	GElektraKeySet * ks = g_object_new (GELEKTRA_TYPE_KEYSET, NULL);
	if (alloc > 0)
	{
		va_start (va, alloc);
		GElektraKey * key = (GElektraKey *) va_arg (va, GElektraKey *);
		while (key)
		{
			gelektra_keyset_append (ks, key);
			key = (GElektraKey *) va_arg (va, GElektraKey *);
		}
		va_end (va);
	}
	return ks;
}

/**
 * gelektra_keyset_make: (skip)
 * @ks: The underlying keyset
 *
 * Returns: (transfer full): A new #GElektraKeySet holding the ownership of @ks
 */
GElektraKeySet * gelektra_keyset_make (KeySet * ks)
{
	if (ks == NULL) return NULL;
	GElektraKeySet * ret = gelektra_keyset_new (0);
	KeySet * old = gelektra_keyset_swap (ret, ks);
	ksDel (old);
	return ret;
}

/* basic methods */
/**
 * gelektra_keyset_dup:
 * @ks: A #GElektraKeySet
 *
 * Returns: (transfer full): A duplicated #GElektraKeySet
 * see ksDup
 */
GElektraKeySet * gelektra_keyset_dup (const GElektraKeySet * ks)
{
	return gelektra_keyset_make (ksDup (ks->keyset));
}

/**
 * gelektra_keyset_copy: (skip)
 * see ksCopy
 */
gint gelektra_keyset_copy (const GElektraKeySet * ks, GElektraKeySet * dest)
{
	return ksCopy (dest->keyset, ks->keyset);
}

gint gelektra_keyset_clear (GElektraKeySet * ks)
{
	return ksClear (ks->keyset);
}

/**
 * gelektra_keyset_swap: (skip)
 * @ls: A #GElektraKeySet
 * @newks: The new underlying keyset
 *
 * Returns: The old underlying keyset
 */
static KeySet * gelektra_keyset_swap (GElektraKeySet * ks, KeySet * newks)
{
	KeySet * oldks = ks->keyset;
	ks->keyset = newks;
	return oldks;
}

/**
 * gelektra_keyset_append: (skip)
 * @ks: A #GElektraKeySet
 * @key: A #GElektraKey to append. On success reference will decremented
 *
 * Returns: The new len of the keyset
 * see ksAppendKey
 */
gssize gelektra_keyset_append (GElektraKeySet * ks, GElektraKey * key)
{
	gssize ret = ksAppendKey (ks->keyset, key->key);
	if (ret > 0) g_object_unref (key);
	return ret;
}

/**
 * gelektra_keyset_gi_append:
 * @ks: A #GElektraKeySet
 * @key: A #GElektraKey to append
 *
 * Returns: The new len of the keyset
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_keyset_append instead
 */
gssize gelektra_keyset_gi_append (GElektraKeySet * ks, GElektraKey * key)
{
	return ksAppendKey (ks->keyset, key->key);
}

/**
 * gelektra_keyset_append_keyset: (skip)
 * @ks: A #GElektraKeySet
 * @append: A #GElektraKeySet to append. On success reference will decremented
 *
 * Returns: The new len of the keyset
 * see ksAppend
 */
gssize gelektra_keyset_append_keyset (GElektraKeySet * ks, GElektraKeySet * append)
{
	gssize ret = ksAppend (ks->keyset, append->keyset);
	if (ret > 0) g_object_unref (append);
	return ret;
}

/**
 * gelektra_keyset_gi_append_keyset:
 * @ks: A #GElektraKeySet
 * @append: A #GElektraKeySet to append
 *
 * Returns: The new len of the keyset
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_keyset_append instead
 */
gssize gelektra_keyset_gi_append_keyset (GElektraKeySet * ks, GElektraKeySet * append)
{
	return ksAppend (ks->keyset, append->keyset);
}

/**
 * gelektra_keyset_pop:
 * @ks: A #GElektraKeySet
 *
 * Returns: (transfer full): The last #GElektraKey of @ks
 * see ksPop
 */
GElektraKey * gelektra_keyset_pop (GElektraKeySet * ks)
{
	return gelektra_key_make (ksPop (ks->keyset));
}

/**
 * gelektra_keyset_cut:
 * @ks: A #GElektraKeySet
 *
 * Returns: (transfer full): Cut #GElektraKeySet
 * see ksCut
 */
GElektraKeySet * gelektra_keyset_cut (GElektraKeySet * ks, const GElektraKey * point)
{
	return gelektra_keyset_make (ksCut (ks->keyset, point->key));
}

gssize gelektra_keyset_len (const GElektraKeySet * ks)
{
	return ksGetSize (ks->keyset);
}

/* searching */
/**
 * gelektra_keyset_lookup:
 * @ks: A #GElektraKeySet
 * @key: The key to search for
 * @options: Search options
 *
 * Returns: (transfer full): Found #GElektraKey in @ks or NULL
 * see ksLookup
 */
GElektraKey * gelektra_keyset_lookup (GElektraKeySet * ks, GElektraKey * key, GElektraKdbOptions options)
{
	return gelektra_key_make (ksLookup (ks->keyset, key->key, options));
}

/**
 * gelektra_keyset_lookup_byname:
 * @ks: A #GElektraKeySet
 * @name: The name to search for
 * @options: Search options
 *
 * Returns: (transfer full): Found #GElektraKey in @ks or NULL
 * see ksLookupByName
 */
GElektraKey * gelektra_keyset_lookup_byname (GElektraKeySet * ks, const char * name, GElektraKdbOptions options)
{
	return gelektra_key_make (ksLookupByName (ks->keyset, name, options));
}

/* iterating */
/**
 * gelektra_keyset_at:
 * @ks: A #GElektraKeySet
 * @pos: The cursor position
 *
 * Returns: (transfer full): #GElektraKey in @ks at @pos
 * see ksAtCursor
 */
GElektraKey * gelektra_keyset_at (GElektraKeySet * ks, gssize pos)
{
	if (pos < 0) pos += gelektra_keyset_len (ks);
	return gelektra_key_make (ksAtCursor (ks->keyset, pos));
}
