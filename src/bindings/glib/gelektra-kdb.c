#include "gelektra-kdb.h"
#include <elektra/kdbextension.h>
#include <string.h>

G_DEFINE_TYPE (GElektraKdb, gelektra_kdb, G_TYPE_OBJECT)
static KDB * gelektra_kdb_swap (GElektraKdb * kdb, KDB * newhandle);

static void gelektra_kdb_init (GElektraKdb * self)
{
	/* initialize the object */
	self->handle = NULL;
}

static void gelektra_kdb_finalize (GObject * object)
{
	GElektraKdb * self = GELEKTRA_KDB (object);

	if (self->handle) kdbClose (self->handle, NULL);
	self->handle = NULL;

	/* Always chain up to the parent class; as with dispose(), finalize()
	 * is guaranteed to exist on the parent's class virtual function table
	 */
	G_OBJECT_CLASS (gelektra_kdb_parent_class)->finalize (object);
}

static void gelektra_kdb_class_init (GElektraKdbClass * klass)
{
	GObjectClass * gobject_class = G_OBJECT_CLASS (klass);
	gobject_class->finalize = gelektra_kdb_finalize;
}

/*
 * Methods
 */

/* constructor */
/**
 * gelektra_kdb_open: (constructor)
 * @contract contract for kdbOpen()
 * @error key which holds errors and warnings which were issued
 *
 * Returns: (transfer full): A new #GElektraKdb
 * see kdbOpen
 */
GElektraKdb * gelektra_kdb_open (GElektraKeySet * contract, GElektraKey * error)
{
	return gelektra_kdb_make (kdbOpen (contract == NULL ? NULL : contract->keyset, error->key));
}

/**
 * gelektra_kdb_make: (skip)
 * @handle: The underlying handle object
 *
 * Returns: (transfer full): A new #GElektraKdb holding the ownership of @handle
 */
GElektraKdb * gelektra_kdb_make (KDB * handle)
{
	if (handle == NULL) return NULL;
	GElektraKdb * ret = g_object_new (GELEKTRA_TYPE_KDB, NULL);
	gelektra_kdb_swap (ret, handle);
	return ret;
}

/* destructor */
gint gelektra_kdb_close (GElektraKdb * kdb, GElektraKey * error)
{
	int ret = kdbClose (kdb->handle, (error) ? error->key : NULL);
	kdb->handle = NULL;
	return ret;
}

/* basic methods */
/**
 * gelektra_kdb_gi_open:
 * @kdb: A #GElektraKdb
 * @error key which holds errors and warnings which were issued
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_kdb_open instead
 */
void gelektra_kdb_gi_open (GElektraKdb * kdb, GElektraKeySet * contract, GElektraKey * error)
{
	kdb->handle = kdbOpen (contract == NULL ? NULL : contract->keyset, error->key);
}

gint gelektra_kdb_get (GElektraKdb * kdb, GElektraKeySet * returned, GElektraKey * parent)
{
	return kdbGet (kdb->handle, returned->keyset, parent->key);
}

gint gelektra_kdb_set (GElektraKdb * kdb, GElektraKeySet * returned, GElektraKey * parent)
{
	return kdbSet (kdb->handle, returned->keyset, parent->key);
}

/**
 * gelektra_kdb_swap: (skip)
 * @kdb: A #GElektraKdb
 * @newhandle: The new underlying handle
 *
 * Returns: The old underlying handle
 */
static KDB * gelektra_kdb_swap (GElektraKdb * kdb, KDB * newhandle)
{
	KDB * oldhandle = kdb->handle;
	kdb->handle = newhandle;
	return oldhandle;
}
