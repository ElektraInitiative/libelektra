#ifndef G_ELEKTRA_KDB_H
#define G_ELEKTRA_KDB_H

#include <elektra/kdb.h>
#include <glib-object.h>

// clang-format off

G_BEGIN_DECLS

/*
 * Type macros.
 */
#define GELEKTRA_TYPE_KDB            (gelektra_kdb_get_type())
#define GELEKTRA_KDB(obj)            (G_TYPE_CHECK_INSTANCE_CAST((obj), GELEKTRA_TYPE_KDB, GElektraKdb))
#define GELEKTRA_IS_KDB(obj)         (G_TYPE_CHECK_INSTANCE_TYPE((obj), GELEKTRA_TYPE_KDB))
#define GELEKTRA_KDB_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST((klass), GELEKTRA_TYPE_KDB, GElektraKdbClass))
#define GELEKTRA_IS_KDB_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), GELEKTRA_TYPE_KDB))
#define GELEKTRA_KDB_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS((obj), GELEKTRA_TYPE_KDB, GElektraKdbClass))

typedef struct _GElektraKdb      GElektraKdb;
typedef struct _GElektraKdbClass GElektraKdbClass;

typedef enum {
	GELEKTRA_KDB_O_NONE        = KDB_O_NONE,
	GELEKTRA_KDB_O_DEL         = KDB_O_DEL,
	GELEKTRA_KDB_O_POP         = KDB_O_POP,
	GELEKTRA_KDB_O_NODIR       = KDB_O_NODIR,
	GELEKTRA_KDB_O_DIRONLY     = KDB_O_DIRONLY,
	GELEKTRA_KDB_O_NOREMOVE    = KDB_O_NOREMOVE,
	GELEKTRA_KDB_O_REMOVEONLY  = KDB_O_REMOVEONLY,
	GELEKTRA_KDB_O_INACTIVE    = KDB_O_INACTIVE,
	GELEKTRA_KDB_O_SYNC        = KDB_O_SYNC,
	GELEKTRA_KDB_O_SORT        = KDB_O_SORT,
	GELEKTRA_KDB_O_NORECURSIVE = KDB_O_NORECURSIVE,
	GELEKTRA_KDB_O_NOCASE      = KDB_O_NOCASE,
	GELEKTRA_KDB_O_WITHOWNER   = KDB_O_WITHOWNER,
	GELEKTRA_KDB_O_NOALL       = KDB_O_NOALL
} GElektraKdbOptions;

#ifndef G_ELEKTRA_CONSTANTS_H
#define GELEKTRA_DB_SYSTEM KDB_DB_SYSTEM
#define GELEKTRA_DB_USER   KDB_DB_USER
#define GELEKTRA_DB_HOME   KDB_DB_HOME

#define GELEKTRA_DEBUG     DEBUG

#define GELEKTRA_VERSION       KDB_VERSION
#define GELEKTRA_VERSION_MAJOR KDB_VERSION_MAJOR
#define GELEKTRA_VERSION_MINOR KDB_VERSION_MINOR
#define GELEKTRA_VERSION_MICRO KDB_VERSION_MICRO
#endif

struct _GElektraKdb
{
	GObject parent_instance;

	/* instance members */
	KDB *handle;
};

struct _GElektraKdbClass
{
	GObjectClass parent_class;

	/* class members */
};

/* used by GELEKTRA_TYPE_KDB */
GType gelektra_kdb_get_type(void);

#include "gelektra-key.h"
#include "gelektra-keyset.h"

/*
 * Method definitions.
 */

/* constructor */
GElektraKdb *gelektra_kdb_open(GElektraKey *error);
GElektraKdb *gelektra_kdb_make(KDB *handle);

/* destructor */
gint gelektra_kdb_close(GElektraKdb *kdb, GElektraKey *error);

/* basic methods */
void gelektra_kdb_gi_open(GElektraKdb *kdb, GElektraKey *error);
gint gelektra_kdb_get(GElektraKdb *kdb, GElektraKeySet *returned,
	GElektraKey *parent);
gint gelektra_kdb_set(GElektraKdb *kdb, GElektraKeySet *returned,
	GElektraKey *parent);


G_END_DECLS

#endif
