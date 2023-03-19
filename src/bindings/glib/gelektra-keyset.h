#ifndef G_ELEKTRA_KEYSET_H
#define G_ELEKTRA_KEYSET_H

#include <elektra/old_kdb.h>
#include <glib-object.h>

G_BEGIN_DECLS

/*
 * Type macros.
 */
#define GELEKTRA_TYPE_KEYSET (gelektra_keyset_get_type ())
#define GELEKTRA_KEYSET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GELEKTRA_TYPE_KEYSET, GElektraKeySet))
#define GELEKTRA_IS_KEYSET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GELEKTRA_TYPE_KEYSET))
#define GELEKTRA_KEYSET_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GELEKTRA_TYPE_KEYSET, GElektraKeySetClass))
#define GELEKTRA_IS_KEYSET_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GELEKTRA_TYPE_KEYSET))
#define GELEKTRA_KEYSET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GELEKTRA_TYPE_KEYSET, GElektraKeySetClass))

typedef struct _GElektraKeySet GElektraKeySet;
typedef struct _GElektraKeySetClass GElektraKeySetClass;

#define GELEKTRA_KEYSET_END KS_END

struct _GElektraKeySet
{
	GObject parent_instance;

	/* instance members */
	KeySet * keyset;
};

struct _GElektraKeySetClass
{
	GObjectClass parent_class;

	/* class members */
};

/* used by GELEKTRA_TYPE_KEYSET */
GType gelektra_keyset_get_type (void);

#include "gelektra-kdb.h"
#include "gelektra-key.h"

/*
 * Method definitions.
 */

/* constructor */
GElektraKeySet * gelektra_keyset_new (gsize alloc, ...);
GElektraKeySet * gelektra_keyset_make (KeySet * ks);

/* basic methods */
GElektraKeySet * gelektra_keyset_dup (const GElektraKeySet * ks);
gint gelektra_keyset_copy (const GElektraKeySet * ks, GElektraKeySet * dest);
gint gelektra_keyset_clear (GElektraKeySet * ks);

gssize gelektra_keyset_append (GElektraKeySet * ks, GElektraKey * key);
gssize gelektra_keyset_gi_append (GElektraKeySet * ks, GElektraKey * key);
gssize gelektra_keyset_append_keyset (GElektraKeySet * ks, GElektraKeySet * append);
gssize gelektra_keyset_gi_append_keyset (GElektraKeySet * ks, GElektraKeySet * append);
GElektraKey * gelektra_keyset_pop (GElektraKeySet * ks);
GElektraKeySet * gelektra_keyset_cut (GElektraKeySet * ks, const GElektraKey * point);
gssize gelektra_keyset_len (const GElektraKeySet * ks);

/* searching */
GElektraKey * gelektra_keyset_lookup (GElektraKeySet * ks, GElektraKey * key, GElektraKdbOptions options);
GElektraKey * gelektra_keyset_lookup_byname (GElektraKeySet * ks, const char * name, GElektraKdbOptions options);

/* iterating */
GElektraKey * gelektra_keyset_at (GElektraKeySet * ks, gssize pos);

G_END_DECLS

#endif
