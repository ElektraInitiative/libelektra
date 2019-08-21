#ifndef G_ELEKTRA_KEY_H
#define G_ELEKTRA_KEY_H

#include <elektra/kdb.h>
#include <glib-object.h>

G_BEGIN_DECLS

/*
 * Type macros.
 */
#define GELEKTRA_TYPE_KEY (gelektra_key_get_type ())
#define GELEKTRA_KEY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GELEKTRA_TYPE_KEY, GElektraKey))
#define GELEKTRA_IS_KEY(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GELEKTRA_TYPE_KEY))
#define GELEKTRA_KEY_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GELEKTRA_TYPE_KEY, GElektraKeyClass))
#define GELEKTRA_IS_KEY_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GELEKTRA_TYPE_KEY))
#define GELEKTRA_KEY_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GELEKTRA_TYPE_KEY, GElektraKeyClass))

typedef struct _GElektraKey GElektraKey;
typedef struct _GElektraKeyClass GElektraKeyClass;

// clang-format off
typedef enum {
	GELEKTRA_KEY_FLAGS          = KEY_FLAGS,
	GELEKTRA_KEY_END            = KEY_END,
	GELEKTRA_KEY_NAME           = KEY_NAME,
	GELEKTRA_KEY_VALUE          = KEY_VALUE,
	GELEKTRA_KEY_OWNER          = KEY_OWNER,
	GELEKTRA_KEY_COMMENT        = KEY_COMMENT,
	GELEKTRA_KEY_BINARY         = KEY_BINARY,
	GELEKTRA_KEY_UID            = KEY_UID,
	GELEKTRA_KEY_GID            = KEY_GID,
	GELEKTRA_KEY_MODE           = KEY_MODE,
	GELEKTRA_KEY_ATIME          = KEY_ATIME,
	GELEKTRA_KEY_MTIME          = KEY_MTIME,
	GELEKTRA_KEY_CTIME          = KEY_CTIME,
	GELEKTRA_KEY_SIZE           = KEY_SIZE,
	GELEKTRA_KEY_FUNC           = KEY_FUNC,
	GELEKTRA_KEY_DIR            = KEY_DIR,
	GELEKTRA_KEY_META           = KEY_META,
	GELEKTRA_KEY_NULL           = KEY_NULL,
	GELEKTRA_KEY_CASCADING_NAME = KEY_CASCADING_NAME,
	GELEKTRA_KEY_META_NAME      = KEY_META_NAME
} GElektraKeySwitch;
// clang-format on

struct _GElektraKey
{
	GObject parent_instance;

	/* instance members */
	Key * key;
};

struct _GElektraKeyClass
{
	GObjectClass parent_class;

	/* class members */
};

/* used by GELEKTRA_TYPE_KEY */
GType gelektra_key_get_type (void);


/*
 * Method definitions.
 */

/* constructor */
GElektraKey * gelektra_key_new (const gchar * name, ...);
GElektraKey * gelektra_key_make (Key * key);
GElektraKey * gelektra_key_gi_make (GElektraKey * key);

/* initialization */
void gelektra_key_gi_init (GElektraKey * key, const gchar * name, int flags, const gchar * value, const void * data, gsize data_size);

/* reference handling */
gssize gelektra_key_incref (GElektraKey * key);
gssize gelektra_key_decref (GElektraKey * key);
gssize gelektra_key_getref (const GElektraKey * key);

/* basic methods */
GElektraKey * gelektra_key_dup (const GElektraKey * key);
gint gelektra_key_copy (const GElektraKey * key, GElektraKey * dest);
gint gelektra_key_clear (GElektraKey * key);

/* operators */
gboolean gelektra_key_equal (const GElektraKey * key, const GElektraKey * other);
gint gelektra_key_cmp (const GElektraKey * key, const GElektraKey * other);

/* name manipulation */
gssize gelektra_key_setname (GElektraKey * key, const char * name);
gssize gelektra_key_setbasename (GElektraKey * key, const char * basename);
gssize gelektra_key_addbasename (GElektraKey * key, const char * basename);

gssize gelektra_key_getnamesize (const GElektraKey * key);
gssize gelektra_key_getbasenamesize (const GElektraKey * key);
gssize gelektra_key_getfullnamesize (const GElektraKey * key);

/* value operations */
gssize gelektra_key_setstring (GElektraKey * key, const gchar * value);
gssize gelektra_key_getstring (const GElektraKey * key, gchar * out, gsize size);
gchar * gelektra_key_gi_getstring (const GElektraKey * key);

gssize gelektra_key_setbinary (GElektraKey * key, const void * data, gsize size);
gssize gelektra_key_getbinary (const GElektraKey * key, void * out, gsize size);
void * gelektra_key_gi_getbinary (const GElektraKey * key, gssize * data_size);

const void * gelektra_key_getvalue (const GElektraKey * key);
gssize gelektra_key_getvaluesize (const GElektraKey * key);

typedef void (*gelektra_func_t) (void);
gelektra_func_t gelektra_key_getfunc (const GElektraKey * key);

/* metadata */
gssize gelektra_key_setmeta (GElektraKey * key, const gchar * name, const gchar * value);
gboolean gelektra_key_hasmeta (const GElektraKey * key, const gchar * name);
GElektraKey * gelektra_key_getmeta (const GElektraKey * key, const gchar * name);
gint gelektra_key_copymeta (const GElektraKey * key, GElektraKey * dest, const gchar * name);
gint gelektra_key_copyallmeta (const GElektraKey * key, GElektraKey * dest);
gint gelektra_key_rewindmeta (GElektraKey * key);
GElektraKey * gelektra_key_nextmeta (GElektraKey * key);
GElektraKey * gelektra_key_currentmeta (const GElektraKey * key);

/* validating */
gboolean gelektra_key_isnull (const GElektraKey * key);
gboolean gelektra_key_isvalid (const GElektraKey * key);
gboolean gelektra_key_issystem (const GElektraKey * key);
gboolean gelektra_key_isuser (const GElektraKey * key);
gboolean gelektra_key_isstring (const GElektraKey * key);
gboolean gelektra_key_isbinary (const GElektraKey * key);
gboolean gelektra_key_isinactive (const GElektraKey * key);
gboolean gelektra_key_isbelow (const GElektraKey * key, const GElektraKey * other);
gboolean gelektra_key_isbeloworsame (const GElektraKey * key, const GElektraKey * other);
gboolean gelektra_key_isdirectbelow (const GElektraKey * key, const GElektraKey * other);

G_END_DECLS

#endif
