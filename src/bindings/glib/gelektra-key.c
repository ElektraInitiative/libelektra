#include <elektra/glib/gelektra-key.h>
#include <string.h>

enum
{
	PROP_0,
	PROP_KEY_NAME, ///< property to get the name of the key
	PROP_KEY_BASENAME,
	N_PROPERTIES
};

G_DEFINE_TYPE (GElektraKey, gelektra_key, G_TYPE_OBJECT)
static Key * gelektra_key_swap (GElektraKey * key, Key * newkey);

static void gelektra_key_init (GElektraKey * self)
{
	/* initialize the object */
	self->key = keyNew ("/", KEY_END);
	keyIncRef (self->key);
}

static void gelektra_key_set_property (GObject * object, guint property_id, const GValue * value __attribute__ ((unused)),
				       GParamSpec * pspec)
{
	G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
}

static void gelektra_key_get_property (GObject * object, guint property_id, GValue * value, GParamSpec * pspec)
{
	GElektraKey * self = GELEKTRA_KEY (object);

	switch (property_id)
	{
	case PROP_KEY_NAME:
		g_value_set_string (value, keyName (self->key));
		break;
	case PROP_KEY_BASENAME:
		g_value_set_string (value, keyBaseName (self->key));
		break;
	default:
		/* We don't have any other property... */
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
		break;
	}
}

static void gelektra_key_finalize (GObject * object)
{
	GElektraKey * self = GELEKTRA_KEY (object);

	keyDecRef (self->key);
	keyDel (self->key);

	/* Always chain up to the parent class; as with dispose(), finalize()
	 * is guaranteed to exist on the parent's class virtual function table
	 */
	G_OBJECT_CLASS (gelektra_key_parent_class)->finalize (object);
}

static GParamSpec * obj_properties[N_PROPERTIES] = {
	NULL,
};

static void gelektra_key_class_init (GElektraKeyClass * klass)
{
	GObjectClass * gobject_class = G_OBJECT_CLASS (klass);

	gobject_class->set_property = gelektra_key_set_property;
	gobject_class->get_property = gelektra_key_get_property;
	gobject_class->finalize = gelektra_key_finalize;

	obj_properties[PROP_KEY_NAME] = g_param_spec_string ("name", "Keyname", "The name of the key", NULL, G_PARAM_READWRITE);

	obj_properties[PROP_KEY_BASENAME] =
		g_param_spec_string ("basename", "Basename", "The basename of the key", NULL, G_PARAM_READWRITE);

	g_object_class_install_properties (gobject_class, N_PROPERTIES, obj_properties);
}


/*
 * Methods
 */

/* constructor */
/**
 * gelektra_key_new: (skip)
 * @name valid name of a key or NULL
 * @... optional parameters closed by GELEKTRA_KEY_END, see keyNew
 *
 * Returns: A new #GElektraKey
 * see keyNew
 */
GElektraKey * gelektra_key_new (const gchar * name, ...)
{
	va_list va;

	GElektraKey * key = g_object_new (GELEKTRA_TYPE_KEY, NULL);
	if (name)
	{
		va_start (va, name);
		Key * newkey = keyVNew (name, va);
		va_end (va);
		if (newkey == NULL) return NULL;

		Key * old = gelektra_key_swap (key, newkey);
		keyDel (old);
	}
	return key;
}

/**
 * gelektra_key_make: (skip)
 * @key: The underlying key
 *
 * Returns: (transfer full): A new #GElektraKey holding the ownership of @key
 */
GElektraKey * gelektra_key_make (Key * key)
{
	if (key == NULL) return NULL;
	GElektraKey * ret = gelektra_key_new (NULL);
	Key * old = gelektra_key_swap (ret, key);
	keyDel (old);
	return ret;
}

/**
 * gelektra_key_gi_make: (constructor)
 * @key: A #GElektraKey
 *
 * Returns: (transfer full): A new #GElektraKey holding the ownership of @key
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_key_make instead
 */
GElektraKey * gelektra_key_gi_make (GElektraKey * key)
{
	return (key != NULL) ? gelektra_key_make (key->key) : NULL;
}

/* initialization */
static void gelektra_key_gi_init_va (GElektraKey * key, const gchar * name, ...)
{
	if (!name) return;
	va_list va;
	va_start (va, name);
	Key * newkey = keyVNew (name, va);
	va_end (va);
	if (newkey == NULL) return;

	Key * old = gelektra_key_swap (key, newkey);
	keyDel (old);
}

/**
 * gelektra_key_gi_init
 * @name valid name of a key or NULL
 * @flags see usage of KEY_FLAGS in keyNew
 * @value: (nullable): key value
 * @data: (array length=data_size zero-terminated=0) (element-type guint8) (nullable): binary key value
 * @data_size: size of the input data
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_key_new instead
 */
void gelektra_key_gi_init (GElektraKey * key, const gchar * name, int flags, const gchar * value, const void * data, gsize data_size)
{
	gelektra_key_gi_init_va (key, name, KEY_FLAGS, flags, KEY_SIZE, (flags & KEY_BINARY) ? data_size : 0, KEY_VALUE,
				 (flags & KEY_BINARY) ? data : value, KEY_END);
}

/* reference handling */
guint16 gelektra_key_incref (GElektraKey * key)
{
	return keyIncRef (key->key);
}

guint16 gelektra_key_decref (GElektraKey * key)
{
	return keyDecRef (key->key);
}

guint16 gelektra_key_getref (const GElektraKey * key)
{
	return keyGetRef (key->key);
}

/* basic methods */
/**
 * gelektra_key_dup:
 * @key: A #GElektraKey
 *
 * Returns: (transfer full): A duplicated #GElektraKey
 * see keyDup
 */
GElektraKey * gelektra_key_dup (const GElektraKey * key, elektraCopyFlags flags)
{
	return gelektra_key_make (keyDup (key->key, flags));
}

/**
 * gelektra_key_copy: (skip)
 * see keyCopy
 */
GElektraKey * gelektra_key_copy (const GElektraKey * key, GElektraKey * dest, elektraCopyFlags flags)
{
	Key * ret = keyCopy (dest->key, key->key, flags);
	return ret == NULL ? NULL : dest;
}

gint gelektra_key_clear (GElektraKey * key)
{
	return keyClear (key->key);
}

/**
 * gelektra_key_swap: (skip)
 * @key: A #GElektraKey
 * @newkey: The new underlying key
 *
 * Returns: The old underlying key
 *
 * \note #GElektraKey also swaps ownership of the underlying keys
 */
static Key * gelektra_key_swap (GElektraKey * key, Key * newkey)
{
	keyDecRef (key->key);
	Key * oldkey = key->key;
	key->key = newkey;
	keyIncRef (key->key);
	return oldkey;
}

/* operators */
gboolean gelektra_key_equal (const GElektraKey * key, const GElektraKey * other)
{
	return keyCmp (key->key, other->key) == 0;
}

gint gelektra_key_cmp (const GElektraKey * key, const GElektraKey * other)
{
	return keyCmp (key->key, other->key);
}

/* name manipulation */
gssize gelektra_key_setname (GElektraKey * key, const char * name)
{
	return keySetName (key->key, name);
}

const gchar * gelektra_key_name (const GElektraKey * key)
{
	return keyName (key->key);
}

gssize gelektra_key_setbasename (GElektraKey * key, const char * basename)
{
	return keySetBaseName (key->key, basename);
}

gssize gelektra_key_addbasename (GElektraKey * key, const char * basename)
{
	return keyAddBaseName (key->key, basename);
}

gssize gelektra_key_getnamesize (const GElektraKey * key)
{
	return keyGetNameSize (key->key);
}

gssize gelektra_key_getbasenamesize (const GElektraKey * key)
{
	return keyGetBaseNameSize (key->key);
}

/* value operations */
gssize gelektra_key_setstring (GElektraKey * key, const gchar * value)
{
	return keySetString (key->key, value);
}

/**
 * gelektra_key_getstring: (skip)
 * see keyGetString
 */
gssize gelektra_key_getstring (const GElektraKey * key, gchar * out, gsize size)
{
	return keyGetString (key->key, out, size);
}

const gchar * gelektra_key_string (const GElektraKey * key)
{
	return keyString (key->key);
}

/**
 * gelektra_key_gi_getstring:
 * @key: A #GElektraKey
 *
 * Returns: (transfer full): a copy of the key value
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_key_getstring instead
 */
gchar * gelektra_key_gi_getstring (const GElektraKey * key)
{
	gssize size = keyGetValueSize (key->key);
	if (size <= 0) return NULL;

	gchar * data = g_malloc0 (size);
	if (keyGetString (key->key, data, size) <= 0)
	{
		g_free (data);
		return NULL;
	}
	return data;
}

/**
 * gelektra_key_setbinary:
 * @key: A #GElektraKey
 * @data: (array length=size zero-terminated=0) (element-type guint8): binary data
 * @size: size of the input data
 */
gssize gelektra_key_setbinary (GElektraKey * key, const void * data, gsize size)
{
	return keySetBinary (key->key, data, size);
}

/**
 * gelektra_key_getbinary: (skip)
 * see keyGetBinary
 */
gssize gelektra_key_getbinary (const GElektraKey * key, void * out, gsize size)
{
	return keyGetBinary (key->key, out, size);
}


/**
 * gelektra_key_gi_getbinary:
 * @key: A #GElektraKey
 * @data_size: size of the returned data
 *
 * Returns: (array length=data_size zero-terminated=0) (element-type guint8) (transfer full): binary data
 *
 * \note This is for GObject Introspection.
 * \note Do NOT use! Use gelektra_key_getbinary instead
 */
void * gelektra_key_gi_getbinary (const GElektraKey * key, gssize * data_size)
{
	*data_size = 0;
	gssize size = keyGetValueSize (key->key);
	if (size <= 0) return NULL;

	void * data = g_malloc0 (size);
	if ((*data_size = keyGetBinary (key->key, data, size)) <= 0)
	{
		g_free (data);
		return NULL;
	}
	return data;
}

/**
 * gelektra_key_getvalue: (skip)
 * @key: A #GElektraKey
 *
 * Returns: see keyValue
 */
const void * gelektra_key_getvalue (const GElektraKey * key)
{
	return keyValue (key->key);
}

gssize gelektra_key_getvaluesize (const GElektraKey * key)
{
	return keyGetValueSize (key->key);
}

/**
 * gelektra_key_getfunc: (skip)
 * @key: A #GElektraKey
 *
 * Returns: a function pointer stored with gelektra_key_setbinary
 */
gelektra_func_t gelektra_key_getfunc (const GElektraKey * key)
{
	union
	{
		gelektra_func_t f;
		void * v;
	} data;

	if (keyGetBinary (key->key, &data.v, sizeof (data)) == sizeof (data)) return data.f;
	return NULL;
}

/* metadata */
gssize gelektra_key_setmeta (GElektraKey * key, const gchar * name, const gchar * value)
{
	return keySetMeta (key->key, name, value);
}

gboolean gelektra_key_hasmeta (const GElektraKey * key, const gchar * name)
{
	return (keyValue (keyGetMeta (key->key, name)) != NULL);
}

/**
 * gelektra_key_getmeta:
 * @key: A #GElektraKey
 * @name: the name of the meta information
 *
 * Returns: (transfer full): A #GElektraKey holding the meta information
 * see keyGetMeta
 */
GElektraKey * gelektra_key_getmeta (const GElektraKey * key, const gchar * name)
{
	return gelektra_key_make ((Key *) keyGetMeta (key->key, name));
}

gint gelektra_key_copymeta (const GElektraKey * key, GElektraKey * dest, const gchar * name)
{
	return keyCopyMeta (dest->key, key->key, name);
}

gint gelektra_key_copyallmeta (const GElektraKey * key, GElektraKey * dest)
{
	return keyCopyAllMeta (dest->key, key->key);
}

/**
 * gelektra_key_meta:
 * @key: A #GElektraKey
 *
 * Returns: (transfer full): A #GElektraKeySet holding all metakeys of the given key
 * see keyMeta
 */
GElektraKeySet * gelektra_key_meta (const GElektraKey * key)
{
	return gelektra_keyset_make ((KeySet *) keyMeta (key->key));
}


/* validating */
gboolean gelektra_key_isnull (const GElektraKey * key)
{
	return key->key == NULL;
}

gboolean gelektra_key_isvalid (const GElektraKey * key)
{
	return keyGetNameSize (key->key) > 1;
}

gboolean gelektra_key_issystem (const GElektraKey * key)
{
	return !strncmp (keyName (key->key), "system:/", sizeof ("system:/") - 1);
}

gboolean gelektra_key_isuser (const GElektraKey * key)
{
	return !strncmp (keyName (key->key), "user:/", sizeof ("user:/") - 1);
}

gboolean gelektra_key_isstring (const GElektraKey * key)
{
	return keyIsString (key->key);
}

gboolean gelektra_key_isbinary (const GElektraKey * key)
{
	return keyIsBinary (key->key);
}

gboolean gelektra_key_isbelow (const GElektraKey * key, const GElektraKey * other)
{
	return keyIsBelow (other->key, key->key);
}

gboolean gelektra_key_isbeloworsame (const GElektraKey * key, const GElektraKey * other)
{
	return keyIsBelowOrSame (other->key, key->key);
}

gboolean gelektra_key_isdirectbelow (const GElektraKey * key, const GElektraKey * other)
{
	return keyIsDirectlyBelow (other->key, key->key);
}
