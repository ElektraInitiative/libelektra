#include "elektra-xfconf-channel.h"
#include "elektra-xfconf-util.h"
#include "elektra-xfconf.h"

#include <gelektra-key.h>

typedef struct XfconfCache XfconfCache;
struct _XfconfChannel
{
	GObject parent;
	gchar * channel_name;
};

typedef struct XfconfChannelClass
{
	GObjectClass parent;

	void (*property_changed) (XfconfChannel * channel, const gchar * property, const GValue * value);
} XfconfChannelClass;

enum
{
	SIG_PROPERTY_CHANGED = 0,
	N_SIGS,
};

enum
{
	PROP0 = 0,
	PROP_CHANNEL_NAME,
};


static GObject * xfconf_channel_constructor (GType type, guint n_construct_properties, GObjectConstructParam * construct_properties);
static void xfconf_channel_set_g_property (GObject * object, guint property_id, const GValue * value, GParamSpec * pspec);
static void xfconf_channel_get_g_property (GObject * object, guint property_id, GValue * value, GParamSpec * pspec);
static void xfconf_channel_dispose (GObject * obj);
static void xfconf_channel_finalize (GObject * obj);

static void xfconf_channel_property_changed (XfconfCache * cache, const gchar * channel_name, const gchar * property, const GValue * value,
					     gpointer user_data);

static guint signals[N_SIGS] = {
	0,
};

G_DEFINE_TYPE (XfconfChannel, xfconf_channel, G_TYPE_OBJECT)


static void xfconf_channel_class_init (XfconfChannelClass * klass)
{
	trace ();
	GObjectClass * object_class = (GObjectClass *) klass;

	object_class->constructor = xfconf_channel_constructor;
	object_class->set_property = xfconf_channel_set_g_property;
	object_class->get_property = xfconf_channel_get_g_property;
	object_class->dispose = xfconf_channel_dispose;
	object_class->finalize = xfconf_channel_finalize;

	signals[SIG_PROPERTY_CHANGED] = g_signal_new (I_ ("property-changed"), XFCONF_TYPE_CHANNEL, G_SIGNAL_RUN_LAST | G_SIGNAL_DETAILED,
						      G_STRUCT_OFFSET (XfconfChannelClass, property_changed), NULL, NULL, NULL, G_TYPE_NONE,
						      2, G_TYPE_STRING, G_TYPE_VALUE);

	g_object_class_install_property (object_class, PROP_CHANNEL_NAME,
					 g_param_spec_string ("channel-name", "Channel Name", "The name of the channel", NULL,
							      G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY | G_PARAM_STATIC_NAME |
								      G_PARAM_STATIC_NICK | G_PARAM_STATIC_BLURB));
}

static void xfconf_channel_init (XfconfChannel * instance)
{
	trace ();
}

static GObject * xfconf_channel_constructor (GType type, guint n_construct_properties, GObjectConstructParam * construct_properties)
{
	trace ();
	const gchar * channel_name = NULL;
	guint i;
	XfconfChannel * channel = NULL;

	for (i = 0; i < n_construct_properties; ++i)
	{
		if (!strcmp (g_param_spec_get_name (construct_properties[i].pspec), "channel-name"))
			channel_name = g_value_get_string (construct_properties[i].value);
	}

	if (G_UNLIKELY (!channel_name))
	{
		g_warning ("Assertion 'channel_name != NULL' failed");
		return NULL;
	}

	channel = XFCONF_CHANNEL (
		G_OBJECT_CLASS (xfconf_channel_parent_class)->constructor (type, n_construct_properties, construct_properties));
	return G_OBJECT (channel);
}

static void xfconf_channel_set_g_property (GObject * object, guint property_id, const GValue * value, GParamSpec * pspec)
{
	trace ();
	XfconfChannel * channel = XFCONF_CHANNEL (object);

	if (property_id == PROP_CHANNEL_NAME)
	{
		g_assert (channel->channel_name == NULL);
		channel->channel_name = g_value_dup_string (value);
	}
	else
	{
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
	}
}

static void xfconf_channel_get_g_property (GObject * object, guint property_id, GValue * value, GParamSpec * pspec)
{
	trace ();
	XfconfChannel * channel = XFCONF_CHANNEL (object);

	if (property_id == PROP_CHANNEL_NAME)
	{
		g_value_set_string (value, channel->channel_name);
	}
	else
	{
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
	}
}

static void xfconf_channel_dispose (GObject * obj)
{
	trace ();
	g_debug ("xfconf_channel_dispose");
}

static void xfconf_channel_finalize (GObject * obj)
{
	trace ();
	XfconfChannel * channel = XFCONF_CHANNEL (obj);
	g_free (channel->channel_name);
	G_OBJECT_CLASS (xfconf_channel_parent_class)->finalize (obj);
}


static gint find_pair_by_name (gconstpointer raw_channel_pair, gconstpointer name)
{
	trace ();
	const ChannelKeySetPair * channel_pair = raw_channel_pair;
	return strcmp (channel_pair->channel->channel_name, name);
}


static gint compare_pairs (gconstpointer a, gconstpointer b)
{
	const ChannelKeySetPair * pair_a = a;
	const ChannelKeySetPair * pair_b = b;
	return strcmp (pair_a->channel->channel_name, pair_b->channel->channel_name);
}

static ChannelKeySetPair * find_or_create_channel_pair (const gchar * channel_name)
{
	trace ();
	GList * channel_item = g_list_find_custom (channel_list, channel_name, &find_pair_by_name);
	if (channel_item != NULL)
	{
		g_debug ("ChannelKeySetPair for channel %s already exists, returning it", channel_name);
		return channel_item->data;
	}
	g_debug ("ChannelKeySetPair for channel %s does not exist, creating it", channel_name);
	ChannelKeySetPair * channel_pair = malloc (sizeof (ChannelKeySetPair));
	channel_pair->channel = xfconf_channel_new (channel_name);
	channel_pair->keySet = gelektra_keyset_new (0, GELEKTRA_KEYSET_END);
	channel_list = g_list_insert_sorted (channel_list, channel_pair, &compare_pairs);
	return channel_pair;
}

XfconfChannel * xfconf_channel_get (const gchar * channel_name)
{
	trace ();
	return find_or_create_channel_pair (channel_name)->channel;
}

XfconfChannel * xfconf_channel_new (const gchar * channel_name)
{
	trace ();
	XfconfChannel * channel = g_object_new (XFCONF_TYPE_CHANNEL, "channel-name", channel_name, NULL);
	return channel;
}

XfconfChannel * xfconf_channel_new_with_property_base (const gchar * channel_name, const gchar * property_base)
{
	unimplemented ();
}

static GElektraKeySet * keySet_from_channel (const gchar * channel_name)
{
	trace ();
	gchar * key_name = malloc ((strlen (channel_name) + strlen (XFCONF_ROOT) + 2) * sizeof (char));
	sprintf (key_name, "%s/%s", XFCONF_ROOT, channel_name);
	ChannelKeySetPair * channel_pair = find_or_create_channel_pair (channel_name);
	GElektraKey * parent_key = gelektra_key_new (key_name, GELEKTRA_KEY_END);
	g_debug ("Fetch keys from parent: %s", key_name);
	switch (gelektra_kdb_get (gElektraKdb, channel_pair->keySet, parent_key))
	{
	case -1:
		g_warning ("There was a failure fetching the keys");
		break;
	case 0:
		g_debug ("The keyset did not change");
		break;
	case 1:
		g_debug ("Retrieved the keyset");
		break;
	default:
		g_error ("An unknown error occurred during keyset fetch");
	}
	GElektraKey * cur;
	gssize key_set_size = gelektra_keyset_len (channel_pair->keySet);
	g_debug ("KeySet has %ld keys", key_set_size);
	for (gssize i = 0; i < key_set_size; i++)
	{
		cur = gelektra_keyset_at (channel_pair->keySet, i);
		g_debug ("Found key: %s", gelektra_key_name (cur));
	}
	free (key_name);
	return channel_pair->keySet;
}

/**
 * Sets a value which is already formatted as a string.
 * @param channel the channel to store the value in
 * @param property the name of the key which should be set
 * @param value the string formatted value to set
 * @param g_type the type of the value, will be set as meta-key
 * @return
 */
static gboolean xfconf_channel_set_formatted (XfconfChannel * channel, const gchar * property, const gchar * value, GType g_type)
{
	trace ();
	if (!channel)
	{
		g_debug ("cannot proceed with null channel");
		return FALSE;
	}
	if (!property)
	{
		g_debug ("cannot proceed with null property");
		return FALSE;
	}
	gchar * property_name =
		malloc ((strlen (XFCONF_NAMESPACE) + strlen (XFCONF_ROOT) + strlen (channel->channel_name) + strlen (property) + 2) *
			sizeof (char));
	sprintf (property_name, "%s%s/%s%s", XFCONF_NAMESPACE, XFCONF_ROOT, channel->channel_name, property);
	GElektraKey * key = gelektra_key_new (property_name, GELEKTRA_KEY_END);
	g_debug ("set %s to %s (type %lu) on channel: %s", property_name, value, g_type, channel->channel_name);
	gelektra_key_setstring (key, value);
	gelektra_key_setmeta (key, XFCONF_GTYPE_META_NAME, g_type_name (g_type));
	GElektraKeySet * key_set = keySet_from_channel (channel->channel_name);
	gelektra_keyset_append (key_set, key);
	gchar * parent_key_name = malloc ((strlen (XFCONF_ROOT) + strlen (channel->channel_name) + 2) * sizeof (char));
	sprintf (parent_key_name, "%s/%s", XFCONF_ROOT, channel->channel_name);
	GElektraKey * parent_key = gelektra_key_new (parent_key_name, GELEKTRA_KEY_END);
	gint result_code = gelektra_kdb_set (gElektraKdb, key_set, parent_key);
	g_debug ("storing key set for parent key %s returned %d", parent_key_name, result_code);
	return result_code >= 0;
}

gboolean xfconf_channel_has_property (XfconfChannel * channel, const gchar * property)
{
	trace ();
	GElektraKeySet * key_set = keySet_from_channel (channel->channel_name);
	gchar * property_name = malloc ((strlen (XFCONF_ROOT) + strlen (channel->channel_name) + strlen (property) + 2) * sizeof (char));
	sprintf (property_name, "%s/%s%s", XFCONF_ROOT, channel->channel_name, property);
	g_debug ("request key %s on channel: %s which has %zd keys", property, channel->channel_name, gelektra_keyset_len (key_set));
	GElektraKey * key = gelektra_keyset_lookup_byname (key_set, property_name, GELEKTRA_KDB_O_NONE);
	g_debug ("channel %s has key %s: %d", channel->channel_name, property_name, key != NULL);
	return key != NULL;
}

gboolean xfconf_channel_is_property_locked (XfconfChannel * channel, const gchar * property)
{
	unimplemented ();
}

void xfconf_channel_reset_property (XfconfChannel * channel, const gchar * property_base, gboolean recursive)
{
	unimplemented ();
}

GHashTable * xfconf_channel_get_properties (XfconfChannel * channel, const gchar * property_base)
{
	unimplemented ();
}

static const gchar * g_value_to_string (GValue * g_value)
{
	trace ();
	GValue str = G_VALUE_INIT;
	g_value_init (&str, G_TYPE_STRING);
	g_value_transform (g_value, &str);
	return g_value_get_string (&str);
}

/* basic types */

gchar * xfconf_channel_get_string (XfconfChannel * channel, const gchar * property, const gchar * default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_string (XfconfChannel * channel, const gchar * property, const gchar * value)
{
	trace ();
	return xfconf_channel_set_formatted (channel, property, value, G_TYPE_STRING);
}

gint32 xfconf_channel_get_int (XfconfChannel * channel, const gchar * property, gint32 default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_int (XfconfChannel * channel, const gchar * property, gint32 value)
{
	unimplemented ();
}

static gint64 xfconf_channel_get_int64 (XfconfChannel * channel, const gchar * property, gint64 default_value)
{
	unimplemented ();
}
static gboolean xfconf_channel_set_int64 (XfconfChannel * channel, const gchar * property, gint64 value)
{
	unimplemented ();
}

guint32 xfconf_channel_get_uint (XfconfChannel * channel, const gchar * property, guint32 default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_uint (XfconfChannel * channel, const gchar * property, guint32 value)
{
	unimplemented ();
}

guint64 xfconf_channel_get_uint64 (XfconfChannel * channel, const gchar * property, guint64 default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_uint64 (XfconfChannel * channel, const gchar * property, guint64 value)
{
	unimplemented ();
}

static glong xfconf_channel_get_long (XfconfChannel * channel, const gchar * property, glong default_value)
{
	unimplemented ();
}
static gboolean xfconf_channel_set_long (XfconfChannel * channel, const gchar * property, glong value)
{
	unimplemented ();
}

static gulong xfconf_channel_get_ulong (XfconfChannel * channel, const gchar * property, gulong default_value)
{
	unimplemented ();
}
static gboolean xfconf_channel_set_ulong (XfconfChannel * channel, const gchar * property, gulong value)
{
	unimplemented ();
}

static gfloat xfconf_channel_get_float (XfconfChannel * channel, const gchar * property, gfloat default_value)
{
	unimplemented ();
}
static gboolean xfconf_channel_set_float (XfconfChannel * channel, const gchar * property, gfloat value)
{
	unimplemented ();
}

gdouble xfconf_channel_get_double (XfconfChannel * channel, const gchar * property, gdouble default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_double (XfconfChannel * channel, const gchar * property, gdouble value)
{
	unimplemented ();
}

gboolean xfconf_channel_get_bool (XfconfChannel * channel, const gchar * property, gboolean default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_bool (XfconfChannel * channel, const gchar * property, gboolean value)
{
	trace ();
	GValue g_value = G_VALUE_INIT;
	g_value_init (&g_value, G_TYPE_BOOLEAN);
	g_value_set_boolean (&g_value, value);
	return xfconf_channel_set_formatted (channel, property, g_value_to_string (&g_value), G_TYPE_BOOLEAN);
}

/* this is just convenience API for the array stuff, where
 * all the values are G_TYPE_STRING */
gchar ** xfconf_channel_get_string_list (XfconfChannel * channel, const gchar * property)
{
	unimplemented ();
}
gboolean xfconf_channel_set_string_list (XfconfChannel * channel, const gchar * property, const gchar * const * values)
{
	unimplemented ();
}

/* really generic API - can set some value types that aren't
 * supported by the basic type API, e.g., char, signed short,
 * unsigned int, etc.  no, you can't set arbitrary GTypes. */
gboolean xfconf_channel_get_property (XfconfChannel * channel, const gchar * property, GValue * value)
{
	trace ();
	GElektraKeySet * key_set = keySet_from_channel (channel->channel_name);
	gchar * property_name = malloc ((strlen (XFCONF_ROOT) + strlen (channel->channel_name) + strlen (property) + 2) * sizeof (char));
	sprintf (property_name, "%s/%s%s", XFCONF_ROOT, channel->channel_name, property);
	g_debug ("request key %s with type %lu, on channel: %s which has %zd keys", property, value->g_type, channel->channel_name,
		 gelektra_keyset_len (key_set));
	GElektraKey * key = gelektra_keyset_lookup_byname (key_set, property_name, GELEKTRA_KDB_O_NONE);
	if (key == NULL)
	{
		g_debug ("got null from keyset by looking up %s", property_name);
		return FALSE;
	}
	value->g_type = G_TYPE_STRING;
	const gchar * key_value = gelektra_key_string (key);
	g_debug ("Found value %s to key %s", key_value, property_name);
	g_value_set_string (value, key_value);
	free (property_name);
	return TRUE;
}
gboolean xfconf_channel_set_property (XfconfChannel * channel, const gchar * property, const GValue * value)
{
	trace ();
	if (!value)
	{
		g_debug ("try to store a null type, interpret it as a string");
		return xfconf_channel_set_string (channel, property, NULL);
	}
	if (!G_IS_VALUE (value))
	{
		g_warning ("THe provided value was not initialized, cannot proceed");
		return FALSE;
	}
	g_debug ("Seems working");
	switch (G_VALUE_TYPE (value))
	{
	case G_TYPE_STRING:
		return xfconf_channel_set_string (channel, property, g_value_get_string (value));
	case G_TYPE_BOOLEAN:
		return xfconf_channel_set_bool (channel, property, g_value_get_boolean (value));
	case G_TYPE_INT:
		return xfconf_channel_set_int (channel, property, g_value_get_int (value));
	case G_TYPE_UINT:
		return xfconf_channel_set_uint (channel, property, g_value_get_uint (value));
	case G_TYPE_INT64:
		return xfconf_channel_set_int64 (channel, property, g_value_get_int64 (value));
	case G_TYPE_UINT64:
		return xfconf_channel_set_uint64 (channel, property, g_value_get_uint64 (value));
	case G_TYPE_FLOAT:
		return xfconf_channel_set_float (channel, property, g_value_get_float (value));
	case G_TYPE_DOUBLE:
		return xfconf_channel_set_double (channel, property, g_value_get_double (value));
	default:
		g_warning ("Unrecognized type: %s", G_VALUE_TYPE_NAME (value));
		return FALSE;
	}
}

/* array types - arrays can be made up of values of arbitrary
 * (and mixed) types, even some not supported by the basic
 * type API */

gboolean xfconf_channel_get_array (XfconfChannel * channel, const gchar * property, GType first_value_type, ...)
{
	unimplemented ();
}
gboolean xfconf_channel_get_array_valist (XfconfChannel * channel, const gchar * property, GType first_value_type, va_list var_args)
{
	unimplemented ();
}
GPtrArray * xfconf_channel_get_arrayv (XfconfChannel * channel, const gchar * property)
{
	unimplemented ();
}

gboolean xfconf_channel_set_array (XfconfChannel * channel, const gchar * property, GType first_value_type, ...)
{
	unimplemented ();
}
gboolean xfconf_channel_set_array_valist (XfconfChannel * channel, const gchar * property, GType first_value_type, va_list var_args)
{
	unimplemented ();
}
gboolean xfconf_channel_set_arrayv (XfconfChannel * channel, const gchar * property, GPtrArray * values)
{
	unimplemented ();
}

/* struct types */

gboolean xfconf_channel_get_named_struct (XfconfChannel * channel, const gchar * property, const gchar * struct_name, gpointer value_struct)
{
	unimplemented ();
}
gboolean xfconf_channel_set_named_struct (XfconfChannel * channel, const gchar * property, const gchar * struct_name, gpointer value_struct)
{
	unimplemented ();
}

gboolean xfconf_channel_get_struct (XfconfChannel * channel, const gchar * property, gpointer value_struct, GType first_member_type, ...)
{
	unimplemented ();
}
gboolean xfconf_channel_get_struct_valist (XfconfChannel * channel, const gchar * property, gpointer value_struct, GType first_member_type,
					   va_list var_args)
{
	unimplemented ();
}
gboolean xfconf_channel_get_structv (XfconfChannel * channel, const gchar * property, gpointer value_struct, guint n_members,
				     GType * member_types)
{
	unimplemented ();
}

gboolean xfconf_channel_set_struct (XfconfChannel * channel, const gchar * property, const gpointer value_struct, GType first_member_type,
				    ...)
{
	unimplemented ();
}
gboolean xfconf_channel_set_struct_valist (XfconfChannel * channel, const gchar * property, const gpointer value_struct,
					   GType first_member_type, va_list var_args)
{
	unimplemented ();
}
gboolean xfconf_channel_set_structv (XfconfChannel * channel, const gchar * property, const gpointer value_struct, guint n_members,
				     GType * member_types)
{
	unimplemented ();
}
