#include "elektra-xfconf-channel.h"
#include "elektra-xfconf-binding.h"
#include "elektra-xfconf-util.h"
#include "elektra-xfconf.h"
#include <kdbhelper.h>
#include <sys/mman.h>

#define XFCONF_GET_TYPED(TYP, FUNC, SET_FUNC)                                                                                              \
	GValue val = G_VALUE_INIT;                                                                                                         \
	g_value_init (&val, TYP);                                                                                                          \
	if (xfconf_channel_get_formatted (channel, property, &val))                                                                        \
	{                                                                                                                                  \
		return FUNC (&val);                                                                                                        \
	}                                                                                                                                  \
	else                                                                                                                               \
	{                                                                                                                                  \
		if (XFCONF_PERSIST_DEFAULT)                                                                                                \
		{                                                                                                                          \
			SET_FUNC (channel, property, default_value);                                                                       \
		}                                                                                                                          \
		return default_value;                                                                                                      \
	}

typedef struct XfconfCache XfconfCache;


typedef struct XfconfChannelClass
{
	GObjectClass parent;

	void (ELEKTRA_UNUSED * property_changed) (XfconfChannel * channel, const gchar * property, const GValue * value);
} XfconfChannelClass;

enum
{
	SIG_PROPERTY_CHANGED = 0,
	N_SIGS,
};

enum
{
	PROP0 ELEKTRA_UNUSED = 0,
	PROP_CHANNEL_NAME,
};


static GObject * xfconf_channel_constructor (GType type, guint n_construct_properties, GObjectConstructParam * construct_properties);
static void xfconf_channel_set_g_property (GObject * object, guint property_id, const GValue * value, GParamSpec * pspec);
static void xfconf_channel_get_g_property (GObject * object, guint property_id, GValue * value, GParamSpec * pspec);
static void xfconf_channel_dispose (GObject * obj);
static void xfconf_channel_finalize (GObject * obj);

static ELEKTRA_UNUSED void xfconf_channel_property_changed (XfconfCache * cache ELEKTRA_UNUSED, const gchar * channel_name ELEKTRA_UNUSED,
							    const gchar * property ELEKTRA_UNUSED, const GValue * value ELEKTRA_UNUSED,
							    gpointer user_data ELEKTRA_UNUSED)
{
	trace ();
	unimplemented ();
}

static ELEKTRA_UNUSED guint signals[N_SIGS] = {
	0,
};

G_DEFINE_TYPE (XfconfChannel, xfconf_channel, G_TYPE_OBJECT)


static char * propertyWithChannelPrefix (const XfconfChannel * channel, const gchar * property);
static ELEKTRA_UNUSED void xfconf_channel_class_init (XfconfChannelClass * klass)
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

static ELEKTRA_UNUSED void xfconf_channel_init (XfconfChannel * instance ELEKTRA_UNUSED)
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

static void xfconf_channel_dispose (GObject * obj ELEKTRA_UNUSED)
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

static const gchar * g_value_to_string (GValue * g_value)
{
	trace ();
	GValue str = G_VALUE_INIT;
	g_value_init (&str, G_TYPE_STRING);
	g_value_transform (g_value, &str);
	return g_value_get_string (&str);
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

static ChannelKeySetPair * find_or_create_channel_pair (const gchar * channel_name, uint already_locked)
{
	trace ();
	if (already_locked != 1)
	{
		require_channel_read_lock ()
	}
	GList * channel_item = g_list_find_custom (channel_list, channel_name, &find_pair_by_name);
	if (already_locked != 1)
	{
		release_channel_lock ()
	}
	if (channel_item != NULL)
	{
		g_debug ("ChannelKeySetPair for channel %s already exists, returning it", channel_name);
		return channel_item->data;
	}
	g_debug ("ChannelKeySetPair for channel %s does not exist, creating it", channel_name);
	ChannelKeySetPair * channel_pair = malloc (sizeof (ChannelKeySetPair));
	channel_pair->channel = xfconf_channel_new (channel_name);
	if (XFCONF_INDIVIDUAL_KEY_SETS == 1)
	{
		channel_pair->keySet = ksNew (0, KS_END);
	}
	else
	{
		channel_pair->keySet = globalKeySet; // new backend loads all keys which belong to the same backend in an atomic way, using
						     // the same pointer for all channels
	}
	if (already_locked != 1)
	{
		require_channel_write_lock ()
	}
	channel_list = g_list_insert_sorted (channel_list, channel_pair, &compare_pairs);
	if (already_locked != 1)
	{
		release_channel_lock ()
	}
	return channel_pair;
}

KeySet * ksDeepDup (const KeySet * ks)
{
	trace ();
	KeySet * dup = ksNew (ksGetSize (ks), KS_END);
	for (elektraCursor i = 0; i < ksGetSize (ks); i++)
	{
		ksAppendKey (dup, keyDup (ksAtCursor (ks, i), KEY_CP_ALL));
	}
	return dup;
}

XfconfChannel * xfconf_channel_get (const gchar * channel_name)
{
	trace ();
	XfconfChannel * channel = find_or_create_channel_pair (channel_name, 0)->channel;
	return channel;
}

XfconfChannel * xfconf_channel_new (const gchar * channel_name)
{
	trace ();
	XfconfChannel * channel = g_object_new (XFCONF_TYPE_CHANNEL, "channel-name", channel_name, NULL);
	return channel;
}

ELEKTRA_UNUSED XfconfChannel * xfconf_channel_new_with_property_base (const gchar * channel_name ELEKTRA_UNUSED,
								      const gchar * property_base ELEKTRA_UNUSED)
{
	unimplemented ();
	return NULL;
}

/**
 * Channel name to key name - construct the absolute elektra key name from an Xconf channel name.
 *
 * @param channelName the channel name to convert to, must be a valid pointer
 * @return a new pointer to a string with the elektra key name, must be freed by the caller
 */
static char * channelNameToKeyName (const char * channelName)
{
	char * kdbKeyName = malloc ((strlen (channelName) + strlen (XFCONF_ROOT) + strlen (XFCONF_NAMESPACE) + 3) * sizeof (char));
	sprintf (kdbKeyName, "%s%s/%s", XFCONF_NAMESPACE, XFCONF_ROOT, channelName);
	return kdbKeyName;
}

static KeySet * keySet_from_channel (const gchar * channel_name, uint reference, uint already_locked)
{
	trace ();
	char * kdbKeyName;
	if (XFCONF_INDIVIDUAL_KEY_SETS == 1)
	{
		kdbKeyName = channelNameToKeyName (channel_name);
	}
	else
	{
		kdbKeyName = channelNameToKeyName ("");
	}
	ChannelKeySetPair * channelPair = find_or_create_channel_pair (channel_name, 1);
	Key * parentKey = keyNew (kdbKeyName, KEY_END);
	g_debug ("Fetch keys from parent: %s", kdbKeyName);
	if (already_locked != 1)
	{
		require_channel_write_lock ()
	}
	g_debug ("Refreshing keyset for channel %s from kdb, currently %ld keys", channel_name, ksGetSize (channelPair->keySet));
	int getStatusCode = kdbGet (elektraKdb, channelPair->keySet, parentKey);
	g_debug ("Refreshed keyset (status code was %d) now has %ld keys", getStatusCode, ksGetSize (channelPair->keySet));
	if (already_locked != 1)
	{
		release_channel_lock ()
	}

	switch (getStatusCode)
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
	case 2:
		g_debug ("No data was loaded from storage");
		break;
	default:
		g_warning ("An unknown status code(%d) occurred during keyset fetch", getStatusCode);
	}
	if (XFCONF_DEBUG_LOG_FOUND_KEYS == 1)
	{
		Key * cur;
		if (already_locked != 1)
		{
			require_channel_read_lock ()
		}
		ssize_t keySetSize = ksGetSize (channelPair->keySet);
		g_debug ("KeySet has %ld keys", keySetSize);
		for (elektraCursor i = 0; i < keySetSize; i++)
		{
			cur = ksAtCursor (channelPair->keySet, i);
			g_debug ("Found key: %s", keyName (cur));
		}
		if (already_locked != 1)
		{
			release_channel_lock ()
		}
		free (kdbKeyName);
	}
	if (reference != 0)
	{
		return channelPair->keySet;
	}
	if (already_locked != 1)
	{
		require_channel_read_lock ()
	}
	KeySet * deepCopy = ksDeepDup (channelPair->keySet);
	if (already_locked != 1)
	{
		release_channel_lock ()
	}
	return deepCopy;
}

/**
 * Convert a string to a GValue - This function tries to parse the content of `new_value` and stores it in the `g_value` pointer.
 * If that is not possible, e.g. the `g_value` has an unknown type, this function does nothing.
 * The desired type is taken from the `g_value`.
 * @param g_value the value to parse the string to - not null
 * @param new_value the string of the new value - not null
 * @retval 0 on success and 1 otherwise
 */
static int g_value_convert_string (GValue * g_value, const char * new_value)
{
	trace ();
	switch (g_value->g_type)
	{
	case G_TYPE_INT:
		g_value_set_int (g_value, (int) strtol (new_value, NULL, 10));
		break;
	case G_TYPE_INT64:
		g_value_set_int64 (g_value, strtol (new_value, NULL, 10));
		break;
	case G_TYPE_UINT:
		g_value_set_uint (g_value, (uint) strtoull (new_value, NULL, 10));
		break;
	case G_TYPE_UINT64:
		g_value_set_uint64 (g_value, strtoull (new_value, NULL, 10));
		break;
	case G_TYPE_LONG:
		g_value_set_long (g_value, strtoll (new_value, NULL, 10));
		break;
	case G_TYPE_ULONG:
		g_value_set_ulong (g_value, strtoull (new_value, NULL, 10));
		break;
	case G_TYPE_FLOAT:
		g_value_set_float (g_value, strtof (new_value, NULL));
		break;
	case G_TYPE_DOUBLE:
		g_value_set_double (g_value, strtod (new_value, NULL));
		break;
	case G_TYPE_BOOLEAN:
		if (elektraStrCaseCmp (new_value, "true") == 0)
		{
			g_value_set_boolean (g_value, TRUE);
		}
		else
		{
			g_value_set_boolean (g_value, FALSE);
		}
		break;
	default:
		g_warning ("Cannot convert to type %s(%lu)", G_VALUE_TYPE_NAME (g_value), g_value->g_type);
		return 1;
	}
	return 0;
}

static char * duplicateWithArrayNumber (const gchar * property, guint index);
static gboolean elektraArrayLength (KeySet * ks, XfconfChannel * channel, const gchar * property, guint * out);
static gboolean ks_get_formatted (KeySet * ks, XfconfChannel * channel, const gchar * property, GValue * g_value);
static GPtrArray * ks_get_arrayv (KeySet * ks, XfconfChannel * channel, const gchar * property)
{
	trace ();
	guint arrayLength = 0;
	if (!elektraArrayLength (ks, channel, property, &arrayLength))
	{
		g_debug ("%s was not a valid array", property);
		return NULL;
	}
	g_debug ("array length is %u", arrayLength);
	GPtrArray * array = g_ptr_array_new ();
	for (size_t i = 0; i < arrayLength; i++)
	{
		const char * elementName = duplicateWithArrayNumber (property, i);
		g_debug ("looking up %s", elementName);
		GValue * g_value = calloc (1, sizeof (GValue));
		if (ks_get_formatted (ks, channel, elementName, g_value))
		{
			g_ptr_array_add (array, g_value);
		}
		else
		{
			g_warning ("unable to read array element with index %lu and name %s", i, elementName);
		}
	}
	return array;
}

gboolean ks_get_formatted (KeySet * ks, XfconfChannel * channel, const gchar * property, GValue * g_value)
{
	trace ();
	gchar * propertyName = malloc ((strlen (XFCONF_ROOT) + strlen (channel->channel_name) + strlen (property) + 2) * sizeof (char));
	sprintf (propertyName, "%s/%s%s", XFCONF_ROOT, channel->channel_name, property);
	g_debug ("request key %s with type %s, on channel: %s which has %zd keys", property, G_VALUE_TYPE_NAME (g_value),
		 channel->channel_name, ksGetSize (ks));
	Key * key = ksLookupByName (ks, propertyName, KDB_O_NONE);
	if (key == NULL)
	{
		g_debug ("got null from keyset by looking up %s", propertyName);
		g_debug ("RESULT: %s does not exist", property);
		return FALSE;
	}
	const char * keyValue = keyString (key);
	g_debug ("Found value %s to key %s", keyValue, propertyName);
	GType g_type = G_TYPE_STRING;
	const char * gtypeName = "";
	if (!G_IS_VALUE (g_value))
	{
		g_debug ("read gtype from key database");
		const Key * gtypeMetaKey = keyGetMeta (key, XFCONF_GTYPE_META_NAME);
		if (gtypeMetaKey)
		{
			gtypeName = keyString (gtypeMetaKey);
			g_debug ("set gtype to %s", gtypeName);
			g_type = g_type_from_name (gtypeName);
		}
		else
		{
			g_debug ("key has no gtype meta - assuming string");
		}
		g_value_init (g_value, g_type ? g_type : G_TYPE_STRING); // fallback to string when the g_type has an invalid id
	}
	if (strcmp (gtypeName, g_type_name (G_TYPE_PTR_ARRAY)) == 0)
	{ // for whatever reason, glib is unable to return the id to G_TYPE_PTR_ARRAY, thus we have to compare the names
		g_debug ("found array gtype");
		g_value_unset (g_value);
		g_value_init (g_value, G_TYPE_PTR_ARRAY);
		g_value_take_boxed (g_value, ks_get_arrayv (ks, channel, property));
	}
	else
	{
		if (G_VALUE_TYPE (g_value) == G_TYPE_STRING)
		{
			g_debug ("since a string is requested, no transformation is required");
			g_value_set_string (g_value, strdup (keyValue));
		}
		else
		{
			g_debug ("perform transformation of the g_type");
			//			GValue g_key_value = G_VALUE_INIT;
			//			g_value_init (&g_key_value, G_TYPE_STRING);
			//			g_value_set_string (&g_key_value, keyValue);
			//			g_value_transform (&g_key_value, g_value);
			g_value_convert_string (g_value, keyValue);
		}
	}
	free (propertyName);
	g_debug ("RESULT: %s is %s", property, g_value_to_string (g_value));
	return TRUE;
}

gboolean xfconf_channel_get_formatted (XfconfChannel * channel, const gchar * property, GValue * g_value)
{
	trace ();
	KeySet * keySet = keySet_from_channel (channel->channel_name, 1, 0);
	require_channel_read_lock ();
	gboolean result = ks_get_formatted (keySet, channel, property, g_value);
	release_channel_lock ();
	return result;
}

/**
 * appendKeyToChannel - Appends a Key to an XfconfChannel and stores the resulting KeySet in the Key Database.
 *
 * This function takes a pointer to an XfconfChannel struct and a pointer to a Key struct and appends the Key to the
 * channel's KeySet. The resulting KeySet is then stored in the Key Database using the parent key constructed from the
 * XFCONF_ROOT and channel name. This function returns an integer value indicating the success or failure of the database
 * operation.
 *
 * @param channel Pointer to an XfconfChannel struct representing the channel to which the key should be appended.
 * @param key Pointer to a Key struct representing the key to be appended to the channel.
 *
 * @return An integer value representing the success or failure of the database operation. A value of 0 indicates success,
 * while a negative value indicates an error.
 */
static int appendKeyToChannel (const XfconfChannel * channel, Key * key)
{
	trace ();
	require_channel_write_lock ();
	KeySet * keySet = keySet_from_channel (channel->channel_name, 1, 1);
	g_debug ("the keyset contains %ld keysx before appending", ksGetSize (keySet));
	ksAppendKey (keySet, key);
	const char * parentKeyName = channelNameToKeyName (channel->channel_name);
	Key * parentKey = keyNew (parentKeyName, KEY_END);
	keySet = keySet_from_channel (channel->channel_name, 1, 1);
	int resultCode = kdbSet (elektraKdb, keySet, parentKey);
	g_debug ("the keyset contains %ld keysx after appending", ksGetSize (keySet));
	release_channel_lock ();
	g_debug ("storing key set for parent key %s returned %d", parentKeyName, resultCode);
	return resultCode;
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
	char * propertyName = propertyWithChannelPrefix (channel, property);
	Key * key = keyNew (propertyName, KEY_END);
	g_debug ("set %s to %s (type %lu) on channel: %s", propertyName, value, g_type, channel->channel_name);
	keySetString (key, value);
	keySetMeta (key, XFCONF_GTYPE_META_NAME, g_type_name (g_type));
	int resultCode = appendKeyToChannel (channel, key);
	notify_property_changed (channel, property);
	g_debug ("RESULT: set %s to %s", propertyName, value);
	return resultCode >= 0;
}

/**
 * duplicateWithArrayNumber - Creates a new string with an array index appended to the original string.
 *
 * This function takes a string pointer to a property name and an unsigned integer index, and returns a newly allocated
 * string with the index appended to the original property name. The appended index is written in a format compatible
 * with Elektra's array syntax.
 *
 * @param property Pointer to a string containing the name of the property to be duplicated.
 * @param index An unsigned integer representing the index of the array element.
 *
 * @return A dynamically allocated string containing the original property name with the array index appended. The caller
 * is responsible for freeing this memory using `free`.
 */
static char * duplicateWithArrayNumber (const gchar * property, guint index)
{
	char * propertyNameWithIndex = calloc (strlen (property) + ELEKTRA_MAX_ARRAY_SIZE + 2, sizeof (char));
	strcpy (propertyNameWithIndex, property);
	propertyNameWithIndex[strlen (propertyNameWithIndex)] = '/';
	elektraWriteArrayNumber (&propertyNameWithIndex[strlen (propertyNameWithIndex)], index);
	return propertyNameWithIndex;
}

/**
 * propertyWithChannelPrefix - Constructs a full property name for the specified channel and property.
 *
 * This function takes a pointer to an XfconfChannel struct and a string pointer to a property name and returns a
 * dynamically allocated string containing the full property name, including the XFCONF_NAMESPACE, XFCONF_ROOT, and
 * channel name.
 *
 * @param channel Pointer to an XfconfChannel struct representing the channel containing the property.
 * @param property Pointer to a string containing the name of the property.
 *
 * @return A dynamically allocated string containing the full property name including the XFCONF_NAMESPACE, XFCONF_ROOT,
 * and channel name. The caller is responsible for freeing this memory using `free`.
 */
static char * propertyWithChannelPrefix (const XfconfChannel * channel, const gchar * property)
{
	char * propertyName =
		malloc ((strlen (XFCONF_NAMESPACE) + strlen (XFCONF_ROOT) + strlen (channel->channel_name) + strlen (property) + 2) *
			sizeof (char));
	sprintf (propertyName, "%s%s/%s%s", XFCONF_NAMESPACE, XFCONF_ROOT, channel->channel_name, property);
	return propertyName;
}

gboolean xfconf_channel_has_property (XfconfChannel * channel, const gchar * property)
{
	trace ();
	KeySet * keySet = keySet_from_channel (channel->channel_name, 1, 0);
	char * propertyName = malloc ((strlen (XFCONF_ROOT) + strlen (channel->channel_name) + strlen (property) + 2) * sizeof (char));
	sprintf (propertyName, "%s/%s%s", XFCONF_ROOT, channel->channel_name, property);
	require_channel_read_lock ();
	g_debug ("request key %s on channel: %s which has %zd keys", property, channel->channel_name, ksGetSize (keySet));
	const Key * key = ksLookupByName (keySet, propertyName, KDB_O_NONE);
	g_debug ("channel %s has key %s: %d", channel->channel_name, propertyName, key != NULL);
	g_debug ("RESULT: %s exists%s", property, key ? "" : " NOT");
	gboolean exists = key != NULL;
	release_channel_lock ();
	return exists;
}

gboolean xfconf_channel_is_property_locked (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}

ELEKTRA_UNUSED void xfconf_channel_reset_property (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property_base ELEKTRA_UNUSED,
						   gboolean recursive ELEKTRA_UNUSED)
{
	unimplemented ();
}

static guint g_value_hash (gconstpointer g_value)
{
	if (g_value == NULL)
	{
		return 0;
	}
	return g_str_hash (g_value_to_string (((GValue *) g_value)));
}

static gboolean g_value_equal (gconstpointer g0, gconstpointer g1)
{
	if (g0 == g1)
	{
		return TRUE;
	}
	if (g0 == NULL || g1 == NULL)
	{
		return FALSE;
	}
	GValue * g_val0 = (GValue *) g0;
	GValue * g_val1 = (GValue *) g1;
	if (g_val0->g_type != g_val1->g_type)
	{
		return FALSE;
	}
	return strcmp (g_value_to_string (g_val0), g_value_to_string (g_val1)) == 0;
}

const gchar * findChannelStart (const gchar * keyName)
{
	const gchar * trimmed = keyName;
	if (strncmp (trimmed, XFCONF_NAMESPACE, strlen (XFCONF_NAMESPACE)) == 0)
	{
		trimmed = &trimmed[strlen (XFCONF_NAMESPACE)];
	}
	else
	{
		return trimmed;
	}
	if (strncmp (trimmed, XFCONF_ROOT, strlen (XFCONF_ROOT)) == 0)
	{
		trimmed = &trimmed[strlen (XFCONF_ROOT)];
	}
	return trimmed;
}

GHashTable * xfconf_channel_get_properties (XfconfChannel * channel, const gchar * property_base)
{
	trace ();
	g_debug ("Fetch properties of channel %s with base %s", channel->channel_name, property_base);
	GHashTable * properties = g_hash_table_new (g_value_hash, g_value_equal);
	KeySet * ks = keySet_from_channel (channel->channel_name, 1, 0);
	const Key * key;
	unsigned long propertyBaseLength = property_base == NULL ? 0 : strlen (property_base);
	require_channel_read_lock ();
	for (elektraCursor i = 0; i < ksGetSize (ks); i++)
	{
		key = ksAtCursor (ks, i);
		const gchar * channelStart = findChannelStart (keyName (key));
		g_debug ("trimmed the key until the channel start: %s", channelStart);
		const char * keyNameWithoutPrefix = &channelStart[strlen (channel->channel_name) + 1];
		if (strncmp (&channelStart[1], channel->channel_name, strlen (channel->channel_name)) == 0 &&
		    (property_base == NULL || strncmp (property_base, keyNameWithoutPrefix, propertyBaseLength) == 0))
		{
			g_debug ("key %s starts with property base %s", keyNameWithoutPrefix, property_base);
			GValue * g_value = calloc (1, sizeof (GValue));
			g_value_init (g_value, G_TYPE_STRING);
			ks_get_formatted (ks, channel, keyNameWithoutPrefix, g_value);
			g_hash_table_add (properties, g_value);
		}
		else
		{
			g_debug ("key %s does NOT start with property base %s", keyNameWithoutPrefix, property_base);
		}
	}
	release_channel_lock ();
	return properties;
}

/* basic types */

ELEKTRA_UNUSED gchar * xfconf_channel_get_string (XfconfChannel * channel, const gchar * property, const gchar * default_value)
{
	trace ();
	g_debug ("REQUEST: get %s with default value %s", property, default_value);
	GValue val = G_VALUE_INIT;
	g_value_init (&val, G_TYPE_STRING);
	if (xfconf_channel_get_formatted (channel, property, &val))
	{
		return strdup (g_value_get_string (&val));
	}
	else
	{
		if (default_value == NULL)
		{
			return NULL;
		}
		return strdup (default_value);
	}
}
gboolean xfconf_channel_set_string (XfconfChannel * channel, const gchar * property, const gchar * value)
{
	trace ();
	return xfconf_channel_set_formatted (channel, property, value, G_TYPE_STRING);
}

ELEKTRA_UNUSED gint32 xfconf_channel_get_int (XfconfChannel * channel, const gchar * property, gint32 default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_INT, g_value_get_int, xfconf_channel_set_int)
}
gboolean xfconf_channel_set_int (XfconfChannel * channel, const gchar * property, gint32 value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%d", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_INT);
}


static gboolean xfconf_channel_set_int64 (XfconfChannel * channel, const gchar * property, gint64 value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%ld", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_INT64);
}

ELEKTRA_UNUSED static gint64 xfconf_channel_get_int64 (XfconfChannel * channel, const gchar * property, gint64 default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_INT64, g_value_get_int64, xfconf_channel_set_int64)
}

ELEKTRA_UNUSED guint32 xfconf_channel_get_uint (XfconfChannel * channel, const gchar * property, guint32 default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_UINT, g_value_get_uint, xfconf_channel_set_uint)
}
gboolean xfconf_channel_set_uint (XfconfChannel * channel, const gchar * property, guint32 value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%u", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_UINT);
}

ELEKTRA_UNUSED guint64 xfconf_channel_get_uint64 (XfconfChannel * channel, const gchar * property, guint64 default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_UINT64, g_value_get_uint64, xfconf_channel_set_uint64)
}
gboolean xfconf_channel_set_uint64 (XfconfChannel * channel, const gchar * property, guint64 value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%lu", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_UINT64);
}

static gboolean xfconf_channel_set_long (XfconfChannel * channel, const gchar * property, glong value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%ld", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_LONG);
}

static ELEKTRA_UNUSED glong xfconf_channel_get_long (XfconfChannel * channel, const gchar * property, glong default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_LONG, g_value_get_long, xfconf_channel_set_long)
}

static gboolean xfconf_channel_set_ulong (XfconfChannel * channel, const gchar * property, gulong value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%lu", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_ULONG);
}

static ELEKTRA_UNUSED gulong xfconf_channel_get_ulong (XfconfChannel * channel, const gchar * property, gulong default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_ULONG, g_value_get_ulong, xfconf_channel_set_ulong)
}

static gboolean xfconf_channel_set_float (XfconfChannel * channel, const gchar * property, gfloat value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%f", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_FLOAT);
}

static ELEKTRA_UNUSED gfloat xfconf_channel_get_float (XfconfChannel * channel, const gchar * property, gfloat default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_FLOAT, g_value_get_float, xfconf_channel_set_float)
}

gdouble ELEKTRA_UNUSED xfconf_channel_get_double (XfconfChannel * channel, const gchar * property, gdouble default_value)
{
	trace ();
	XFCONF_GET_TYPED (G_TYPE_DOUBLE, g_value_get_double, xfconf_channel_set_double)
}

gboolean xfconf_channel_set_double (XfconfChannel * channel, const gchar * property, gdouble value)
{
	trace ();
	char stringValue[XFCONF_NUM_BUF_SIZE] = { 0 };
	snprintf (stringValue, XFCONF_NUM_BUF_SIZE, "%f", value);
	return xfconf_channel_set_formatted (channel, property, stringValue, G_TYPE_DOUBLE);
}

gboolean ELEKTRA_UNUSED xfconf_channel_get_bool (XfconfChannel * channel, const gchar * property, gboolean default_value)
{
	trace ();
	if (!xfconf_channel_has_property (channel, property))
	{
		if (XFCONF_PERSIST_DEFAULT)
		{
			xfconf_channel_set_bool (channel, property, default_value);
		}
		return default_value;
	}
	GValue g_value = G_VALUE_INIT;
	g_value_init (&g_value, G_TYPE_BOOLEAN);
	xfconf_channel_get_formatted (channel, property, &g_value);
	return g_value_get_boolean (&g_value);
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
ELEKTRA_UNUSED gchar ** xfconf_channel_get_string_list (XfconfChannel * channel, const gchar * property)
{
	trace ();
	GPtrArray * array = xfconf_channel_get_arrayv (channel, property);
	if (!array)
	{
		g_debug ("found no array named %s in channel %s", property, channel->channel_name);
		return NULL;
	}
	guint length = array->len;
	gchar ** stringArray = calloc (length + 1, sizeof (gchar *));
	stringArray[length] = NULL;
	for (guint i = 0; i < length; i++)
	{
		GValue * g_value = g_ptr_array_steal_index (array, 0);
		stringArray[i] = strdup (g_value_to_string (g_value));
	}
	return stringArray;
}
ELEKTRA_UNUSED gboolean xfconf_channel_set_string_list (XfconfChannel * channel, const gchar * property, const gchar * const * values)
{
	trace ();
	const gchar * currentElement;
	GPtrArray * array = g_ptr_array_new ();
	for (int i = 0; (currentElement = values[i]); i++)
	{
		GValue * g_key_value = calloc (1, sizeof (GValue));
		g_value_init (g_key_value, G_TYPE_STRING);
		g_value_set_string (g_key_value, strdup (currentElement));
		g_ptr_array_add (array, g_key_value);
	}
	return xfconf_channel_set_arrayv (channel, property, array);
}

/* really generic API - can set some value types that aren't
 * supported by the basic type API, e.g., char, signed short,
 * unsigned int, etc.  no, you can't set arbitrary GTypes. */
gboolean xfconf_channel_get_property (XfconfChannel * channel, const gchar * property, GValue * value)
{
	trace ();
	return xfconf_channel_get_formatted (channel, property, value);
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
	if (G_VALUE_TYPE (value) == G_TYPE_PTR_ARRAY)
	{
		return xfconf_channel_set_arrayv (channel, property, value->data->v_pointer);
	}
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

ELEKTRA_UNUSED gboolean xfconf_channel_get_array (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
						  GType first_value_type ELEKTRA_UNUSED, ...)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_get_array_valist (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
							 GType first_value_type ELEKTRA_UNUSED, va_list var_args ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}

/**
 * elektraArrayLength - Calculate the length of an array stored in elektra.
 *
 * This function calculates the length of the array which is stored at the given property in the given channel.
 *
 *
 * @param channel Pointer to the Xfconf channel where the property is stored at.
 * @param property An string array which contains the name of the property in the Xfconf format.
 * @param out The pointer where the length should be stored at. The caller is responsible that it is a valid address.
 *
 * @return `TRUE` if the provided property is an array and `FALSE` otherwise
 */
static gboolean elektraArrayLength (KeySet * ks, XfconfChannel * channel, const gchar * property, guint * out)
{
	if (!ks)
	{
		g_debug ("no keyset to channel %s was found", channel->channel_name);
		return FALSE;
	}
	const char * propertyPath = propertyWithChannelPrefix (channel, property);
	const Key * arrayKey = ksLookupByName (ks, propertyPath, KDB_O_NONE);
	if (!arrayKey)
	{
		g_debug ("no array key found");
		return FALSE;
	}
	const Key * arrayMetaKey = keyGetMeta (arrayKey, "array");
	if (!arrayMetaKey)
	{
		g_debug ("no array meta key found");
		return FALSE;
	}
	const char * lastArrayNumber = keyString (arrayMetaKey);
	size_t prefixOffset = 0;
	g_debug ("last array number string is %s", lastArrayNumber);
	// search for the first number, assuming the strings length is at least 1 including null-termination
	while (lastArrayNumber[prefixOffset] != '\0' && (lastArrayNumber[prefixOffset] < '0' || lastArrayNumber[prefixOffset] > '9'))
	{
		g_debug ("last offset character is %c", lastArrayNumber[prefixOffset]);
		prefixOffset++;
	}
	g_debug ("try to parse array number: %s", &lastArrayNumber[prefixOffset]);
	char * invalidPointer = NULL;
	*out = strtoul (&lastArrayNumber[prefixOffset], &invalidPointer, 10) + 1;
	if (*invalidPointer != '\0')
	{
		g_warning ("there are invalid characters in the array number");
		*out = 0;
	}
	return TRUE;
}

GPtrArray * xfconf_channel_get_arrayv (XfconfChannel * channel, const gchar * property)
{
	trace ();
	KeySet * ks = keySet_from_channel (channel->channel_name, 1, 0);
	require_channel_read_lock ();
	GPtrArray * resultPtr = ks_get_arrayv (ks, channel, property);
	release_channel_lock ();
	return resultPtr;
}

ELEKTRA_UNUSED gboolean xfconf_channel_set_array (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
						  GType first_value_type ELEKTRA_UNUSED, ...)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_set_array_valist (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
							 GType first_value_type ELEKTRA_UNUSED, va_list var_args ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}
gboolean xfconf_channel_set_arrayv (XfconfChannel * channel, const gchar * property, GPtrArray * values)
{
	trace ();
	if (values->len <= 0)
	{
		return TRUE;
	}
	GValue * currentValue;
	gboolean result = TRUE;
	guint length = values->len;
	for (guint i = 0; i < length; i++)
	{
		currentValue = g_ptr_array_steal_index (values, 0);
		char * propertyNameWithIndex = duplicateWithArrayNumber (property, i);
		result &= xfconf_channel_set_property (channel, propertyNameWithIndex, currentValue);
	}
	const char * arrayKeyName = propertyWithChannelPrefix (channel, property);
	Key * arrayKey = keyNew (arrayKeyName, KEY_END);
	char * lastElementIndex = calloc (ELEKTRA_MAX_ARRAY_SIZE + 1, sizeof (char));
	elektraWriteArrayNumber (lastElementIndex, length - 1);
	keySetMeta (arrayKey, "array", lastElementIndex);
	keySetMeta (arrayKey, XFCONF_GTYPE_META_NAME, g_type_name (G_TYPE_PTR_ARRAY));
	g_debug ("appending array meta key %s -> %s", keyName (arrayKey), lastElementIndex);
	appendKeyToChannel (channel, arrayKey);
	return result;
}

/* struct types */

ELEKTRA_UNUSED gboolean xfconf_channel_get_named_struct (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
							 const gchar * struct_name ELEKTRA_UNUSED, gpointer value_struct ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_set_named_struct (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
							 const gchar * struct_name ELEKTRA_UNUSED, gpointer value_struct ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}

ELEKTRA_UNUSED gboolean xfconf_channel_get_struct (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
						   gpointer value_struct ELEKTRA_UNUSED, GType first_member_type ELEKTRA_UNUSED, ...)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_get_struct_valist (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
							  gpointer value_struct ELEKTRA_UNUSED, GType first_member_type ELEKTRA_UNUSED,
							  va_list var_args ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_get_structv (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
						    gpointer value_struct ELEKTRA_UNUSED, guint n_members ELEKTRA_UNUSED,
						    GType * member_types ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}

ELEKTRA_UNUSED gboolean xfconf_channel_set_struct (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
						   const gpointer value_struct ELEKTRA_UNUSED, GType first_member_type ELEKTRA_UNUSED, ...)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_set_struct_valist (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
							  const gpointer value_struct ELEKTRA_UNUSED,
							  GType first_member_type ELEKTRA_UNUSED, va_list var_args ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}
ELEKTRA_UNUSED gboolean xfconf_channel_set_structv (XfconfChannel * channel ELEKTRA_UNUSED, const gchar * property ELEKTRA_UNUSED,
						    const gpointer value_struct ELEKTRA_UNUSED, guint n_members ELEKTRA_UNUSED,
						    GType * member_types ELEKTRA_UNUSED)
{
	unimplemented ();
	return FALSE;
}
