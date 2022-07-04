#include "elektra-xfconf-channel.h"
#include "elektra-xfconf-util.h"

GType xfconf_channel_get_type (void)
{
	unimplemented ();
}

XfconfChannel * xfconf_channel_get (const gchar * channel_name)
{
	unimplemented ();
}

XfconfChannel * xfconf_channel_new (const gchar * channel_name)
{
	unimplemented ();
}

XfconfChannel * xfconf_channel_new_with_property_base (const gchar * channel_name, const gchar * property_base)
{
	unimplemented ();
}

gboolean xfconf_channel_has_property (XfconfChannel * channel, const gchar * property)
{
	unimplemented ();
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

/* basic types */

gchar * xfconf_channel_get_string (XfconfChannel * channel, const gchar * property, const gchar * default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_string (XfconfChannel * channel, const gchar * property, const gchar * value)
{
	unimplemented ();
}

gint32 xfconf_channel_get_int (XfconfChannel * channel, const gchar * property, gint32 default_value)
{
	unimplemented ();
}
gboolean xfconf_channel_set_int (XfconfChannel * channel, const gchar * property, gint32 value)
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
	unimplemented ();
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
	unimplemented ();
}
gboolean xfconf_channel_set_property (XfconfChannel * channel, const gchar * property, const GValue * value)
{
	unimplemented ();
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
