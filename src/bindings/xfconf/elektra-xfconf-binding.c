#include "elektra-xfconf-binding.h"
#include "elektra-xfconf-channel.h"
#include "elektra-xfconf-util.h"
#include "elektra-xfconf.h"

#define require_binding_read_lock() require_read_lock (&binding_lock, "BINDING")
#define require_binding_write_lock() require_read_lock (&binding_lock, "BINDING")
#define release_binding_lock() require_read_lock (&binding_lock, "BINDING")

typedef struct
{
	gulong id;
	XfconfChannel * channel;
	const gchar * xfconf_property;
	gpointer object;
	const gchar * object_property;
} propertyBinding;

GList * property_bindings = NULL;
gulong last_id = 0;

pthread_rwlock_t binding_lock = PTHREAD_RWLOCK_INITIALIZER;

static void notify_binding (const propertyBinding * binding, const GValue * gValue)
{
	trace ();
	g_object_set_property (G_OBJECT (binding->object), binding->object_property, gValue);
}

gulong xfconf_g_property_bind (XfconfChannel * channel, const gchar * xfconf_property, GType xfconf_property_type, gpointer object,
			       const gchar * object_property)
{
	trace ();
	g_debug ("try to bind property %sto %s", xfconf_property, object_property);

	propertyBinding * binding = calloc (1, sizeof (propertyBinding));
	binding->channel = channel;
	binding->xfconf_property = strdup (xfconf_property);
	binding->object = object;
	binding->object_property = strdup (object_property);

	require_binding_write_lock () binding->id = last_id++;
	property_bindings = g_list_append (property_bindings, binding);
	release_binding_lock () if (xfconf_channel_has_property (channel, xfconf_property))
	{
		GValue * gValue = calloc (1, sizeof (GValue));
		xfconf_channel_get_formatted (channel, xfconf_property, gValue);
		notify_binding (binding, gValue);
	}
	return binding->id;
}

gulong xfconf_g_property_bind_gdkcolor (XfconfChannel * channel, const gchar * xfconf_property, gpointer object,
					const gchar * object_property)
{
	trace ();
	return xfconf_g_property_bind (channel, xfconf_property, G_TYPE_STRING, object, object_property);
}

gulong xfconf_g_property_bind_gdkrgba (XfconfChannel * channel, const gchar * xfconf_property, gpointer object,
				       const gchar * object_property)
{
	trace ();
	return xfconf_g_property_bind (channel, xfconf_property, G_TYPE_STRING, object, object_property);
}

static gint find_by_id (gconstpointer a, gconstpointer b)
{
	const gulong id = (*(gulong *) a);
	const propertyBinding * binding = (propertyBinding *) b;
	return id == binding->id;
}

void xfconf_g_property_unbind (gulong id)
{
	trace ();
	require_binding_write_lock () GList * item = g_list_find_custom (property_bindings, &id, &find_by_id);
	if (item == NULL)
	{
		g_info ("no binding with such id: %ld", id);
	}
	else
	{
		property_bindings = g_list_remove_link (property_bindings, item);
		g_list_free_full (item, &free);
	}
	release_binding_lock ()
}

void xfconf_g_property_unbind_by_property (XfconfChannel * channel, const gchar * xfconf_property, gpointer object,
					   const gchar * object_property)
{
	trace ();
	require_binding_write_lock () GList * cur = property_bindings;
	GList * nextItem;
	propertyBinding * current_binding;
	while (cur != NULL)
	{
		current_binding = cur->data;
		nextItem = cur->next;
		if (current_binding->channel == channel && current_binding->object == object &&
		    strcmp (current_binding->xfconf_property, xfconf_property) == 0 &&
		    strcmp (current_binding->object_property, object_property) == 0)
		{
			property_bindings = g_list_remove_link (property_bindings, cur);
			g_list_free_full (cur, &free);
		}
		cur = nextItem;
	}
	release_binding_lock ()
}

void xfconf_g_property_unbind_all (gpointer channel_or_object)
{
	trace ();
	require_binding_write_lock () GList * cur = property_bindings;
	GList * nextItem;
	propertyBinding * current_binding;
	while (cur != NULL)
	{
		current_binding = cur->data;
		nextItem = cur->next;
		if (current_binding->channel == channel_or_object || current_binding->object == channel_or_object)
		{
			property_bindings = g_list_remove_link (property_bindings, cur);
			g_list_free_full (cur, &free);
		}
		cur = nextItem;
	}
	release_binding_lock ()
}

void notify_property_changed (XfconfChannel * channel, const gchar * property_name)
{
	trace ();
	GValue * gValue = calloc (1, sizeof (GValue));
	xfconf_channel_get_formatted (channel, property_name, gValue);
	require_binding_read_lock () GList * cur = property_bindings;
	propertyBinding * binding;
	while (cur != NULL)
	{
		binding = cur->data;
		if (strcmp (binding->channel->channel_name, channel->channel_name) == 0 &&
		    strcmp (binding->xfconf_property, property_name) == 0)
		{
			notify_binding (binding, gValue);
		}
		cur = cur->next;
	}
	release_binding_lock ()
}
