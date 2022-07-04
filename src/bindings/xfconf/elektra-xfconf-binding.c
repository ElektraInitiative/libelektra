#include "elektra-xfconf-binding.h"
#include "elektra-xfconf-util.h"

gulong xfconf_g_property_bind (XfconfChannel * channel, const gchar * xfconf_property, GType xfconf_property_type, gpointer object,
			       const gchar * object_property)
{
	unimplemented ();
}

gulong xfconf_g_property_bind_gdkcolor (XfconfChannel * channel, const gchar * xfconf_property, gpointer object,
					const gchar * object_property)
{
	unimplemented ();
}

gulong xfconf_g_property_bind_gdkrgba (XfconfChannel * channel, const gchar * xfconf_property, gpointer object,
				       const gchar * object_property)
{
	unimplemented ();
}

void xfconf_g_property_unbind (gulong id)
{
	unimplemented ();
}

void xfconf_g_property_unbind_by_property (XfconfChannel * channel, const gchar * xfconf_property, gpointer object,
					   const gchar * object_property)
{
	unimplemented ();
}

void xfconf_g_property_unbind_all (gpointer channel_or_object)
{
	unimplemented ();
}
