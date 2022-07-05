#include "elektra-xfconf.h"
#include "elektra-xfconf-util.h"

#include <glib.h>

GList * channel_list = NULL;

gboolean xfconf_init (GError ** error)
{
	unimplemented ();
}
void xfconf_shutdown (void)
{
	g_list_free (channel_list);
}

void xfconf_named_struct_register (const gchar * struct_name, guint n_members, const GType * member_types)
{
	unimplemented ();
}

void xfconf_array_free (GPtrArray * arr)
{
	unimplemented ();
}

gchar ** xfconf_list_channels (void)
{
	unimplemented ();
}
