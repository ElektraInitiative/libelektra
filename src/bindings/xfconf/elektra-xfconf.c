#include "elektra-xfconf.h"
#include "elektra-xfconf-util.h"

#include <gelektra-key.h>

GList * channel_list = NULL;
GElektraKdb * gElektraKdb = NULL;

gboolean xfconf_init (GError ** error)
{
	GElektraKeySet * empty_set = gelektra_keyset_new (0);
	GElektraKey * elektra_error = gelektra_key_new ("/elektra_error", KEY_END);
	gElektraKdb = gelektra_kdb_open (empty_set, elektra_error);
	if (gElektraKdb == NULL)
	{
		*error = g_error_new (0, 1, "unable to open kdb: %s", gelektra_key_string (elektra_error));
		return FALSE;
	}
	return TRUE;
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
