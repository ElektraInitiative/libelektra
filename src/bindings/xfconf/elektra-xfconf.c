#include "elektra-xfconf.h"
#include "elektra-xfconf-util.h"

GList * channel_list = NULL;
KDB * elektraKdb = NULL;

gboolean xfconf_init (GError ** error)
{
	trace ();
	Key * elektraError = keyNew ("/elektra_error", KEY_END);
	elektraKdb = kdbOpen (NULL, elektraError);
	if (elektraKdb == NULL)
	{
		g_debug ("unable to open elektraKdb");
		*error = g_error_new (0, 1, "unable to open kdb: %s", keyString (elektraError));
		return FALSE;
	}
	return TRUE;
}
void xfconf_shutdown (void)
{
	trace ();
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
