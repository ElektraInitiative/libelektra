#include "elektra-xfconf-types.h"
#include "elektra-xfconf-util.h"

#include <elektra.h>

ELEKTRA_UNUSED GType xfconf_uint16_get_type (void)
{
	return G_TYPE_UINT;
}

ELEKTRA_UNUSED guint16 xfconf_g_value_get_uint16 (const GValue * value ELEKTRA_UNUSED)
{
	unimplemented ();
	return 0;
}
ELEKTRA_UNUSED void xfconf_g_value_set_uint16 (GValue * value ELEKTRA_UNUSED, guint16 v_uint16 ELEKTRA_UNUSED)
{
	unimplemented ();
}

ELEKTRA_UNUSED GType xfconf_int16_get_type (void)
{
	return G_TYPE_INT;
}

ELEKTRA_UNUSED gint16 xfconf_g_value_get_int16 (const GValue * value ELEKTRA_UNUSED)
{
	unimplemented ();
	return 0;
}
ELEKTRA_UNUSED void xfconf_g_value_set_int16 (GValue * value ELEKTRA_UNUSED, gint16 v_int16 ELEKTRA_UNUSED)
{
	unimplemented ();
}
