#include "./elektra-xfconf-errors.h"
#include "./elektra-xfconf-util.h"

#include <internal/macros/attributes.h>

ELEKTRA_UNUSED GType xfconf_error_get_type (void)
{
	unimplemented ();
	return G_TYPE_NONE;
}
ELEKTRA_UNUSED GQuark xfconf_get_error_quark (void)
{
	unimplemented ();
	return g_quark_from_static_string ("0");
}
