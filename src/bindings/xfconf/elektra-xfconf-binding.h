#ifndef ELEKTRA_XFCONF_BINDING_H
#define ELEKTRA_XFCONF_BINDING_H

#include <xfconf/xfconf.h>

void notify_property_changed (XfconfChannel * channel, const gchar * property_name);

#endif
